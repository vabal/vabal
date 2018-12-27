module VabalMain where

import ArgumentParsers
import GhcMetadata

import Distribution.Types.GenericPackageDescription
import Options.Applicative

import System.Directory
import System.FilePath

import VabalError

import VabalContext
import GhcupProgram

import CabalAnalyzer
import UserInterface

import XArgsEscape

import Control.Monad (unless)

import qualified Data.ByteString as B

data VabalMainArguments = VabalMainArguments
               { versionSpecification :: VersionSpecification
               , configFlags          :: FlagAssignment
               , cabalFile            :: Maybe FilePath
               , noInstallFlag        :: Bool
               , alwaysNewestFlag     :: Bool
               }
               deriving(Show)

mainArgumentsParser :: Parser VabalMainArguments
mainArgumentsParser = VabalMainArguments
                   <$> versionSpecificationOptions
                   <*> flagsOption
                   <*> cabalFileOption
                   <*> noInstallSwitch
                   <*> alwaysNewestSwitch

findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    childs <- listDirectory currDir
    let cabalFiles = filter (\c -> takeExtension c == ".cabal") childs
    case cabalFiles of
        [] -> throwVabalErrorIO "No cabal file found."
        (cf:_) -> return cf

mainProgDesc :: String
mainProgDesc = "Finds a version of GHC that is compatible with \
               \ the constraints imposed on base package found \
               \ in the cabal file analyzed, \
               \ then uses ghcup to obtain it (possibly downloading it). \
               \ Finally it prints to stdout options you can feed \
               \ to cabal to use the obtained GHC compiler \
               \ (options are already escaped so that they can sent to xargs). \
               \ WARNING: Probably this is not what you want to use, \
               \ See \"vabal configure --help\" for info about how to \
               \ directly configure your project to use the found GHC compiler."

vabalMain :: VabalMainArguments -> IO ()
vabalMain args = do
    ghcMetadataDir <- getGhcMetadataDir
    cabalFilePath <- maybe findCabalFile return (cabalFile args)
    cabalFileContents <- B.readFile cabalFilePath

    let ghcMetadataPath = ghcMetadataDir </> ghcMetadataFilename
    let flags = configFlags args

    ghcDb <- readGhcMetadata ghcMetadataPath
    installedGhcs <- subMap ghcDb <$> getInstalledGhcs

    let vabalCtx = VabalContext installedGhcs ghcDb(alwaysNewestFlag args)

    version <- case versionSpecification args of
                    GhcVersion ghcVer -> do
                        let res = checkIfGivenVersionWorksForAllTargets flags
                                                                        vabalCtx
                                                                        cabalFileContents
                                                                        ghcVer
                        unless res $
                            writeWarning "Warning: The specified ghc version probably won't work."
                        return ghcVer

                    BaseVersion baseVer -> return $
                                  analyzeCabalFileAllTargets flags
                                                             vabalCtx
                                                             (Just baseVer)
                                                             cabalFileContents

                    NoSpecification -> return $
                                  analyzeCabalFileAllTargets flags
                                                             vabalCtx
                                                             Nothing
                                                             cabalFileContents


    ghcLocation <- requireGHC installedGhcs version (noInstallFlag args)
    writeMessage $ "Selected GHC version: " ++ prettyPrintVersion version

    -- Output generation
    writeOutput $ generateCabalOptions args ghcLocation


generateCabalOptions :: VabalMainArguments -> Maybe FilePath -> String
generateCabalOptions args ghcLocation =
    let flagsOutput = unwords
              . map showFlagValue $ unFlagAssignment (configFlags args)

        outputGhcLocationArg = case ghcLocation of
                                 Nothing -> ""
                                 Just loc -> "-w\n" ++ escapeForXArgs loc
        outputFlagsArg = if null flagsOutput then
                            ""
                         else
                           -- we don't escape flags because we are sure no invalid
                           -- sequence is in them, since otherwise they weren't
                           -- parsed when passed as arguments
                           "\n--flags\n'" ++ flagsOutput ++ "'"

        outputCabalFile = case cabalFile args of
                             Nothing -> ""
                             Just cabalFilePath -> "\n--cabal-file\n" ++ escapeForXArgs cabalFilePath

    in dropWhile (== '\n') $ outputGhcLocationArg ++ outputFlagsArg ++ outputCabalFile -- Remove initial newlines


