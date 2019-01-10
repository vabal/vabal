module VabalMain where

import ArgumentParsers
import MetadataManager
import VabalContext

import GhcDatabase

import Distribution.Types.GenericPackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Options.Applicative

import System.Directory
import System.FilePath

import VabalError

import GhcupProgram

import PackageSolver
import UserInterface

import XArgsEscape

import Distribution.Version

import Control.Monad (unless)
import Data.Maybe (fromMaybe)

import qualified Data.Set as S

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

makeVabalContext :: IO VabalContext
makeVabalContext = do
    ghcMetadataDir <- getGhcMetadataDir
    let ghcMetadataPath = ghcMetadataDir </> ghcMetadataFilename
    ghcDb <- readGhcDatabase ghcMetadataPath
    installedGhcs <- S.fromList <$> getInstalledGhcs

    return $ VabalContext installedGhcs ghcDb

runSolver :: VabalMainArguments
          -> FlagAssignment
          -> VabalContext
          -> GenericPackageDescription
          -> Version
runSolver args flags ctx pkgDescr =
        if S.null compatibleVersions then
            throwVabalError "Could not solve constraints."
        else if alwaysNewestFlag args then
                 newestCompatibleVersion
        else
            let availableCompatibleVersions = S.intersection
                                              (availableGhcs ctx)
                                              compatibleVersions

            in fromMaybe newestCompatibleVersion
             $ S.lookupMax availableCompatibleVersions

    where compatibleVersions = analyzePackage flags (ghcDatabase ctx) pkgDescr
          newestCompatibleVersion = S.findMax compatibleVersions

vabalFindGhcVersion :: VabalMainArguments -> VabalContext -> IO Version
vabalFindGhcVersion args vabalCtx = do
    cabalFilePath <- maybe findCabalFile return (cabalFile args)
    pkgDescr <- readGenericPackageDescription normal cabalFilePath

    let flags = configFlags args

    case versionSpecification args of
        GhcVersion ghcVer -> do
            let res = doesGhcVersionSupportPackage flags
                                                   (ghcDatabase vabalCtx)
                                                   pkgDescr
                                                   ghcVer
            unless res $
                writeWarning "Warning: The specified ghc version probably won't work."
            return ghcVer

        BaseVersion baseVer -> do
            -- Restrict the database to only those ghcs with the required baseVersion
            let db' = filterBaseVersionIn (ghcDatabase vabalCtx) (thisVersion baseVer)
            let ctx' = vabalCtx { ghcDatabase = db' }
            return $ runSolver args flags ctx' pkgDescr

        NoSpecification -> return $ runSolver args flags vabalCtx pkgDescr





vabalMain :: VabalMainArguments -> IO ()
vabalMain args = do
    vabalCtx <- makeVabalContext
    version <- vabalFindGhcVersion args vabalCtx
    ghcLocation <- requireGHC (availableGhcs vabalCtx) version (noInstallFlag args)
    writeMessage $ "Selected GHC version: " ++ prettyPrintVersion version

    -- Output generation
    writeOutput $ generateCabalOptions args ghcLocation


generateCabalOptions :: VabalMainArguments -> FilePath -> String
generateCabalOptions args ghcLocation =
    let flagsOutput = unwords
              . map showFlagValue $ unFlagAssignment (configFlags args)

        outputGhcLocationArg = "-w\n" ++ escapeForXArgs ghcLocation
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

    in outputGhcLocationArg ++ outputFlagsArg ++ outputCabalFile

