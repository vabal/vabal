module Main where
    
import System.Directory
import System.FilePath

import Control.Exception
import System.Exit

import VabalError
import CabalAnalyzer

import Control.Monad (unless)

import GhcupProgram
import UserInterface

import Distribution.Types.GenericPackageDescription
import Distribution.Types.Version
import Distribution.Parsec.Class
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromString)

import qualified Data.ByteString as B

import Options.Applicative

versionParser :: ReadM Version
versionParser = maybeReader simpleParsec

flagAssignmentParser :: ReadM FlagAssignment
flagAssignmentParser = maybeReader $ \flagsString ->
    either (const Nothing) Just $
        runParsecParser parsecFlagAssignment "<flagParsec>"
                        (fieldLineStreamFromString flagsString)

data VersionSpecification = GhcVersion Version
                          | BaseVersion Version
                          | NoSpecification
                          deriving(Show)

data Arguments = Arguments
               { versionSpecification :: VersionSpecification
               , configFlags          :: FlagAssignment
               , cabalFile            :: Maybe FilePath
               , noInstallFlag        :: Bool
               , alwaysNewestFlag     :: Bool
               }
               deriving(Show)

ghcVersionArgument :: Parser VersionSpecification
ghcVersionArgument = pure GhcVersion
                  <*> option versionParser
                      ( long "with-ghc-version"
                      <> short 'g'
                      <> metavar "VER"
                      <> help "Explicitly tell which version of ghc you want to use \
                              \ for the project. \
                              \ (Incompatible with option --with-base-version)"
                      )

baseVersionArgument :: Parser VersionSpecification
baseVersionArgument = pure BaseVersion
                   <*> option versionParser
                       ( long "with-base-version"
                       <> short 'b'
                       <> metavar "VER"
                       <> help "Specify the version of base package you want to use. \
                                \ It is going to be checked against base constraints in \
                                \ the cabal file for validity. \
                                \ (Incompatible with option --with-ghc-version)"
                       )

versionSpecificationArguments :: Parser VersionSpecification
versionSpecificationArguments =
    ghcVersionArgument <|> baseVersionArgument <|> pure NoSpecification



argsParser :: Parser Arguments
argsParser = pure Arguments
           <*> versionSpecificationArguments
           <*> option flagAssignmentParser
               ( long "flags"
               <> metavar "FLAGS"
               <> help "String containing a list  of space separated flags \
                       \ to be used to configure the project \
                       \ (You can enable or disable a flag by adding a + or - \
                       \ in front of the flag name. \
                       \ When none is specified, the flag is enabled)."
               <> value (mkFlagAssignment [])
               )
           <*> optional
               ( strOption
                 ( long "cabal-file"
                 <> metavar "FILE"
                 <> help "Explicitly tell which cabal file to use."
                 )
               )
           <*> switch
               ( long "no-install"
               <> help "If GHC needs to be downloaded, fail, instead."
               )
           <*> switch
               ( long "always-newest"
               <> help "Always choose newest GHC possible, don't prefer \
                       \ already installed GHCs"
               )

main :: IO ()
main = do
    let opts = info (argsParser <**> helper)
             ( fullDesc
             <> header "vabal - The Cabal Companion"
             <> progDesc "Find out a version of the GHC compiler that satisfies \
                         \ the constraints imposed on base in the cabal project \
                         \ (By default already installed GHCs are preferred). \
                         \ Then print to stdout the path to a GHC compiler \
                         \ with that version (potentially downloading it)."
             )

    args <- execParser opts
    let errorHandler :: SomeException -> IO ()
        errorHandler ex = do
            writeError $ show ex
            exitWith (ExitFailure 1)

    catch (vabalMain args) errorHandler
    return ()

vabalMain :: Arguments -> IO ()
vabalMain args = do

    cabalFilePath <- maybe findCabalFile return (cabalFile args)

    cabalFileContents <- B.readFile cabalFilePath

    let flags = configFlags args

    availableGhcs <- getAvailableGhcs

    print availableGhcs

    version <- case versionSpecification args of
                    GhcVersion ghcVersion -> do
                        let res = checkIfGivenVersionWorksForAllTargets flags
                                                                        cabalFileContents
                                                                        ghcVersion
                        unless res $
                            writeWarning "Warning: The specified ghc version probably won't work."
                        return ghcVersion

                    BaseVersion baseVersion -> return $
                                  analyzeCabalFileAllTargets flags
                                                             (alwaysNewestFlag args)
                                                             availableGhcs
                                                             (Just baseVersion)
                                                             cabalFileContents

                    NoSpecification -> return $
                                  analyzeCabalFileAllTargets flags
                                                             (alwaysNewestFlag args)
                                                             availableGhcs
                                                             Nothing
                                                             cabalFileContents


    ghcLocation <- requireGHC availableGhcs version (noInstallFlag args)

    writeMessage $ "Selected GHC version: " ++ prettyPrintVersion version
    writeOutput ghcLocation


findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    childs <- listDirectory currDir
    let cabalFiles = filter (\c -> takeExtension c == ".cabal") childs
    case cabalFiles of
        [] -> throwVabalErrorIO "No cabal file found."
        (cf:_) -> return cf

