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
import qualified Data.ByteString.Lazy as BL

import Options.Applicative

import VabalContext
import GhcMetadataDownloader

import Prelude hiding (putStrLn)

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

data Command = Update
             | RunVabal Arguments

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

commandParser :: Parser Command
commandParser = subparser
    (command "update" (info (pure Update) (progDesc "Download updated ghc metadata."))
    <> command "run" (info (RunVabal <$> argsParser) (progDesc "Analyze cabal package."))
    )
main :: IO ()
main = do
    let opts = info (commandParser <**> helper)
             ( fullDesc
             <> header "vabal - The Cabal Companion"
             <> progDesc "Find out a version of the GHC compiler that satisfies \
                         \ the constraints imposed on base in the cabal project \
                         \ (By default already installed GHCs are preferred). \
                         \ Then print to stdout the path to a GHC compiler \
                         \ with that version (potentially downloading it)."
             )

    cmd <- execParser opts
    let errorHandler :: SomeException -> IO ()
        errorHandler ex = do
            writeError $ show ex
            writeError "WARNING: If you used vabal within a command substitution \
                       \ be careful because cabal may have used \
                       \ something else as PATH to ghc (e.g. the next argument)."
            exitWith (ExitFailure 1)

    handle errorHandler $ do
        case cmd of
            Update -> vabalUpdate
            RunVabal args -> vabalMain args
    return ()


getGhcMetadataDir :: IO FilePath
getGhcMetadataDir = do
    homeDir <- getHomeDirectory
    return (homeDir </> ".vabal")

ghcMetadataFilename :: String
ghcMetadataFilename = "ghcMetadata.csv"

vabalUpdate :: IO ()
vabalUpdate = do
    dir <- getGhcMetadataDir
    createDirectoryIfMissing True dir
    downloadGhcMetadata (dir </> ghcMetadataFilename)
    writeOutput "Vabal successfully updated."

vabalMain :: Arguments -> IO ()
vabalMain args = do
    ghcMetadataDir <- getGhcMetadataDir

    cabalFilePath <- maybe findCabalFile return (cabalFile args)

    cabalFileContents <- B.readFile cabalFilePath

    let ghcMetadataPath = ghcMetadataDir </> ghcMetadataFilename

    ghcDb <- readGhcMetadata ghcMetadataPath

    let flags = configFlags args

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
    writeOutput ghcLocation


findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    childs <- listDirectory currDir
    let cabalFiles = filter (\c -> takeExtension c == ".cabal") childs
    case cabalFiles of
        [] -> throwVabalErrorIO "No cabal file found."
        (cf:_) -> return cf

readGhcMetadata :: FilePath -> IO GhcToBaseMap
readGhcMetadata filepath = do
    fileExists <- doesFileExist filepath

    if not fileExists then
        throwVabalErrorIO "Ghc metadata not found, run `vabal update` to download it."
    else do
      contents <- BL.readFile filepath
      case readGhcToBaseMap contents of
        Left err -> throwVabalErrorIO $ "Error, could not parse ghc metadata:\n" ++ err
        Right res -> return res
