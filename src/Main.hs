module Main where
    
import System.Directory
import System.FilePath
import System.Process
import System.Environment (getArgs)

import Control.Exception
import System.Exit

import VabalError
import CabalAnalyzer

import Data.List (find)
import Control.Monad (when)

import ProcessUtils
import FlagsUtils

import GhcupProgram

import UserInterface

import System.TimeIt

import Distribution.Types.GenericPackageDescription
import Distribution.Verbosity
import Distribution.PackageDescription.Parsec
import Distribution.Types.Version
import Distribution.Parsec.Class
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromString)

import qualified Data.ByteString as B

import Options.Applicative

withExceptionHandler :: Exception e => (e -> IO a) -> IO a -> IO a
withExceptionHandler = flip catch

versionParser :: ReadM Version
versionParser = maybeReader simpleParsec

flagAssignmentParser :: ReadM FlagAssignment
flagAssignmentParser = maybeReader $ \str ->
    either (const Nothing) Just $
        runParsecParser parsecFlagAssignment "<flagParsec>"
                        (fieldLineStreamFromString str)

data VersionSpecification = GhcVersion Version
                          | BaseVersion Version
                          | NoSpecification
                          deriving(Show)

data Arguments = Arguments
               { versionSpecification :: VersionSpecification
               , configFlags :: FlagAssignment
               , cabalFile   :: Maybe FilePath
               , noInstall   :: Bool
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

           <*> optional(strOption
               ( long "cabal-file"
               <> metavar "FILE"
               <> help "Explicitly tell which cabal file to use."
               ))
           <*> switch
               ( long "no-install"
               <> help "If GHC needs to be downloaded, fail, instead."
               )

main :: IO ()
main = do
    let opts = info (argsParser <**> helper)
             ( fullDesc
             <> header "vabal - The Cabal Companion"
             <> progDesc "Find out a version of the GHC compiler that satisfies \
                         \ the constraints imposed on base in the cabal project. \
                         \ then configure the cabal project \
                         \ to use this version of the compiler."
             )

    args <- execParser opts
    let errorHandler :: SomeException -> IO ()
        errorHandler ex = do
            writeError $ show ex
            exitWith (ExitFailure 1)

    catch (vabalConfigure args) errorHandler
    return ()

vabalConfigure :: Arguments -> IO ()
vabalConfigure args = do

    cabalFilePath <- maybe findCabalFile return (cabalFile args)

    cabalFile <- B.readFile cabalFilePath

    let flags = configFlags args

    version <- case versionSpecification args of
                    GhcVersion ghcVersion -> do
                        res <- return $ checkIfGivenVersionWorksForAllTargets flags cabalFile ghcVersion
                        when (not res) $
                            writeWarning "Warning: The specified ghc version probably won't work."
                        return ghcVersion

                    BaseVersion baseVersion -> return $ analyzeCabalFileAllTargets flags (Just baseVersion) cabalFile

                    NoSpecification -> return $ analyzeCabalFileAllTargets flags Nothing cabalFile


    putStrLn "Das version: "
    print version

    -- exitWith ExitSuccess

    path <- requireGHC version (noInstall args)

    runCabalConfigure flags path

    return ()


-- Run cabal new-configure with the given compiler version
-- If The filepath is Nothing, then use the ghc in path
runCabalConfigure :: FlagAssignment -> GhcLocation -> IO ()
runCabalConfigure flags ghcLoc = do
    writeMessage "Running cabal new-configure."

    let args = case ghcLoc of
            InPath                 -> ["new-configure"]
            CustomLocation ghcLoc  -> ["new-configure", "-w", ghcLoc ]

    let argsPlusFlags = args ++ makeCabalArguments flags
    res <- runExternalProcess "cabal" argsPlusFlags
    case res of
        ExitSuccess -> return ()
        ExitFailure _ -> throwVabalErrorIO "Error while running cabal configure."

findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    childs <- listDirectory currDir
    let cabalFiles = filter (\c -> takeExtension c == ".cabal") childs
    case cabalFiles of
        [] -> throwVabalErrorIO "No cabal file found."
        (cf:cfs) -> return cf

