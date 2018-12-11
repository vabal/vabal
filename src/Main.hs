module Main where

import System.Directory
import System.FilePath
import System.Process
import System.IO (hGetLine, hFlush, stdout)
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

import Distribution.Types.GenericPackageDescription
import Distribution.Verbosity
import Distribution.PackageDescription.Parsec
import Distribution.Types.Version
import Distribution.Parsec.Class
import Distribution.Parsec.FieldLineStream (fieldLineStreamFromString)

import Options.Applicative

withExceptionHandler :: Exception e => (e -> IO a) -> IO a -> IO a
withExceptionHandler = flip catch

versionParser :: ReadM (Maybe Version)
versionParser = Just <$> maybeReader simpleParsec

flagAssignmentParser :: ReadM FlagAssignment
flagAssignmentParser = maybeReader $ \str ->
    either (const Nothing) Just $
        runParsecParser parsecFlagAssignment "<flagParsec>"
                        (fieldLineStreamFromString str)


data Arguments = Arguments
               { ghcVersion :: Maybe Version
               , configFlags :: FlagAssignment
               }
               deriving(Show)

argsParser :: Parser Arguments
argsParser = pure Arguments
           <*> option versionParser
               ( long "with-version"
               <> short 'w'
               <> metavar "VER"
               <> help "Explicitly tell which version of ghc you want to use \
                       \ for the project."
               <> value Nothing
               )
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
        errorHandler ex = putStrLn $ show ex
    catch (vabalConfigure args) errorHandler
    return ()

vabalConfigure :: Arguments -> IO ()
vabalConfigure args = do

    cabalFilePath <- findCabalFile

    let specifiedGhcVersion = ghcVersion args
    let flags = configFlags args

    version <- case specifiedGhcVersion of
                    Just ghcVersion -> do
                        res <- checkIfGivenVersionWorksForAllTargets flags cabalFilePath ghcVersion
                        when (not res) $ putStrLn "Warning: The specified ghc version probably won't work."
                        return ghcVersion

                    Nothing -> analyzeCabalFileAllTargets flags cabalFilePath


    path <- requireGHC version

    runCabalConfigure flags path

    return ()


-- Run cabal new-configure with the given compiler version
-- If The filepath is Nothing, then use the ghc in path
runCabalConfigure :: FlagAssignment -> GhcLocation -> IO ()
runCabalConfigure flags ghcLoc = do
    putStrLn "Running cabal new-configure."

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

