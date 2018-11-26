module Main where

import System.Info (os, arch)
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkdtemp)
import System.Process
import System.IO (hGetLine, hFlush, stdout)

import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as TLS
import qualified Network.HTTP.Types.Header as N

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString as B
import Control.Exception
import System.Exit

import VabalError
import GHCInstallation

import CabalAnalyzer

import Data.List (find)
import Control.Monad (when)

import Downloader
import BuildTypeRecognizer
import GHCBinaryIntegrityVerifier
import ProcessUtils
import FlagsUtils

import Distribution.Types.GenericPackageDescription

withExceptionHandler :: Exception e => (e -> IO a) -> IO a -> IO a
withExceptionHandler = flip catch

showHelp :: IO ()
showHelp = do
    putStrLn "vabal usage:"
    putStrLn "vabal <flags>"
    putStrLn "<flags>  is a list of space separated flags."
    putStrLn "         You can prefix a flag with -"
    putStrLn "         to disable it."
    putStrLn "         You can use + (or no prefix at all"
    putStrLn "         to enable the flag, instead."

main :: IO ()
main = do
    args <- getArgs
    let errorHandler :: SomeException -> IO ()
        errorHandler ex = putStrLn $ show ex

    
    if "--help" `elem` args then
        showHelp
    else
        catch (vabalConfigure args) errorHandler
    return ()

-- Directory containing ghc installs
ghcDirectory :: IO FilePath
ghcDirectory = do
    home <- getHomeDirectory
    return $ home </> ".vabal"

vabalConfigure :: [String] -> IO ()
vabalConfigure args = do
    buildType <- detectGhcBuildType
    ghcInstallDir <- ghcDirectory
    createDirectoryIfMissing True ghcInstallDir

    cabalFilePath <- findCabalFile

    let flags = makeFlagAssignment args

    version <- analyzeCabalFileDefaultTarget flags cabalFilePath

    let outputDir = ghcInstallDir </> ("ghc-" ++ version)

    ghcInPath <- checkIfNeededGhcIsInPath version

    ghcAlreadyInstalled <- doesDirectoryExist outputDir

    if ghcInPath then do
        putStrLn "Using GHC in path."
        runCabalConfigure flags Nothing -- use ghc in path
    else if ghcAlreadyInstalled then do
        putStrLn "Already installed."
        runCabalConfigure flags (Just outputDir)
    else do
        putStr $ "Do you want to download GHC " ++ version ++ "? [Y/n]:"
        hFlush stdout
        response <- getLine
        case response of
            "n" -> putStrLn "Download aborted."
            _   -> do

                let onInstallError :: SomeException -> IO ()
                    onInstallError ex = do
                        -- If ~/.vabal/ghc-version was already created,
                        -- but the compiler installation was unsuccessful,
                        -- remove the directory
                        dirCreated <- doesDirectoryExist outputDir
                        when dirCreated $ do
                            removeDirectoryRecursive outputDir
                        -- Rethrow to let the top level handler deal with this
                        throwIO ex

                -- withExceptionHandler = flip catch
                withExceptionHandler onInstallError $ do
                    putStrLn $ "Using GHC build type: " ++ buildType
                    putStrLn $ "Installing GHC in " ++ outputDir
                    installGhcBinary buildType version outputDir
                    putStrLn "Ghc installed."

                runCabalConfigure flags (Just outputDir)

    return ()


-- Run cabal new-configure with the given compiler version
-- If The filepath is Nothing, then use the ghc in path
runCabalConfigure :: FlagAssignment -> Maybe FilePath -> IO ()
runCabalConfigure flags outputDir = do
    putStrLn "Running cabal new-configure."

    let args = case outputDir of
            Nothing        -> ["new-configure"]
            Just outputDir -> ["new-configure", "-w", outputDir </> "bin" </> "ghc"]

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

checkIfNeededGhcIsInPath :: String -> IO Bool
checkIfNeededGhcIsInPath version = catch getGhcVersion noGhcFound
    where noGhcFound :: SomeException -> IO Bool
          noGhcFound _ = return False
          getGhcVersion = do
                let processDescr = (proc "ghc" ["--numeric-version"])
                                 { std_out = CreatePipe
                                 }

                (_, Just outHandle, _, procHandle) <- createProcess processDescr
                ghcVersion <- hGetLine outHandle
                return (version == ghcVersion)

