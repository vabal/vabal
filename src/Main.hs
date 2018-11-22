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

import CabalAnalyzer

import Data.List (find)

import Downloader
import BuildTypeRecognizer

runExternalProcess :: FilePath -> [String] -> IO ExitCode
runExternalProcess bin args = do
    let processDescr = (proc bin args)
    (_, _, _, procHandle) <- createProcess processDescr
    waitForProcess procHandle

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action =
    bracket (mkdtemp "/tmp/ghc-binaries")
            removeDirectoryRecursive
            action

main :: IO ()
main = do
    args <- getArgs
    let errorHandler :: SomeException -> IO ()
        errorHandler ex = putStrLn $ show ex

    catch (vabalConfigure args) errorHandler



-- Directory containing ghc installs
ghcDirectory :: IO FilePath
ghcDirectory = do
    home <- getHomeDirectory
    return $ home </> ".ghc_install_dir"

vabalConfigure :: [String] -> IO ()
vabalConfigure args = do
    buildType <- detectGhcBuildType
    ghcInstallDir <- ghcDirectory
    createDirectoryIfMissing True ghcInstallDir

    cabalFilePath <- findCabalFile

    version <- analyzeCabalFileDefaultTarget [] cabalFilePath

    let outputDir = ghcInstallDir </> ("ghc-" ++ version)

    ghcInPath <- return False -- checkIfNeededGhcIsInPath version

    ghcAlreadyInstalled <- doesDirectoryExist outputDir

    if ghcInPath then do
        putStrLn "Using GHC in path."
        runCabalConfigure Nothing -- use ghc in path
    else if ghcAlreadyInstalled then do
        putStrLn "Already installed."
        runCabalConfigure (Just outputDir)
    else do
        putStr $ "Do you want to download GHC " ++ version ++ "? [Y/n]:"
        hFlush stdout
        response <- getLine
        case response of
            "n" -> putStrLn "Download aborted."
            _   -> do
                putStrLn $ "Installing GHC in " ++ outputDir
                installGhcBinary buildType version outputDir
                putStrLn "Ghc installed."
                runCabalConfigure (Just outputDir)

    return ()


-- Run cabal new-configure with the given compiler version
-- If The filepath is Nothing, then use the ghc in path
runCabalConfigure :: Maybe FilePath -> IO ()
runCabalConfigure outputDir = do
    putStrLn "Running cabal new-configure."

    let args = case outputDir of
            Nothing        -> ["new-configure"]
            Just outputDir -> ["new-configure", "-w", outputDir </> "bin" </> "ghc"]
    res <- runExternalProcess "cabal" args
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

installGhcBinary :: String -> String -> FilePath -> IO ()
installGhcBinary buildType version outputDir = withTemporaryDirectory $ \tmpDir -> do
    let outputFilename = tmpDir </> "ghc.tar.xz"
    downloadGhcBinaries buildType version outputFilename

    withCurrentDirectory tmpDir $ do
        extractArchive outputFilename
        withCurrentDirectory ("ghc-" ++ version) $ do
            installBinaries outputDir


downloadGhcBinaries :: String -> String -> FilePath -> IO ()
downloadGhcBinaries buildType version outputFilename = do
    let baseUrl = "https://downloads.haskell.org/~ghc/" ++ version
    let buildName = "ghc-" ++ version ++ "-" ++ buildType ++ ".tar.xz"
    let downloadUrl = baseUrl ++ "/" ++ buildName

    putStrLn "Downloading GHC."
    runDownloader downloadUrl outputFilename


extractArchive :: FilePath -> IO ()
extractArchive archive = do
    putStrLn "Extracting GHC binaries archive."
    res <- runExternalProcess "tar" ["-xJf", archive]
    case res of
        ExitSuccess -> return ()
        ExitFailure _ -> throwVabalErrorIO "Error while extracting ghc binary archive."

installBinaries :: FilePath -> IO ()
installBinaries outputDir = do
    putStrLn "Installing binaries."
    res1 <- runExternalProcess "./configure" ["--silent", "--prefix=" ++ outputDir]
    case res1 of
        ExitFailure _ -> throwVabalErrorIO "Error while installing binaries"
        ExitSuccess -> do
            res2 <- runExternalProcess "make" ["install", "--silent"]
            case res2 of
                ExitSuccess -> return ()
                ExitFailure _ -> throwVabalErrorIO "Error while installing binaries"

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

