module Main where

import System.Info (os, arch)
import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.Posix.Temp (mkdtemp)
import System.Process

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


withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action =
    bracket (mkdtemp "/tmp/ghc-binaries")
            removeDirectoryRecursive
            action


runExternalProcess :: FilePath -> [String] -> IO ExitCode
runExternalProcess bin args = do
    let processDescr = (proc bin args)
    (_, _, _, procHandle) <- createProcess processDescr
    waitForProcess procHandle


data SupportedOS = Linux
                 | Darwin

data SupportedArch = X86_64
                   | I386


getSupportedOS :: Maybe SupportedOS
getSupportedOS = case os of
    "linux" -> Just Linux
    "darwin" -> Just Darwin
    _        -> Nothing

getSupportedArch :: Maybe SupportedArch
getSupportedArch = case arch of
    "x86_64" -> Just X86_64
    "i386"   -> Just I386
    "i686"   -> Just I386
    _        -> Nothing


-- Get the ghc build type to look for
getBuildType :: SupportedOS -> SupportedArch -> Maybe String
getBuildType Linux X86_64  = Just "x86_64-fedora27-linux"
getBuildType Linux I386    = Just "i386-deb8-linux"
getBuildType Darwin X86_64 = Just "x86_64-apple-darwin"
getBuildType _ _           = Nothing


detectGhcBuildType :: Maybe String
detectGhcBuildType = do
    os <- getSupportedOS
    arch <- getSupportedArch
    getBuildType os arch

errorHandler :: SomeException -> IO ()
errorHandler ex = putStrLn $ show ex

main :: IO ()
main = do
    args <- getArgs
    case detectGhcBuildType of
        Nothing -> putStrLn "Unsupported platform."
        Just buildType -> catch (vabalConfigure buildType args) errorHandler


-- TODO: Make these configurable

-- Directory containing ghc installs
ghcDirectory :: IO FilePath
ghcDirectory = do
    home <- getHomeDirectory
    return $ home </> ".ghc_install_dir"

vabalConfigure :: String -> [String] -> IO ()
vabalConfigure buildType args = do
    ghcInstallDir <- ghcDirectory
    createDirectoryIfMissing True ghcInstallDir

    cabalFilePath <- findCabalFile

    version <- analyzeCabalFileDefaultTarget cabalFilePath

    let outputDir = ghcInstallDir </> ("ghc-" ++ version)

    ghcAlreadyInstalled <- doesDirectoryExist outputDir

    if ghcAlreadyInstalled then do
        putStrLn "Already installed."
        runCabalConfigure outputDir
    else do
        putStrLn $ "Do you want to download GHC " ++ version ++ "? [Y/n]:"
        response <- getLine
        case response of
            "n" -> putStrLn "Download aborted."
            _   -> do
                installGhcBinary buildType version outputDir
                putStrLn "Ghc installed."
                runCabalConfigure outputDir

    return ()


-- Run cabal new-configure with the given compiler version
runCabalConfigure :: FilePath -> IO ()
runCabalConfigure outputDir = do
    putStrLn "Running cabal new-configure."
    res <- runExternalProcess "cabal" ["new-configure", "-w", outputDir </> "bin" </> "ghc"]
    case res of
        ExitSuccess -> return ()
        ExitFailure _ -> throwVabalError "Error while running cabal configure."

findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    childs <- listDirectory currDir
    let cabalFiles = filter (\c -> takeExtension c == ".cabal") childs
    case cabalFiles of
        [] -> throwVabalError "No cabal file found."
        (cf:cfs) -> return cf

installGhcBinary :: String -> String -> FilePath -> IO ()
installGhcBinary buildType version outputDir = withTemporaryDirectory $ \tmpDir -> do
    let outputFilename = tmpDir </> "ghc.tar.xz"
    downloadGhcBinaries buildType version outputFilename

    withCurrentDirectory tmpDir $ do
        extractArchive outputFilename
        withCurrentDirectory ("ghc-" ++ version) $ do
            createDirectory outputDir
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
        ExitFailure _ -> throwVabalError "Error while extracting ghc binary archive."

installBinaries :: FilePath -> IO ()
installBinaries outputDir = do
    putStrLn "Installing binaries."
    res1 <- runExternalProcess "./configure" ["--silent", "--prefix=" ++ outputDir]
    case res1 of
        ExitFailure _ -> throwVabalError "Error while installing binaries"
        ExitSuccess -> do
            res2 <- runExternalProcess "make" ["install", "--silent"]
            case res2 of
                ExitSuccess -> return ()
                ExitFailure _ -> throwVabalError "Error while installing binaries"

