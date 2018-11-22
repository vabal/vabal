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

import VabalError

import CabalAnalyzer

import Data.List (find)

import Downloader


withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action =
    bracket (mkdtemp "/tmp/ghc-binaries")
            removeDirectoryRecursive
            action


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

main :: IO ()
main = getArgs >>= \args -> case detectGhcBuildType of
                                Nothing -> putStrLn "Unsupported platform."
                                Just buildType -> vabalConfigure buildType args


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

    if ghcAlreadyInstalled then
        putStrLn "Already installed."
    else do
        putStr $ "Do you want to download GHC " ++ version ++ "? [Y/n]:"
        response <- getLine
        case response of
            "n" -> throwVabalError "Download aborted."
            _   -> do
                installGhcBinary buildType version outputDir
                putStrLn "Ghc installed."

    callProcess "cabal" ["new-configure", "-w", outputDir </> "bin" </> "ghc"]
    return ()


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
    callProcess "tar" ["-xJf", archive]
    return ()

installBinaries :: FilePath -> IO ()
installBinaries outputDir = do
    callProcess "./configure" ["--prefix=" ++ outputDir]
    callProcess "make" ["install"]
    return ()

