{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHCInstallation where

import Control.Exception
import System.Posix.Temp
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N

import GHCBinaryIntegrityVerifier
import InstallationContext
import ProcessUtils
import Downloader
import VabalError

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action =
    bracket (mkdtemp "/tmp/ghc-binaries")
            removeDirectoryRecursive
            action

installGhcBinary :: String -> String -> FilePath -> IO ()
installGhcBinary buildType version installDir = withTemporaryDirectory $ \tmpDir -> do
    netManager <- N.newTlsManager
    let outputFilename = tmpDir </> "ghc.tar.xz"
    let installationContext = InstallationContext
                            { buildType          = buildType
                            , version            = version
                            , installDir         = installDir
                            , tmpDir             = tmpDir
                            , ghcArchiveFilename = outputFilename
                            , netManager         = netManager
                            }
    downloadGhcBinaries installationContext
    verifyDownloadedBinary installationContext

    withCurrentDirectory tmpDir $ do
        extractArchive outputFilename
        withCurrentDirectory ("ghc-" ++ version) $ do
            installBinaries installDir


downloadGhcBinaries :: InstallationContext -> IO ()
downloadGhcBinaries ctx = do
    let baseUrl = "https://downloads.haskell.org/~ghc/" ++ version ctx
    let buildName = "ghc-" ++ version ctx ++ "-" ++ buildType ctx ++ ".tar.xz"
    let downloadUrl = baseUrl ++ "/" ++ buildName

    putStrLn "Downloading GHC."
    runDownloader downloadUrl (netManager ctx) (ghcArchiveFilename ctx)


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

