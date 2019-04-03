module MetadataManager where

import Network.HTTP.Client as N
import Network.HTTP.Types.Status as N
import Network.HTTP.Client.TLS as N

import Data.ByteString.Lazy as B

import VabalError

import System.Directory
import System.FilePath

import GhcDatabase

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar

import Control.Exception

getGhcMetadataDir :: IO FilePath
getGhcMetadataDir = do
    homeDir <- getHomeDirectory
    return (homeDir </> ".vabal")

ghcMetadataFilename :: String
ghcMetadataFilename = "vabal-ghc-metadata.csv"

defaultFakeGhcURL :: String
defaultFakeGhcURL = "https://github.com/vabal/vabal-ghc-metadata/raw/master/fake-ghc.tar"

fakeGhcFilename :: String
fakeGhcFilename = "fake-ghc"

hasGhcMetadata :: IO Bool
hasGhcMetadata = do
    baseDir <- getGhcMetadataDir
    b1 <- doesFileExist (baseDir </> ghcMetadataFilename)
    b2 <- doesDirectoryExist (baseDir </> fakeGhcFilename)
    return (b1 && b2)

readGhcDatabase :: FilePath -> IO GhcDatabase
readGhcDatabase filepath = do
    fileExists <- doesFileExist filepath

    if not fileExists then
        throwVabalErrorIO "Ghc metadata not found, run `vabal update` to download it."
    else do
      contents <- B.readFile filepath
      case parseGhcDatabase contents of
        Left err -> throwVabalErrorIO $ "Error, could not parse ghc metadata:\n" ++ err
        Right res -> return res

downloadGhcDatabase :: N.Manager -> FilePath -> IO ()
downloadGhcDatabase manager filepath = do
    request <- N.parseRequest defaultGhcDatabaseURL
    response <- N.httpLbs request manager

    if N.responseStatus response /= N.status200 then
        throwVabalErrorIO "Error while downloading metadata."
    else
        B.writeFile filepath (N.responseBody response)

downloadFakeGhcs :: N.Manager -> FilePath -> IO ()
downloadFakeGhcs manager baseDir = do
        request <- N.parseRequest defaultFakeGhcURL
        response <- N.httpLbs request manager

        if N.responseStatus response /= N.status200 then
            throwVabalErrorIO "Error while downloading metadata."
        else unpackTar . Tar.checkSecurity $ Tar.read (N.responseBody response)

    where unpackTar (Tar.Fail e)    = either throwIO throwIO e
          unpackTar Tar.Done        = return ()
          unpackTar (Tar.Next e es) = do
              let entryPath = Tar.entryPath e
              case Tar.entryContent e of
                  Tar.NormalFile content _ -> unpackFile entryPath content
                  Tar.Directory            -> unpackDir entryPath
                  _                        -> return () -- Ignore other entries, we don't expect them
              unpackTar es

          unpackFile entryPath content = do
              let absPath = baseDir </> entryPath
              let dir     = baseDir </> takeDirectory entryPath
              createDirectoryIfMissing True dir
              B.writeFile absPath content
              -- Make the scripts executable
              p <- getPermissions absPath
              setPermissions absPath ( p { executable = True } )
 
          unpackDir entryPath = createDirectoryIfMissing True (baseDir </> entryPath)

downloadMetadata :: FilePath -> IO ()
downloadMetadata baseDir  = do
    manager <- N.newTlsManager
    downloadGhcDatabase manager (baseDir </> ghcMetadataFilename)
    downloadFakeGhcs manager baseDir
