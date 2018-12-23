module GhcMetadata where

import Network.HTTP.Client as N
import Network.HTTP.Types.Status as N
import Network.HTTP.Client.TLS as N

import Data.ByteString.Lazy as B

import VabalError

import System.Directory
import System.FilePath

import VabalContext

metadataUrl :: String
metadataUrl = "https://raw.githubusercontent.com/Franciman/vabal-ghc-metadata/master/ghc-metadata.csv"


getGhcMetadataDir :: IO FilePath
getGhcMetadataDir = do
    homeDir <- getHomeDirectory
    return (homeDir </> ".vabal")

ghcMetadataFilename :: String
ghcMetadataFilename = "ghc-metadata.csv"


readGhcMetadata :: FilePath -> IO GhcToBaseMap
readGhcMetadata filepath = do
    fileExists <- doesFileExist filepath

    if not fileExists then
        throwVabalErrorIO "Ghc metadata not found, run `vabal update` to download it."
    else do
      contents <- B.readFile filepath
      case readGhcToBaseMap contents of
        Left err -> throwVabalErrorIO $ "Error, could not parse ghc metadata:\n" ++ err
        Right res -> return res

downloadGhcMetadata :: FilePath -> IO ()
downloadGhcMetadata filepath = do
    manager <- N.newTlsManager
    request <- N.parseRequest metadataUrl
    response <- N.httpLbs request manager

    if N.responseStatus response /= N.status200 then
        throwVabalErrorIO "Error while downloading metadata."
    else
        B.writeFile filepath (N.responseBody response)


