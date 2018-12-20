module GhcMetadataDownloader where

import Network.HTTP.Client as N
import Network.HTTP.Types.Status as N
import Network.HTTP.Client.TLS as N

import Data.ByteString.Lazy as B

import VabalError

metadataUrl :: String
metadataUrl = "https://raw.githubusercontent.com/Franciman/vabal-ghc-metadata/master/ghc-metadata.csv"

downloadGhcMetadata :: FilePath -> IO ()
downloadGhcMetadata filepath = do
    manager <- N.newTlsManager
    request <- N.parseRequest metadataUrl
    response <- N.httpLbs request manager

    if N.responseStatus response /= N.status200 then
        throwVabalErrorIO "Error while downloading metadata."
    else
        B.writeFile filepath (N.responseBody response)


