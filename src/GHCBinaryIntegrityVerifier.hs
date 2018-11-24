module GHCBinaryIntegrityVerifier (verifyDownloadedBinary) where

import System.IO.Unsafe
import qualified Data.Conduit as DC
import Data.Conduit ((.|))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Binary (get)
import Data.Conduit.Serialization.Binary (conduitGet)
import Data.List (find)
import Data.Char (isSpace)

import Data.Conduit.OpenPGP.Keyring
import Data.Conduit.OpenPGP.Compression
import Data.Conduit.OpenPGP.Verify

import Codec.Encryption.OpenPGP.Signatures
import Codec.Encryption.OpenPGP.Types

import qualified Data.ByteString.Lazy as B

import qualified Data.ByteString as BS

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

import qualified Data.ByteString.Base16 as B16 (encode)

import Crypto.Hash.SHA256 (hashlazy)

import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N
import qualified Network.HTTP.Types as N

import System.FilePath

import VabalError

import Debug.Trace

import InstallationContext

verifySignature :: FilePath -> BS.ByteString -> B.ByteString -> IO Bool
verifySignature pubKey sig file = do
    kr <- DC.runConduitRes $ CB.sourceFile pubKey
                          .| conduitGet get
                          .| conduitToTKs
                          .| sinkKeyringMap

    sigData <- DC.runConduitRes $ DC.yield sig
                                .| conduitGet get
                                .| conduitDecompress
                                .| CL.consume

    -- Get first signature
    let isSignature (SignaturePkt _) = True
        isSignature _                = False

    let sig = find isSignature sigData

    case sig of
        Nothing -> return False
        Just signature -> do
            let res = verifyAgainstKeyring kr signature Nothing file
            case res of
                Left err -> return False
                Right ver -> return True


getBinaryHash :: FilePath -> IO BS.ByteString
getBinaryHash path = B16.encode . hashlazy <$> B.readFile path

splitRow :: T.Text -> Maybe (T.Text, T.Text)
splitRow row = let (hashSum, rest) = T.break isSpace row
                   (separator, filename') = T.break (== '/') rest
                   filename = T.tail filename' -- Remove the '/' at start
               in case T.null hashSum || T.null filename of
                      False -> Just (hashSum, filename)
                      True -> Nothing

extractSha256Sum :: B.ByteString -> String -> Maybe BS.ByteString
extractSha256Sum shaSums ghcBinaryName = do
    -- Force the bytestring, the shasum file won't be big
    let entries = T.lines . T.decodeUtf8 . B.toStrict $ shaSums
    return $ unsafePerformIO $ print (traverse splitRow entries)
    checksums <- traverse splitRow entries
    binarySha256Sum <- fst <$> find ((== T.pack ghcBinaryName) . snd) checksums
    return $ T.encodeUtf8 binarySha256Sum


verifyDownloadedBinary :: InstallationContext -> IO ()
verifyDownloadedBinary ctx = do
    let ghcBinaryName = "ghc-" ++ version ctx ++ "-" ++ buildType ctx ++ ".tar.xz"

    let baseUrl = "https://downloads.haskell.org/~ghc/" ++ version ctx

    let downloadUrl = baseUrl ++ "/" ++ "SHA256SUMS"
    let sigDownloadUrl = downloadUrl ++ ".sig"

    let manager = netManager ctx
    request <- N.parseRequest downloadUrl

    shasumsResponse <- N.httpLbs request manager
    case N.responseStatus shasumsResponse == N.status200 of
        False -> putStrLn "Warning: It was no shasums file to use to verify integrity."
        True -> do
            signatureRequest <- N.parseRequest sigDownloadUrl

            let shasums = N.responseBody shasumsResponse

            shasumsSignatureResponse <- N.httpLbs signatureRequest manager
            case N.responseStatus shasumsSignatureResponse == N.status200 of
                False -> putStrLn "Warning: No signature for the shasums file found to validate the shasums."
                True -> do
                    let shasumsSignature = N.responseBody shasumsSignatureResponse
                    res <- verifySignature "/home/francesco/Projects/vabal/ghcSignaturePublicKey"
                                           (B.toStrict shasumsSignature)
                                           shasums

                    if not res then
                        throwVabalErrorIO "Invalid SHA256SUMS, PGP Verification Failed"
                    else do
                        case extractSha256Sum shasums ghcBinaryName of
                            Nothing -> return () -- throwVabalErrorIO "Can't find binary checksum"
                            Just shasum -> do
                                hash <- getBinaryHash $ ghcArchiveFilename ctx
                                case shasum == hash of
                                    True -> putStrLn "Binary integrity successfully verified."
                                    False -> throwVabalErrorIO "The downloaded binary is not valid."

