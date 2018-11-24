{-# LANGUAGE TemplateHaskell #-}
module GHCBinaryIntegrityVerifier (verifyDownloadedBinary) where
    
import System.IO.Unsafe
import Data.Either (isRight)
import qualified Data.Conduit as DC
import Data.Conduit ((.|))
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

import Data.FileEmbed

import System.FilePath

import VabalError

import Debug.Trace

import InstallationContext


ghcSignaturePublicKey :: BS.ByteString
ghcSignaturePublicKey = $(embedFile "ghcSignaturePublicKey")

verifySignature :: BS.ByteString -> BS.ByteString -> B.ByteString -> IO Bool
verifySignature pubKey sig file = do
    kr <- DC.runConduitRes $ DC.yield pubKey
                          .| conduitGet get
                          .| conduitToTKs
                          .| sinkKeyringMap

    sigData <- DC.runConduitRes $ DC.yield sig
                                .| conduitGet get
                                .| conduitDecompress
                                .| CL.consume

    -- Take only signature packets
    let isSignature (SignaturePkt _) = True
        isSignature _                = False

    let signatures = filter isSignature sigData
    -- Try validating against any of the signatures,
    -- stopping when we find one that succeeds
    -- (this is guaranteed by lazy semantics of any)
    let res = any (\sig -> isRight $ verifyAgainstKeyring kr sig Nothing file) signatures

    return res


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


tryFindResource :: N.Manager -> String -> IO (Maybe B.ByteString)
tryFindResource manager url = do
    request <- N.parseRequest url

    response <- N.httpLbs request manager
    case N.responseStatus response == N.status200 of
        False -> return Nothing
        True -> return . Just $ N.responseBody response

verifySha256Sum :: FilePath -> String -> B.ByteString -> B.ByteString -> IO ()
verifySha256Sum ghcBinaryFilePath ghcBinaryName shasums shasumsSignature = do
    res <- verifySignature ghcSignaturePublicKey
                           (B.toStrict shasumsSignature)
                           shasums
    if not res then
        throwVabalErrorIO "Invalid SHA256SUMS, PGP Verification Failed"
    else do
        case extractSha256Sum shasums ghcBinaryName of
            Nothing -> throwVabalErrorIO "Can't find checksum for downloaded binary."
            Just shasum -> do
                hash <- getBinaryHash ghcBinaryFilePath
                case shasum == hash of
                    True -> putStrLn "Binary integrity successfully verified."
                    False -> throwVabalErrorIO "The downloaded binary is not valid."

verifyUsingBinarySignature :: FilePath -> B.ByteString -> IO ()
verifyUsingBinarySignature ghcBinaryFilePath binarySignature = do
    binaryData <- B.readFile ghcBinaryFilePath
    res <- verifySignature ghcSignaturePublicKey
                           (B.toStrict binarySignature)
                           binaryData
    if res then
        putStrLn "Binary integrity successfully verified."
    else
        throwVabalErrorIO "The downloaded binary is not valid."

verifyDownloadedBinary :: InstallationContext -> IO ()
verifyDownloadedBinary ctx = do
    let ghcBinaryName = "ghc-" ++ version ctx ++ "-" ++ buildType ctx ++ ".tar.xz"

    let baseUrl = "https://downloads.haskell.org/~ghc/" ++ version ctx

    let ghcBinarySigUrl = baseUrl ++ "/" ++ ghcBinaryName ++ ".sig"

    let shasumsUrl = baseUrl ++ "/" ++ "SHA256SUMS"
    let shasumsSigUrl = shasumsUrl ++ ".sig"

    let manager = netManager ctx

    -- First try looking for a signature of the binary file
    -- If not found try with the SHA256SUMS file
    -- Finally desist and don't verify
    binarySig <- tryFindResource manager ghcBinarySigUrl
    case binarySig of
        Just binarySig -> verifyUsingBinarySignature (ghcArchiveFilename ctx) binarySig
        Nothing -> do
            putStrLn "Signature for binary file not found. Trying with SHA256SUMS file."
            shasums <- tryFindResource manager shasumsUrl
            case shasums of
                Nothing -> putStrLn "Warning: There was no SHA256SUMS file to use to verify integrity."
                Just shasums -> do
                    shasumsSignature <- tryFindResource manager shasumsSigUrl
                    case shasumsSignature of
                        Nothing -> putStrLn "Warning: No signature for the SHA256SUMS file found."
                        Just shasumsSignature -> do
                            verifySha256Sum (ghcArchiveFilename ctx)
                                            ghcBinaryName
                                            shasums
                                            shasumsSignature

