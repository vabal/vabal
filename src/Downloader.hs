module Downloader (runDownloader) where

import ListT

import qualified Data.ByteString as B

import qualified Network.HTTP.Client as N
import qualified Network.HTTP.Client.TLS as N
import qualified Network.HTTP.Types.Header as N
import qualified Network.HTTP.Types.Status as N

import System.IO (hFlush, stdout, withFile, Handle, IOMode(WriteMode))

import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Data.List (find)

import VabalError

type ChunkProducer = ListT IO B.ByteString


-- Pretty ugly
-- We mix chunk generation with chunk size calculation
downloader :: N.BodyReader -> ListT IO (Int, B.ByteString)
downloader bodyReader = unfoldM go 0
    where go :: Int -> IO (Maybe ((Int, B.ByteString), Int))
          go byteCount = do
              chunk <- N.brRead bodyReader
              if B.null chunk then
                  return Nothing
              else do
                  let newByteCount = byteCount + B.length chunk
                  return $ Just ((byteCount, chunk), newByteCount)


writeChunk :: Handle -> B.ByteString -> IO ()
writeChunk handle chunk = B.hPut handle chunk

progress :: Int -> Int -> IO ()
progress totalSize byteCount = do
    let perc = round $ (realToFrac byteCount / realToFrac totalSize) * 100
    putStr $ "Progress: " ++ show perc ++ "%\r"
    hFlush stdout
    return ()

downloadCountLoop :: Int -> IO ()
downloadCountLoop byteCount = return ()

getContentLength :: N.Response a -> Maybe Int
getContentLength resp =
    let headers = N.responseHeaders resp
        contentLengthHeader = find (\header -> fst header == N.hContentLength) headers
    in read . T.unpack . decodeUtf8 . snd <$> contentLengthHeader


for_ :: Monad m => ListT m a -> (a -> m ()) -> m ()
for_ = flip traverse_

runDownloader :: String -> N.Manager -> FilePath -> IO ()
runDownloader url manager outputFilename = do
    print url
    request <- N.parseRequest url

    N.withResponse request manager $ \resp -> do
        let contentLength = getContentLength resp

        let progressReporter = maybe downloadCountLoop progress contentLength
        case N.responseStatus resp == N.status200 of
            False -> throwVabalErrorIO "Error while downloading GHC."
            True  -> do
                putStr "Progress: 0%\r"
                hFlush stdout
                withFile outputFilename WriteMode $ \handle -> do
                    for_ (downloader $ N.responseBody resp) $ \(byteCount, chunk) -> do
                        writeChunk handle chunk
                        progressReporter byteCount
                putStrLn "\nDone"

