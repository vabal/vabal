module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as Lazy

import Distribution.Types.GenericPackageDescription

import System.FilePath
import System.Directory

import CabalAnalyzer

import Control.Exception
import Control.DeepSeq

import VabalContext
import VabalError

readGhcMetadata :: FilePath -> IO GhcToBaseMap
readGhcMetadata filepath = do
    fileExists <- doesFileExist filepath

    if not fileExists then
        throwVabalErrorIO "Ghc metadata not found, run `vabal update` to download it."
    else do
      contents <- Lazy.readFile filepath
      case readGhcToBaseMap contents of
        Left err -> throwVabalErrorIO $ "Error, could not parse ghc metadata:\n" ++ err
        Right res -> return res

cabalAnalyzer :: GhcToBaseMap -> (FilePath, Lazy.ByteString) -> IO Bool
cabalAnalyzer ghcDb (path, pkg) = do
    let ctx = VabalContext emptyMap ghcDb False
    let fun = return . analyzeCabalFileAllTargets (mkFlagAssignment []) ctx Nothing . Lazy.toStrict

    let errorHandler :: SomeException -> IO Bool
        errorHandler _ = do
            putStrLn path
            return False
    handle errorHandler $ do
        res <- fun pkg
        res `deepseq` return True


takeCabalFile :: Tar.Entry -> [(FilePath, Lazy.ByteString)] -> [(FilePath, Lazy.ByteString)]
takeCabalFile entry accum =
    case Tar.entryContent entry of
        Tar.NormalFile content _
            | takeExtension (Tar.entryPath entry) == ".cabal" -> (Tar.entryPath entry, content) : accum
            | otherwise                                       -> accum

        _ -> accum

main :: IO ()
main = do

    homeDir <- getHomeDirectory
    let filepath = homeDir </> ".cabal/packages/hackage.haskell.org/01-index.tar"

    indexContents <- Lazy.readFile filepath

    ghcDb <- readGhcMetadata $ homeDir </> ".vabal/ghcMetadata.csv"

    let entries = Tar.read indexContents

    let cabalFiles = Tar.foldEntries takeCabalFile [] (error "Error while reading index") entries

    putStrLn "This is the list of packages on which I failed:"

    res <- mapM (cabalAnalyzer ghcDb) cabalFiles

    let success = length $ filter id res

    putStrLn $ "Success: " ++ show success
    putStrLn $ "Failure: " ++ show (length res - success)

    return ()
