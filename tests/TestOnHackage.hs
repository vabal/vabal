module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as Lazy

import Distribution.Types.GenericPackageDescription

import System.FilePath
import System.Directory

import CabalAnalyzer

import Control.Exception
import Control.DeepSeq

cabalAnalyzer :: (FilePath, Lazy.ByteString) -> IO Bool
cabalAnalyzer (path, pkg) = do
    let fun = return . analyzeCabalFileAllTargets (mkFlagAssignment []) Nothing . Lazy.toStrict

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

    let entries = Tar.read indexContents

    let cabalFiles = Tar.foldEntries takeCabalFile [] (error "Error while reading index") entries

    putStrLn "This is the list of packages on which I failed:"

    res <- mapM cabalAnalyzer cabalFiles

    let success = length $ filter id res

    putStrLn $ "Success: " ++ show success
    putStrLn $ "Failure: " ++ show (length res - success)

    return ()
