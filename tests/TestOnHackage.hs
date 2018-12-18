module Main where

import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as B

import Distribution.Version
import Distribution.Types.GenericPackageDescription

import System.FilePath
import System.Directory

import CabalAnalyzer

import Control.Exception

import Control.Monad (void)

import Control.DeepSeq

import qualified Data.Map.Lazy as M

cabalAnalyzer :: (FilePath, Lazy.ByteString) -> IO Bool
cabalAnalyzer (path, pkg) = do
    let fun = return . analyzeCabalFileAllTargets (mkFlagAssignment []) Nothing . Lazy.toStrict

    let errorHandler :: SomeException -> IO Bool
        errorHandler ex = do
            putStrLn path
            return False
    (flip catch) errorHandler $ do
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
