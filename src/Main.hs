module Main where

import System.Directory
import System.FilePath
import System.Process
import System.IO (hGetLine, hFlush, stdout)
import System.Environment (getArgs)

import Control.Exception
import System.Exit

import VabalError
import CabalAnalyzer

import Data.List (find)
import Control.Monad (when)

import ProcessUtils
import FlagsUtils

import GhcupProgram

import Distribution.Types.GenericPackageDescription

withExceptionHandler :: Exception e => (e -> IO a) -> IO a -> IO a
withExceptionHandler = flip catch

showHelp :: IO ()
showHelp = do
    putStrLn "vabal usage:"
    putStrLn "vabal <flags>"
    putStrLn "<flags>  is a list of space separated flags."
    putStrLn "         You can prefix a flag with -"
    putStrLn "         to disable it."
    putStrLn "         You can use + (or no prefix at all"
    putStrLn "         to enable the flag, instead."

main :: IO ()
main = do
    args <- getArgs
    let errorHandler :: SomeException -> IO ()
        errorHandler ex = putStrLn $ show ex

    
    if "--help" `elem` args then
        showHelp
    else
        catch (vabalConfigure args) errorHandler
    return ()

vabalConfigure :: [String] -> IO ()
vabalConfigure args = do

    cabalFilePath <- findCabalFile

    let flags = makeFlagAssignment args

    version <- analyzeCabalFileAllTargets flags cabalFilePath

    path <- requireGHC version

    runCabalConfigure flags path

    return ()


-- Run cabal new-configure with the given compiler version
-- If The filepath is Nothing, then use the ghc in path
runCabalConfigure :: FlagAssignment -> GhcLocation -> IO ()
runCabalConfigure flags outputDir = do
    putStrLn "Running cabal new-configure."

    let args = case outputDir of
            InPath                   -> ["new-configure"]
            CustomLocation outputDir -> ["new-configure", "-w", outputDir </> "bin" </> "ghc"]

    let argsPlusFlags = args ++ makeCabalArguments flags
    res <- runExternalProcess "cabal" argsPlusFlags
    case res of
        ExitSuccess -> return ()
        ExitFailure _ -> throwVabalErrorIO "Error while running cabal configure."

findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    childs <- listDirectory currDir
    let cabalFiles = filter (\c -> takeExtension c == ".cabal") childs
    case cabalFiles of
        [] -> throwVabalErrorIO "No cabal file found."
        (cf:cfs) -> return cf

