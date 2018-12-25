module VabalConfigure where

import VabalMain

import Options.Applicative
import System.Exit
import System.Process

import System.IO (hPutStr, hClose)

import UserInterface

configureArgumentsParser :: Parser VabalMainArguments
configureArgumentsParser = mainArgumentsParser

configureProgDesc :: String
configureProgDesc = "Finds a version of GHC that is compatible with \
               \ the constraints imposed on base package found \
               \ in the cabal file analyzed, \
               \ then uses ghcup to obtain it (possibly downloading it). \
               \ Then it configures your project \
               \ to use the obtained GHC compiler by running \
               \ \"cabal v2-configure\". \
               \ In order to pass other custom arguments to \"cabal v2-configure\", \
               \ specify them after --."


vabalConfigure :: [String] -> [String] -> IO ()
vabalConfigure cabalArgs vabalArgs = do
    (vabalExitCode, vabalOutput, vabalErr) <- readProcessWithExitCode "vabal" vabalArgs ""
    case vabalExitCode of
        ExitFailure _ -> do
            writeError vabalErr
            exitWith vabalExitCode

        ExitSuccess   -> do
            let xargsArgs = ["-t", "cabal", "v2-configure"] ++ cabalArgs
            let procDescr = (proc "xargs" xargsArgs)
                          { std_in = CreatePipe
                          }
            (Just handle, _, _, processHandle) <- createProcess procDescr
            hPutStr handle vabalOutput
            hClose handle
            xargsExitCode <- waitForProcess processHandle
            exitWith xargsExitCode

