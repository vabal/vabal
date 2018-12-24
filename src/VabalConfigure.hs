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
configureProgDesc = "Finds a version of GHC (and downloads it, if necessary) \
               \ that is compatible with \
               \ the constraints imposed on base package found \
               \ in the cabal file analyzed. Then it configures your project \
               \ to use the said GHC compiler by running \
               \ \"cabal v2-configure\". \
               \ To pass other custom arguments to \"cabal v2-configure\" \
               \ specify them after --."


vabalConfigure :: [String] -> [String] -> IO ()
vabalConfigure cabalArgs vabalArgs = do
    (exitCode, vabalOutput, vabalErr) <- readProcessWithExitCode "vabal" vabalArgs ""
    case exitCode of
        ExitFailure _ -> writeError vabalErr
        ExitSuccess   -> do
            let xargsArgs = ["-t", "cabal", "v2-configure"] ++ cabalArgs
            let procDescr = (proc "xargs" xargsArgs)
                          { std_in = CreatePipe
                          }
            (Just handle, _, _, processHandle) <- createProcess procDescr
            hPutStr handle vabalOutput
            hClose handle
            _ <- waitForProcess processHandle
            return ()

