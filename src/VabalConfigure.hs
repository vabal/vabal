module VabalConfigure where

import VabalMain

import Options.Applicative
import System.Exit
import System.Process

import VabalError


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
    let args = vabalArgs ++ [ "--", "cabal", "v2-configure" ] ++ cabalArgs

    (_, _, _, vabalProcHandle) <- createProcess (proc "vabal" args)
 
    vabalExitCode <- waitForProcess vabalProcHandle

    case vabalExitCode of
        ExitFailure _ -> throwVabalErrorIO "Vabal failed, exiting."

        ExitSuccess   -> return ()
