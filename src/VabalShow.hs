module VabalShow where

import Options.Applicative

import VabalMain

import UserInterface
import VabalContext
import GhcupProgram

showArgumentsParser :: Parser VabalMainArguments
showArgumentsParser = mainArgumentsParser

showProgDesc :: String
showProgDesc = "Finds a version of GHC that is compatible with \
               \ the constraints imposed on base package found \
               \ in the cabal file analyzed, \
               \ then uses ghcup to obtain it (possibly downloading it). \
               \ Finally it prints to stdout the version of the obtained GHC. \
               \ WARNING: Probably this is not what you want to use, \
               \ See \"vabal configure --help\" for info about how to \
               \ directly configure your project to use the found GHC compiler."
vabalShow :: VabalMainArguments -> IO ()
vabalShow args = do
    vabalCtx <- makeVabalContext args
    version <- vabalFindGhcVersion args vabalCtx
    _ <- requireGHC (availableGhcs vabalCtx) version (noInstallFlag args)
    writeOutput $ prettyPrintVersion version
