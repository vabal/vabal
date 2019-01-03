module VabalNix where

import VabalMain

import Options.Applicative

import Backend
import NixBackend
import VabalContext

import UserInterface
import Utils

nixArgumentsParser :: Parser VabalMainArguments
nixArgumentsParser = mainArgumentsParser

nixProgDesc :: String
nixProgDesc = "Finds a version of GHC that is compatible with \
               \ the constraints imposed on base package found \
               \ in the cabal file analyzed, then runs cabal2nix \
               \ inside a nix-shell to produce a shell.nix \
               \ file for the cabal package. \
               \ The shell.nix package by default will use the GHC version determined."

vabalNix :: VabalMainArguments -> IO ()
vabalNix args = do
    let backend = nixBackend
    vabalCtx <- makeVabalContext backend args
    envParams <- vabalMakeEnvParams args vabalCtx
    _ <- (setupEnv backend) envParams (availableGhcs vabalCtx) (noInstallFlag args)
    writeMessage $ "Selected GHC version: " ++ prettyPrintVersion (envGhcVersion envParams)
    return ()
