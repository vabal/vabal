module GhcupBackend (ghcupBackend) where

import System.Directory
import Control.Exception (SomeException, handle)
import System.FilePath
import System.Environment (lookupEnv)

import Distribution.Parsec.Class

import Distribution.Version

import Control.Monad (unless)

import Data.Maybe (fromMaybe, maybeToList)

import VabalError

import GhcDatabase

import System.Process
import System.Exit

import Backend

import Utils

unableToReadGhcupOutputError :: a
unableToReadGhcupOutputError = throwVabalError "Could not parse ghcup output."

ghcInPathVersion :: IO (Maybe String)
ghcInPathVersion = do
    let noGhcFound :: SomeException -> IO (Maybe String)
        noGhcFound _ = return Nothing

    handle noGhcFound $ do
        ghcVer <- removeTrailingNewlines <$> readProcess "ghc" ["--numeric-version"] ""
        return $ Just ghcVer

ghcupGetInstalledGhcs :: IO [Version]
ghcupGetInstalledGhcs = do
    output <- readProcess "ghcup" ["show"] ""

    let ghcupInstalledVersions = filter (/= "None") -- Ignore the None line (when there is no ghc installed)
                          . map trimVersionString
                          . tail -- Ignore line containing the header
                          . takeWhile (not . null)
                          . lines
                          $ output

    inPathVersion <- maybeToList <$> ghcInPathVersion

    let installedVersions = inPathVersion ++ ghcupInstalledVersions

    return $ map (fromMaybe unableToReadGhcupOutputError . simpleParsec) installedVersions


checkGhcInPath :: String  -> IO Bool
checkGhcInPath version = do
    ghcVer <- ghcInPathVersion
    case ghcVer of
        Nothing -> return False
        Just ghcVer' -> return $ version == ghcVer'

-- Asks ghcup to get the provided version for ghc,
-- It'll return the file path of the downloaded ghc.
-- If an error occurs a VabalError is thrown.
ghcupRequireGHC :: GhcDatabase -> Version -> Bool -> IO FilePath
ghcupRequireGHC installedGhcs ghcVer noInstall = do
    let version = prettyPrintVersion ghcVer
    ghcInPathIsGood <- checkGhcInPath version

    if ghcInPathIsGood then do
        removeTrailingNewlines <$> readCreateProcess (shell "command -v ghc") ""
    else do
        let ghcAlreadyInstalled = hasGhcVersion installedGhcs ghcVer
        unless ghcAlreadyInstalled $
            if noInstall then
                throwVabalErrorIO "Required GHC version is not available on the system."
            else do
                res <- runExternalProcess "ghcup" ["install", version]
                case res of
                    ExitFailure _ -> throwVabalErrorIO "Error while installing ghc."
                    ExitSuccess   -> return ()

        -- ghcup's install directory can be customized through the use of
        -- the GHCUP_INSTALL_BASE_PREFIX env variabile.
        -- If it is not set, its default value is $HOME
        homeDir <- getHomeDirectory
        ghcupInstallBasePrefix <- fromMaybe homeDir
                                  <$> lookupEnv "GHCUP_INSTALL_BASE_PREFIX"

        return $ ghcupInstallBasePrefix </> ".ghcup" </> "ghc" </> version </> "bin" </> "ghc"

ghcupSetupEnv :: EnvParams
              -> GhcDatabase
              -> Bool
              -> IO [CabalOption]
ghcupSetupEnv envParams installedGhcs noInstall = do
    ghcLoc <- ghcupRequireGHC installedGhcs (envGhcVersion envParams) noInstall
    return ["-w", ghcLoc]

ghcupBackend :: Backend
ghcupBackend = Backend
             { getInstalledGhcs = ghcupGetInstalledGhcs
             , setupEnv         = ghcupSetupEnv
             }
