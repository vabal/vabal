module GhcupProgram where

import Data.List (intercalate)

import System.Directory
import Control.Exception (SomeException, catch)
import System.FilePath
import System.Environment (lookupEnv)

import Distribution.Parsec.Class

import Distribution.Version

import Control.Monad (unless)

import Data.Maybe (fromMaybe)

import System.IO (stderr)

import VabalError

import VabalContext

import System.Process
import System.Exit

runExternalProcess :: FilePath -> [String] -> IO ExitCode
runExternalProcess bin args = do
    let processDescr = (proc bin args)
                     { std_out = UseHandle stderr
                     , std_err = UseHandle stderr
                     }
    (_, _, _, procHandle) <- createProcess processDescr
    waitForProcess procHandle

removeTrailingNewlines :: String -> String
removeTrailingNewlines = reverse . dropWhile (== '\n') . reverse

prettyPrintVersion :: Version -> String
prettyPrintVersion ver = intercalate "." $ map show (versionNumbers ver)

trimVersionString :: String -> String
trimVersionString = dropWhile (== ' ')

unableToReadGhcupOutputError :: a
unableToReadGhcupOutputError = throwVabalError "Could not parse ghcup output."

getInstalledGhcs :: IO [Version]
getInstalledGhcs = do
    output <- readProcess "ghcup" ["show"] ""

    let installedVersions = map trimVersionString
                          . tail -- Ignore line containing the header
                          . takeWhile (not . null)
                          . lines
                          $ output

    case installedVersions of
        ["None"] -> return []
        _ -> return $ map (fromMaybe unableToReadGhcupOutputError . simpleParsec) installedVersions


checkGhcInPath :: String  -> IO (Maybe FilePath)
checkGhcInPath version = catch checkGhcAndGetPath noGhcFound
    where noGhcFound :: SomeException -> IO (Maybe FilePath)
          noGhcFound _ = return Nothing

          checkGhcAndGetPath = do
              ghcVer <- removeTrailingNewlines <$> readProcess "ghc" ["--numeric-version"] ""
              if version == ghcVer then
                  -- if the previos command didn't fail,
                  -- it's *almost* sure this one won't fail
                  Just . removeTrailingNewlines <$> readCreateProcess (shell "command -v ghc") ""
              else
                  return Nothing

-- Asks ghcup to get the provided version for ghc,
-- It'll return the file path of the downloaded ghc.
-- If an error occurs a VabalError is thrown.
requireGHC :: GhcToBaseMap -> Version -> Bool -> IO FilePath
requireGHC installedGhcs ghcVer noInstall = do
    let version = prettyPrintVersion ghcVer
    ghcPath <- checkGhcInPath version

    case ghcPath of
        Just path -> return path
        Nothing -> do
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

