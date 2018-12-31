module GhcupProgram where

import Data.List (intercalate)

import System.Directory
import Control.Exception (SomeException, handle)
import System.FilePath
import System.Environment (lookupEnv)

import Distribution.Parsec.Class

import Distribution.Version

import Control.Monad (unless)

import Data.Maybe (fromMaybe, maybeToList)

import System.IO (stderr)

import VabalError

import GhcDatabase

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

ghcInPathVersion :: IO (Maybe String)
ghcInPathVersion = do
    let noGhcFound :: SomeException -> IO (Maybe String)
        noGhcFound _ = return Nothing

    handle noGhcFound $ do
        ghcVer <- removeTrailingNewlines <$> readProcess "ghc" ["--numeric-version"] ""
        return $ Just ghcVer

getInstalledGhcs :: IO [Version]
getInstalledGhcs = do
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
requireGHC :: GhcDatabase -> Version -> Bool -> IO (Maybe FilePath)
requireGHC installedGhcs ghcVer noInstall = do
    let version = prettyPrintVersion ghcVer
    ghcPathIsGood <- checkGhcInPath version

    if ghcPathIsGood then
        return Nothing
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

        return . Just $ ghcupInstallBasePrefix </> ".ghcup" </> "ghc" </> version </> "bin" </> "ghc"

