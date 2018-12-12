module GhcupProgram where

import ProcessUtils
import Data.List (intercalate)

import System.Process
import System.Exit
import System.Directory
import System.IO
import Control.Exception (SomeException, catch)
import System.FilePath

import Distribution.Version

import Control.Monad (when)

import VabalError

data GhcLocation = CustomLocation FilePath
                 | InPath



prettyPrintVersion :: Version -> String
prettyPrintVersion ver = intercalate "." $ map show (versionNumbers ver)

trimVersionString :: String -> String
trimVersionString = dropWhile (== ' ')

versionAlreadyInstalled :: String -> IO Bool
versionAlreadyInstalled version = do
    let processDescr = (proc "ghcup" ["show"])
                     { std_out = CreatePipe
                     }

    (_, Just outHandle, _, _) <- createProcess processDescr
    hGetLine outHandle -- Ignore header
    installedVersions <- (map trimVersionString . lines) <$> hGetContents outHandle
    return $ version `elem` installedVersions


checkGhcInPath :: String  -> IO Bool
checkGhcInPath version = catch getGhcVersion noGhcFound
    where noGhcFound :: SomeException -> IO Bool
          noGhcFound _ = return False

          getGhcVersion = do
              let procDescr = (proc "ghc" ["--numeric-version"])
                              { std_out = CreatePipe
                              }

              (_, Just outHandle, _, _) <- createProcess procDescr
              ghcVersion <- hGetLine outHandle
              return $ version == ghcVersion

-- Asks ghcup to get the provided version for ghc,
-- It'll return the file path of the downloaded ghc.
-- If an error occurs a VabalError is thrown.
requireGHC :: Version -> Bool -> IO GhcLocation
requireGHC ghcVersion noInstall = do
    let version = prettyPrintVersion ghcVersion
    ghcInPathIsFine <- checkGhcInPath version
    if ghcInPathIsFine then
        return InPath
    else do
        ghcAlreadyInstalled <- versionAlreadyInstalled version
        when (not ghcAlreadyInstalled) $ do
            if noInstall then
                throwVabalErrorIO "Required GHC version is not available on the system."
            else do
                res <- runExternalProcess "ghcup" ["install", version]
                case res of
                    ExitFailure _ -> throwVabalErrorIO "Error while installing ghc."
                    ExitSuccess   -> return ()

        home <- getHomeDirectory
        return . CustomLocation $ home </> ".ghcup" </> "ghc" </> version </> "bin" </> "ghc"

