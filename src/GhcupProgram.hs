module GhcupProgram where

import ProcessUtils
import Data.List (intercalate)

import System.Process
import System.Exit
import System.Directory
import System.IO
import Control.Exception (SomeException, catch)
import System.FilePath

import Control.Monad (when)

import VabalError

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
requireGHC :: String -> IO (Maybe FilePath)
requireGHC version = do
    ghcInPathIsFine <- checkGhcInPath version
    if ghcInPathIsFine then
        return Nothing
    else do
        ghcAlreadyInstalled <- versionAlreadyInstalled version
        when (not ghcAlreadyInstalled) $ do
            res <- runExternalProcess "ghcup" ["install", version]
            case res of
                ExitFailure _ -> throwVabalErrorIO "Error while installing ghc."
                ExitSuccess   -> return ()

        home <- getHomeDirectory
        return . Just $ home </> ".ghcup" </> "ghc" </> version </> "bin" </> "ghc"

