module NixBackend (nixBackend) where

import Distribution.Types.Version
import Distribution.Types.GenericPackageDescription
import GhcDatabase
import System.Process
import System.Exit

import Backend
import VabalError

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Distribution.Parsec.Class

import System.FilePath

import Utils

unableToReadNixEnvOutputError :: a
unableToReadNixEnvOutputError = throwVabalError "Could not parse nix output."

isCached :: [String] -> Bool
isCached [status, _, _] = case status of
                            (_:'P':_) -> True
                            _         -> False
isCached              _ = unableToReadNixEnvOutputError -- This should be unreachable

isGhc :: [String] -> Bool
isGhc [_, _, attr] = take 4 attr == "ghc-"
isGhc _            = unableToReadNixEnvOutputError -- This should be unreachable

getVersion :: [String] -> String
getVersion [_, _, attr] = takeWhile (/= '-') . drop 4 $ attr
getVersion _            = unableToReadNixEnvOutputError -- This should be unreachable

-- Dead simple check that nix didn't change the output format
isValidPackageDescr :: [String] -> Bool
isValidPackageDescr descr = length descr == 3

nixGetInstalledGhcs :: IO [Version]
nixGetInstalledGhcs = do
     packages <- lines <$> readProcess "nix-env" [ "-qas"
                                                 , "-A"
                                                 , "nixpkgs.haskell.compiler"
                                                 , "--attr-path" ] ""

     let packages' = map words packages
     if all isValidPackageDescr packages' then do
         let cachedGhcs = map getVersion $ filter (\x -> isGhc x && isCached x) packages'
         return $ map (fromMaybe unableToReadNixEnvOutputError . simpleParsec) cachedGhcs
     else
         throwVabalErrorIO "Could not parse nix output."

makeCabal2NixFlags :: FlagAssignment -> String
makeCabal2NixFlags ass = intercalate " " $ map renderFlag $ unFlagAssignment ass
    where renderFlag :: (FlagName, Bool) -> String
          renderFlag (name, True) = "-f+" ++ unFlagName name
          renderFlag (name, False) = "-f-" ++ unFlagName name

nixSetupEnv :: EnvParams
            -> GhcDatabase
            -> Bool
            -> IO (NonEmptyList CabalOption)
nixSetupEnv envParams installedGhcs noInstall = do
    let ghcVer = envGhcVersion envParams
    if not (hasGhcVersion installedGhcs ghcVer) && noInstall then
        throwVabalErrorIO "Required GHC version is not available on the system."
    else do
        let projectDir = takeDirectory (envCabalFilePath envParams)
        let flags      = makeCabal2NixFlags (envFlags envParams)
        let nixGhcPkg = "ghc" ++ foldMap show (versionNumbers ghcVer)
        let nixShellScript = "cabal2nix --shell --compiler ghc-"
                             ++ prettyPrintVersion ghcVer
                             ++ " "
                             ++ flags
                             ++ " "
                             ++ "'"
                             ++ projectDir
                             ++ "' | sed -e 's/compiler ? \\\"default\\\"/compiler ? \\\""
                             ++ nixGhcPkg
                             ++ "\\\"/' > shell.nix"

        exitCode <- runExternalProcess "nix-shell" ["-p", "cabal2nix", "--run", nixShellScript]
        case exitCode of
            ExitFailure _ -> throwVabalErrorIO "Error while running cabal2nix inside nix-shell."
            ExitSuccess   -> return $ singleton "--enable-nix"

nixBackend :: Backend
nixBackend = Backend
           { getInstalledGhcs = nixGetInstalledGhcs
           , setupEnv         = nixSetupEnv
           }
