module Backend where

import Distribution.Types.Version
import Distribution.Types.GenericPackageDescription
import GhcDatabase

type CabalOption = String

-- Params that customize the resulting environment
data EnvParams = EnvParams
               { envFlags :: FlagAssignment
               , envCabalFilePath :: FilePath
               , envGhcVersion :: Version
               }

data Backend = Backend
             { getInstalledGhcs :: IO [Version]
             , setupEnv :: EnvParams
                        -> GhcDatabase
                        -> Bool
                        -> IO [CabalOption]
             }
