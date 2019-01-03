module Backend where

import Distribution.Types.Version
import Distribution.Types.GenericPackageDescription
import GhcDatabase

type CabalOption = String

newtype NonEmptyList a = NonEmptyList { toList :: [a] }

cons :: a -> [a] -> NonEmptyList a
cons h t = NonEmptyList (h : t)

singleton :: a -> NonEmptyList a
singleton h = NonEmptyList [h]

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
                        -> IO (NonEmptyList CabalOption)
             }
