{-# LANGUAGE OverloadedStrings #-}
module VabalContext where
    
import Distribution.Version
import Distribution.Parsec.Class

import Data.Csv
import Data.List (find, sortOn)
import Data.Maybe (isJust)
import Data.Ord (Down(..))

import Data.Foldable (toList)

import qualified Data.ByteString.Lazy as B

data GhcMetadata = GhcMetadata
                 { ghcVersion  :: Version
                 , baseVersion :: Version
                 }

instance Eq GhcMetadata where
    (GhcMetadata g _) == (GhcMetadata g' _) = g == g'

instance Ord GhcMetadata where
    compare (GhcMetadata g _) (GhcMetadata g' _) = compare g g'

instance FromNamedRecord GhcMetadata where
    parseNamedRecord r = do
        field1  <- r .: "ghcVersion"
        ghcVer  <- maybe (fail "Expected version") return $ simpleParsec field1
        field2  <- r .: "baseVersion"
        baseVer <- maybe (fail "Expected version") return $ simpleParsec field2
        return $ GhcMetadata ghcVer baseVer


newtype GhcToBaseMap = GhcToBaseMap { unwrapMap :: [GhcMetadata] }

emptyMap :: GhcToBaseMap
emptyMap = GhcToBaseMap []

readGhcToBaseMap :: B.ByteString -> Either String GhcToBaseMap
readGhcToBaseMap contents = do
    (_, entries) <- decodeByName contents
    -- Sort them in descending order, newest first
    return . GhcToBaseMap . sortOn Down $ toList entries

-- get a submap containing only specified ghc versions
subMap :: GhcToBaseMap -> [Version] -> GhcToBaseMap
subMap (GhcToBaseMap m) versions = GhcToBaseMap $
    filter (\e -> ghcVersion e `elem` versions) m

hasGhcVersion :: GhcToBaseMap -> Version -> Bool
hasGhcVersion (GhcToBaseMap m) v = isJust $
   find (\e -> ghcVersion e == v) m

data VabalContext = VabalContext
                  { availableGhcs   :: GhcToBaseMap
                  , allGhcInfo      :: GhcToBaseMap
                  , alwaysNewestGhc :: Bool
                  }
