{-# LANGUAGE OverloadedStrings #-}
{- | This module contains the "GhcDatabase", which is a Map-like type associating to each
     ghc version its "GhcMetadata" informations,
     and utility functions to to manipulate and query it.
-}
module GhcDatabase
( GhcMetadata(..)
, GhcDatabase
, defaultGhcDatabaseURL
-- * Conversion from/to List
, dbFromList
, dbToList
, ghcVersions
-- * Queries
, isEmpty
, metadataForGhc
, baseVersionForGhc
, cabalLibRangeForGhc
, hasGhcVersion
, newest
-- * Parse from CSV file
, parseGhcDatabase
-- * Filter database entries
, filterGhcVersions
, excludeGhcVersions
, filterBaseVersionIn
, filterMinCabalVersionIn
) where

import           Distribution.Version
import           Distribution.Parsec
import           Data.Csv
import           Data.Foldable (toList)

import qualified Data.ByteString.Lazy as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | The URL from which you can download a ready to be used "GhcDatabase"
defaultGhcDatabaseURL :: String
defaultGhcDatabaseURL = "https://raw.githubusercontent.com/vabal/vabal-ghc-metadata/master/vabal-ghc-metadata.csv"

-- | Metadata associated to a GHC version
data GhcMetadata = GhcMetadata
                 { baseVersion     :: Version -- ^ base version supported by this GHC
                 , minCabalVersion :: Version -- ^ minimum version of Cabal that supports this GHC
                 }
                 deriving(Eq)

newtype MetadataEntry = MetadataEntry { unwrapMetadataEntry :: (Version, GhcMetadata) }

instance FromNamedRecord MetadataEntry  where
    parseNamedRecord r = do
        field1  <- r .: "ghcVersion"
        ghcVer  <- maybe (fail "Expected version") return $ simpleParsec field1
        field2  <- r .: "baseVersion"
        baseVer <- maybe (fail "Expected version") return $ simpleParsec field2
        field3 <- r .: "minCabalVersion"
        minCabalVer <- maybe (fail "Expected version") return $ simpleParsec field3
        return $ MetadataEntry (ghcVer, GhcMetadata baseVer minCabalVer)


-- | Map associating to each GHC version its "GhcMetadata"
newtype GhcDatabase = GhcDatabase { unwrapDb :: M.Map Version GhcMetadata }

-- | /O(n*log n)/. Create a "GhcDatabase" from a list key-value pairs.
-- 
-- The key is the ghc version,
-- the value is the "GhcMetadata" for that version
dbFromList :: [(Version, GhcMetadata)] -> GhcDatabase
dbFromList = GhcDatabase . M.fromList

-- | /O(n)/. Convert the "GhcDatabase" to a list of key-value pairs
dbToList :: GhcDatabase -> [(Version, GhcMetadata)]
dbToList = M.toList . unwrapDb

-- | /O(n)/. Get all ghc versions contained in the database
ghcVersions :: GhcDatabase -> S.Set Version
ghcVersions = M.keysSet . unwrapDb

-- | /O(1)/. Check whether the "GhcDatabase" is empty
isEmpty :: GhcDatabase -> Bool
isEmpty = M.null . unwrapDb

-- | /O(log n)/. Get newest GHC (and its metadata) found in the database
newest :: GhcDatabase -> Maybe (Version, GhcMetadata)
newest = M.lookupMax . unwrapDb

-- | /O(log n)/. Get "GhcMetadata" for a GHC version, if it's in the database
metadataForGhc :: GhcDatabase -> Version -> Maybe GhcMetadata
metadataForGhc (GhcDatabase db) v = M.lookup v db

-- | /O(log n)/. Get base version for a GHC version, if it's in the database
baseVersionForGhc :: GhcDatabase -> Version -> Maybe Version
baseVersionForGhc db v = baseVersion <$> metadataForGhc db v

-- | /O(log n)/. Get Supported Cabal version range for a GHC version, if it's in the databasr
cabalLibRangeForGhc :: GhcDatabase -> Version -> Maybe VersionRange
cabalLibRangeForGhc db v = orLaterVersion . minCabalVersion <$> metadataForGhc db v

-- | Parse a "GhcDatabase" from a csv string.
-- The format of the csv string must be the following:
--
-- The first line is the header, it is expected to contain these three columns: ghcVersion, baseVersion, minCabalVersion
-- Each line must at least contain values for those three columns.
-- 
-- A simple example:
--
-- > ghcVersion,baseVersion,minCabalVersion
-- > 8.6.3,4.12.0.0,2.4
parseGhcDatabase :: B.ByteString -> Either String GhcDatabase
parseGhcDatabase contents = do
    (_, entries) <- decodeByName contents
    return . GhcDatabase . M.fromList . map unwrapMetadataEntry $ toList entries

-- | /O(m*log(n/m + 1)), m <= n/. Get a restricted database with only a set of GHC versions
filterGhcVersions :: GhcDatabase -> S.Set Version -> GhcDatabase
filterGhcVersions (GhcDatabase db) = GhcDatabase . M.restrictKeys db

-- | /O(m*log(n/m + 1)), m <= n/. Exclude all ghc versions in the set from the database
excludeGhcVersions :: GhcDatabase -> S.Set Version -> GhcDatabase
excludeGhcVersions (GhcDatabase db) = GhcDatabase . M.withoutKeys db

-- | /O(log n)/. Check whether the database contains a GHC version
hasGhcVersion :: GhcDatabase -> Version -> Bool
hasGhcVersion (GhcDatabase s) v = v `M.member` s


isVersionInRange :: VersionRange -> Version -> Bool
isVersionInRange = flip withinRange

-- | /O(n)/. Filter database entries with base version in the given "VersionRange"
filterBaseVersionIn :: GhcDatabase -> VersionRange -> GhcDatabase
filterBaseVersionIn (GhcDatabase db) vr =
    GhcDatabase $ M.filter (isVersionInRange vr . baseVersion) db

-- | /O(n)/. Filter database entries with supported Cabal version range in the given "VersionRange"
filterMinCabalVersionIn :: GhcDatabase -> VersionRange -> GhcDatabase
filterMinCabalVersionIn (GhcDatabase db) vr =
    GhcDatabase $ M.filter (not . isNoVersion . intersectVersionRanges vr . orLaterVersion . minCabalVersion) db

