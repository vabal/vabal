{-# LANGUAGE OverloadedStrings #-}
module GhcDatabase where

import Distribution.Version

import Distribution.Parsec.Class

import Data.Csv

import Data.Foldable (toList)

import qualified Data.ByteString.Lazy as B

import qualified Data.Map.Strict as M
import qualified Data.Set as S


type GhcVersion = Version
type BaseVersion = Version


data GhcMetadata = GhcMetadata
                 { baseVersion     :: Version
                 , minCabalVersion :: Version
                 }

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


type GhcDatabase = M.Map Version GhcMetadata

parseGhcDatabase :: B.ByteString -> Either String GhcDatabase
parseGhcDatabase contents = do
    (_, entries) <- decodeByName contents
    return . M.fromList . map unwrapMetadataEntry $ toList entries

filterGhcVersions :: GhcDatabase -> S.Set Version -> GhcDatabase
filterGhcVersions = M.restrictKeys

hasGhcVersion :: GhcDatabase -> Version -> Bool
hasGhcVersion s v = v `M.member` s


isVersionInRange :: VersionRange -> Version -> Bool
isVersionInRange = flip withinRange

newest :: GhcDatabase -> Maybe (Version, GhcMetadata)
newest = M.lookupMax

entriesWithBaseVersionIn :: GhcDatabase -> VersionRange -> GhcDatabase
entriesWithBaseVersionIn db vr = M.filter (isVersionInRange vr . baseVersion) db

entriesCompatibleWithCabalVersionRange :: GhcDatabase -> VersionRange -> GhcDatabase
entriesCompatibleWithCabalVersionRange db vr =
    M.filter (not . isNoVersion . intersectVersionRanges vr . orLaterVersion . minCabalVersion) db

metadataForGhc :: GhcDatabase -> Version -> Maybe GhcMetadata
metadataForGhc db v = M.lookup v db

baseVersionForGhc :: GhcDatabase -> Version -> Maybe Version
baseVersionForGhc db v = baseVersion <$> metadataForGhc db v

cabalLibRangeForGhc :: GhcDatabase -> Version -> Maybe VersionRange
cabalLibRangeForGhc db v = orLaterVersion . minCabalVersion <$> metadataForGhc db v

