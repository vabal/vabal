module GhcDatabase
( newestGhcVersion            -- :: GhcToBaseMap -> GhcVersion
, ghcVersionsWithBaseIn       -- :: GhcToBaseMap -> VersionRange -> [GhcVersion]
, newestGhcVersionIn          -- :: GhcToBaseMap -> VersionRange -> Maybe GhcVersion
, baseVersionsIn              -- :: GhcToBaseMap -> VersionRange -> [BaseVersion]
, newestBaseVersionIn         -- :: GhcToBaseMap -> VersionRange -> Maybe GhcVersion
, baseVersionForGhc           -- :: GhcToBaseMap -> GhcVersion -> Maybe BaseVersion
) where

import Distribution.Version
import Data.Maybe (listToMaybe)
import Data.List (find)

import VabalContext

type GhcVersion = Version
type BaseVersion = Version


newestGhcVersion :: GhcToBaseMap -> GhcVersion
newestGhcVersion = ghcVersion . head . unwrapMap

isVersionInRange :: VersionRange -> Version -> Bool
isVersionInRange = flip withinRange

entriesWithBaseVersionIn :: GhcToBaseMap -> VersionRange -> [GhcMetadata]
entriesWithBaseVersionIn gtb vr =
    filter (isVersionInRange vr . baseVersion) (unwrapMap gtb)

-- The GhcVersions are in decreasing order
ghcVersionsWithBaseIn :: GhcToBaseMap -> VersionRange -> [GhcVersion]
ghcVersionsWithBaseIn gtb vr = ghcVersion <$> entriesWithBaseVersionIn gtb vr

newestGhcVersionIn :: GhcToBaseMap -> VersionRange -> Maybe GhcVersion
newestGhcVersionIn gtb = listToMaybe . ghcVersionsWithBaseIn gtb

-- The BaseVersions are in decreasing order
baseVersionsIn :: GhcToBaseMap -> VersionRange -> [BaseVersion]
baseVersionsIn gtb vr = baseVersion <$> entriesWithBaseVersionIn gtb vr

newestBaseVersionIn :: GhcToBaseMap -> VersionRange -> Maybe BaseVersion
newestBaseVersionIn gtb = listToMaybe . baseVersionsIn gtb

baseVersionForGhc :: GhcToBaseMap -> GhcVersion -> Maybe BaseVersion
baseVersionForGhc gtb v = baseVersion <$> find ((v ==) . ghcVersion) (unwrapMap gtb)
