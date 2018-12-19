module GhcDatabase
( newestGhcVersion    -- :: GhcVersion
, ghcVersionsIn       -- :: VersionRange -> [GhcVersion]
, newestGhcVersionIn  -- :: VersionRange -> Maybe GhcVersion
, baseVersionsIn      -- :: VersionRange -> [BaseVersion]
, newestBaseVersionIn -- :: VersionRange -> Maybe GhcVersion
, baseVersionForGhc   -- :: GhcVersion -> Maybe BaseVersion
) where

import Distribution.Version
import Data.Maybe (listToMaybe)
import Data.List (find)

type GhcVersion = Version
type BaseVersion = Version


-- TODO: put this table in a separate external file
baseToGhcMap :: [(BaseVersion, GhcVersion)]
baseToGhcMap =
    [ (mkVersion [4,12,0,0], mkVersion [8,6,3])
    , (mkVersion [4,12,0,0], mkVersion [8,6,2])
    , (mkVersion [4,12,0,0], mkVersion [8,6,1])
    , (mkVersion [4,11,1,0], mkVersion [8,4,4])
    , (mkVersion [4,11,1,0], mkVersion [8,4,3])
    , (mkVersion [4,11,1,0], mkVersion [8,4,2])
    , (mkVersion [4,11,0,0], mkVersion [8,4,1])
    , (mkVersion [4,10,1,0], mkVersion [8,2,2])
    , (mkVersion [4,10,0,0], mkVersion [8,2,1])
    , (mkVersion [4,9,1,0], mkVersion [8,0,2])
    , (mkVersion [4,9,0,0], mkVersion [8,0,1])
    , (mkVersion [4,8,2,0], mkVersion [7,10,3])
    , (mkVersion [4,8,1,0], mkVersion [7,10,2])
    , (mkVersion [4,8,0,0], mkVersion [7,10,1])
    , (mkVersion [4,7,0,2], mkVersion [7,8,4])
    , (mkVersion [4,7,0,1], mkVersion [7,8,3])
    , (mkVersion [4,7,0,0], mkVersion [7,8,1])
    , (mkVersion [4,6,0,1], mkVersion [7,6,2])
    , (mkVersion [4,6,0,0], mkVersion [7,6,1])
    , (mkVersion [4,5,1,0], mkVersion [7,4,2])
    , (mkVersion [4,5,0,0], mkVersion [7,4,1])
    , (mkVersion [4,4,1,0], mkVersion [7,2,2])
    , (mkVersion [4,4,0,0], mkVersion [7,2,1])
    , (mkVersion [4,3,1,0], mkVersion [7,0,2])
    , (mkVersion [4,3,0,0], mkVersion [7,0,1])
    , (mkVersion [4,2,0,2], mkVersion [6,12,3])
    , (mkVersion [4,2,0,1], mkVersion [6,12,2])
    , (mkVersion [4,2,0,0], mkVersion [6,12,1])
    , (mkVersion [4,1,0,0], mkVersion [6,10,2])
    , (mkVersion [4,0,0,0], mkVersion [6,10,1])
    ]

newestGhcVersion :: GhcVersion
newestGhcVersion = snd $ head baseToGhcMap

isVersionInRange :: VersionRange -> Version -> Bool
isVersionInRange = flip withinRange

-- TODO: Use binary search

-- The GhcVersions are in decreasing order
ghcVersionsIn :: VersionRange -> [GhcVersion]
ghcVersionsIn vr = snd <$> filter (isVersionInRange vr . fst) baseToGhcMap

newestGhcVersionIn :: VersionRange -> Maybe GhcVersion
newestGhcVersionIn = listToMaybe . ghcVersionsIn

-- The BaseVersions are in decreasing order
baseVersionsIn :: VersionRange -> [BaseVersion]
baseVersionsIn vr = fst <$> filter (isVersionInRange vr . fst) baseToGhcMap

newestBaseVersionIn :: VersionRange -> Maybe BaseVersion
newestBaseVersionIn = listToMaybe . baseVersionsIn

baseVersionForGhc :: GhcVersion -> Maybe BaseVersion
baseVersionForGhc v = fst <$> find ((v ==) . snd) baseToGhcMap
