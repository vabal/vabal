module Sat (usefulGhcsForPkgDescr)  where

-- When there are `if impl(GHC)` conditionals in the package description,
-- we must find a suitable GHC version that makes it possible to solve constraints.
-- To do this, we follow the same method that cabal uses to find the flags assignment
-- that makes it possible to solve constraints,
-- i.e. we try all possible assignments until one succeeds.
--
-- Specifically, we pick all the `if impl(GHC)` conditions,
-- and we try all the possible truth assignments.
-- E.g. if there is `if impl(GHC > 6)` and somewhere else there is `if impl(GHC <= 8.4)`
-- We have two conditions:
-- - GHC > 6
-- - GHC <= 8.4
--
-- Now we try all 4 possible truth assignments.
-- The first one being:
-- GHC > 6 -> False (Meaning GHC <= 6)
-- GHC <= 8.4 -> False (Meaning GHC > 8.4)

-- Then we find a ghc version that matches the given constraints.
-- If none is found, this truth assignment can never happen, and therefore is ignored.
--
-- When a GHC version is found that matches the constraints, we also find
-- the associated constraints on base (we're going to need it later).
-- With GHC version and flag assignments,
-- we can fully configure the package and extract the base constraint.
-- Now there can be something odd happening:
-- If it turns out that a older (or newer) version of base is needed,
-- then we should provide a different GHC from the one we used to configure the package.
-- This may result in a failure in determining the dependencies,
-- because with a different GHC, one of the `if impl(GHC)` clauses may change it's truth value,
-- and this could enable different dependencies that can cause conflicts.
-- To prevent this, we impose further constraints on base,
-- i.e. we also require that base be in a version range that respects the given truth assignment,
-- this means that the corresponding GHC chosen
-- will not make any of the `if impl(GHC)` change its truth value from the assignment we're trying
-- and thus it won't add different dependencies from the ones we analyzed.

import Distribution.Version
import Distribution.Compiler
import Distribution.Types.VersionRange
import Distribution.Types.CondTree
import Distribution.Types.Condition
import Distribution.Types.GenericPackageDescription

import Data.Ord (comparing, Down(..))
import Data.List (nub, find, sortBy)
import Data.Maybe (catMaybes, maybeToList)
import Data.Function (on)
import Debug.Trace

-- TODO: put this table in a separate external file
baseToGHCMap :: [(Version, Version)]
baseToGHCMap = reverse
    [ (mkVersion [4,0,0,0], mkVersion [6,10,1])
    , (mkVersion [4,1,0,0], mkVersion [6,10,2])
    , (mkVersion [4,2,0,0], mkVersion [6,12,1])
    , (mkVersion [4,2,0,1], mkVersion [6,12,2])
    , (mkVersion [4,2,0,2], mkVersion [6,12,3])
    , (mkVersion [4,3,0,0], mkVersion [7,0,1])
    , (mkVersion [4,3,1,0], mkVersion [7,0,2])
    , (mkVersion [4,4,0,0], mkVersion [7,2,1])
    , (mkVersion [4,4,1,0], mkVersion [7,2,2])
    , (mkVersion [4,5,0,0], mkVersion [7,4,1])
    , (mkVersion [4,5,1,0], mkVersion [7,4,2])
    , (mkVersion [4,6,0,0], mkVersion [7,6,1])
    , (mkVersion [4,6,0,1], mkVersion [7,6,2])
    , (mkVersion [4,7,0,0], mkVersion [7,8,1])
    , (mkVersion [4,7,0,1], mkVersion [7,8,3])
    , (mkVersion [4,7,0,2], mkVersion [7,8,4])
    , (mkVersion [4,8,0,0], mkVersion [7,10,1])
    , (mkVersion [4,8,1,0], mkVersion [7,10,2])
    , (mkVersion [4,8,2,0], mkVersion [7,10,3])
    , (mkVersion [4,9,0,0], mkVersion [8,0,1])
    , (mkVersion [4,9,1,0], mkVersion [8,0,2])
    , (mkVersion [4,10,0,0], mkVersion [8,2,1])
    , (mkVersion [4,10,1,0], mkVersion [8,2,2])
    , (mkVersion [4,11,0,0], mkVersion [8,4,1])
    , (mkVersion [4,11,1,0], mkVersion [8,4,2])
    , (mkVersion [4,11,1,0], mkVersion [8,4,3])
    , (mkVersion [4,12,0,0], mkVersion [8,6,1])
    , (mkVersion [4,12,0,0], mkVersion [8,6,2])
    , (mkVersion [4,12,0,0], mkVersion [8,6,3])
    ]

type GhcVersionRange = VersionRange

type VersionRangeAssignment = [VersionRange]

getNewestGHCFromVersionRange :: VersionRange -> Maybe Version
getNewestGHCFromVersionRange vr = snd <$> find (versionInRange vr . fst) baseToGHCMap
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange

getAllBaseInVersionRange :: VersionRange -> VersionRange
getAllBaseInVersionRange vr = let allBase = fst <$> filter (versionInRange vr . fst) baseToGHCMap
                              in foldr unionVersionRanges noVersion $ map thisVersion allBase
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange

-- Find the complementary of a version range
complementaryVersionRange :: VersionRange -> VersionRange
complementaryVersionRange = foldVersionRange noVersion
                                             notThisVersion
                                             orEarlierVersion
                                             orLaterVersion
                                             intersectVersionRanges
                                             unionVersionRanges


genAllPossibleAssignments :: [GhcVersionRange] -> [VersionRangeAssignment]
genAllPossibleAssignments vars =
    let vars' = map (\v -> [v, complementaryVersionRange v]) vars
    in foldr combine [[]] vars'

    where combine :: [GhcVersionRange] -> [VersionRangeAssignment] -> [VersionRangeAssignment]
          combine alternatives accum = do
              val <- alternatives
              rest <- accum
              return (val : rest)

solveAssignment :: [VersionRangeAssignment] -> [(VersionRange, Version)]
solveAssignment vrss = let vrs = map (foldr intersectVersionRanges anyVersion) vrss
                       in catMaybes . map getInfo $ vrs

    where getInfo :: VersionRange -> Maybe (VersionRange, Version)
          getInfo vr = case getNewestGHCFromVersionRange vr of
                           Nothing -> Nothing
                           Just ghc -> Just (getAllBaseInVersionRange vr, ghc)

extractFromCondition :: Condition ConfVar -> [VersionRange]
extractFromCondition (Lit _) = []
extractFromCondition (Var (Impl GHC vr)) = [vr]
extractFromCondition (Var _)             = []
extractFromCondition (CNot c) = extractFromCondition c
extractFromCondition (COr c1 c2) = (extractFromCondition c1) ++ (extractFromCondition c2)
extractFromCondition (CAnd c1 c2) = (extractFromCondition c1) ++ (extractFromCondition c2)

extractFromTree :: CondTree ConfVar c a -> [VersionRange]
extractFromTree = foldMap (extractFromCondition . condBranchCondition) . condTreeComponents


extractFromPkgDescr :: GenericPackageDescription -> [VersionRange]
extractFromPkgDescr pkgDescr = traceShowId $
    concat [
        concatMap extractFromTree $ maybeToList (condLibrary pkgDescr)
        , concatMap (extractFromTree . snd) $ condSubLibraries pkgDescr
        , concatMap (extractFromTree . snd) $ condForeignLibs pkgDescr
        , concatMap (extractFromTree . snd) $ condExecutables pkgDescr
        , concatMap (extractFromTree . snd) $ condTestSuites pkgDescr
        , concatMap (extractFromTree . snd) $ condBenchmarks pkgDescr
    ]


usefulGhcs :: [GhcVersionRange] -> [(VersionRange, Version)]
usefulGhcs = sortBy (comparing Down `on` snd) . solveAssignment . genAllPossibleAssignments . nub


usefulGhcsForPkgDescr :: GenericPackageDescription -> [(VersionRange, Version)]
usefulGhcsForPkgDescr = usefulGhcs . extractFromPkgDescr
