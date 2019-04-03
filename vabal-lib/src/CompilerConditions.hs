module CompilerConditions
( genCompilerAssignments
, GhcAssignment
)  where

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
import Distribution.Types.CondTree
import Distribution.Types.Condition
import Distribution.Types.GenericPackageDescription

import Data.List (nub)
import Data.Maybe (mapMaybe, maybeToList)

import GhcDatabase

-- It represents a condition on GHC version
type CompilerVersionRangeCondition = VersionRange

-- We represent a single assignment for a CompilerVersionRangeCondition
-- as a VersionRange (that can make the condition either true or false).
--
-- Therefore a truth assignment for a list of CompilerVersionRangeConditions
-- is the intersection of a list of VersionRanges
type VersionRangeConditionTruthAssignment = VersionRange

-- A GhcAssignment is a pair of:
-- - base version constraints
-- - version of a ghc that fullfills those constraints
type GhcAssignment = (VersionRange, Version)



-- | Get all compiler conditional conditions from the package description
compilerConditions :: CompilerFlavor -> GenericPackageDescription -> [CompilerVersionRangeCondition]
compilerConditions comp pkgDescr = concat
                       [ concatMap extractCompilerConditions $ maybeToList (condLibrary pkgDescr)
                       , concatMap (extractCompilerConditions . snd) $ condSubLibraries pkgDescr
                       , concatMap (extractCompilerConditions . snd) $ condForeignLibs pkgDescr
                       , concatMap (extractCompilerConditions . snd) $ condExecutables pkgDescr
                       , concatMap (extractCompilerConditions . snd) $ condTestSuites pkgDescr
                       , concatMap (extractCompilerConditions . snd) $ condBenchmarks pkgDescr
                       ]
    -- TODO: Only extract interesting compiler conditions, i.e. those affecting 'base' and 'Cabal' dependencies
    where extractCompilerConditions :: CondTree ConfVar c a -> [CompilerVersionRangeCondition]
          extractCompilerConditions =
                  foldMap (analyzeCond . condBranchCondition) . condTreeComponents

              where analyzeCond :: Condition ConfVar -> [CompilerVersionRangeCondition]
                    analyzeCond (Lit _) = []
                    analyzeCond (Var (Impl c vr)) = [vr | c == comp]
                    analyzeCond (Var _)             = []
                    analyzeCond (CNot c) = analyzeCond c
                    analyzeCond (COr c1 c2) = analyzeCond c1 ++ analyzeCond c2
                    analyzeCond (CAnd c1 c2) = analyzeCond c1 ++ analyzeCond c2


makeVersionRangeConditionTruthAssignment :: [VersionRange]
                                         -> VersionRangeConditionTruthAssignment
makeVersionRangeConditionTruthAssignment = foldr intersectVersionRanges anyVersion

-- | Find the complementary of a version range
complementaryVersionRange :: VersionRange -> VersionRange
complementaryVersionRange = foldVersionRange noVersion
                                             notThisVersion
                                             orEarlierVersion
                                             orLaterVersion
                                             intersectVersionRanges
                                             unionVersionRanges


-- Generate all possible truth assignments for the given list of conditions
allTruthAssignments :: [CompilerVersionRangeCondition] -> [VersionRangeConditionTruthAssignment]
allTruthAssignments vars =
    let vars' = map (\v -> [v, complementaryVersionRange v]) vars
    in map makeVersionRangeConditionTruthAssignment $ foldr addAssignment [[]] vars'

    where addAssignment :: [CompilerVersionRangeCondition]
                        -> [[VersionRange]]
                        -> [[VersionRange]]
          addAssignment alternatives assignments = do
              newAssignment <- alternatives
              otherAssignments <- assignments
              return (newAssignment : otherAssignments)

truthAssignmentToGhcVersion :: GhcDatabase
                            -> VersionRangeConditionTruthAssignment
                            -> Maybe Version
truthAssignmentToGhcVersion db vr = fst <$> newest (filterBaseVersionIn db vr)

truthAssignmentToBaseVersionRange :: GhcDatabase
                                  -> VersionRangeConditionTruthAssignment
                                  -> VersionRange
truthAssignmentToBaseVersionRange db versionRangeAssignment =
    let baseVersions = map (thisVersion . baseVersion . snd)
                     . dbToList
                     $ filterBaseVersionIn db versionRangeAssignment
    in foldr unionVersionRanges noVersion baseVersions


truthAssignmentToGhcAssignment :: GhcDatabase
                               -> VersionRangeConditionTruthAssignment
                               -> Maybe GhcAssignment
truthAssignmentToGhcAssignment db ass = do
    ghcVer <- truthAssignmentToGhcVersion db ass
    let baseVerRange = truthAssignmentToBaseVersionRange db ass
    return (baseVerRange, ghcVer)


ghcAssignments :: GhcDatabase
               -> [CompilerVersionRangeCondition]
               -> [GhcAssignment]
ghcAssignments db =
    mapMaybe (truthAssignmentToGhcAssignment db) -- Ignore impossible constraints
  . allTruthAssignments
  . nub

{-# ANN ghcAssignments "HLint: ignore Fuse mapMaybe/map" #-}

-- | returns all compiler assignments needed to explore all possible package configurations
genCompilerAssignments :: GhcDatabase               -- ^ The database containing metadata for ghc versions to consider
                       -> GenericPackageDescription -- ^ The package description from which to extract compiler conditions
                       -> [GhcAssignment]           -- ^ all compiler assignments needed to explore all package configurations
genCompilerAssignments db = ghcAssignments db
                          . compilerConditions GHC

