{-# LANGUAGE OverloadedStrings #-}
module CabalAnalyzer (analyzeCabalFileAllTargets, checkIfGivenVersionWorksForAllTargets) where


import Distribution.Types.GenericPackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Distribution.Version
import Distribution.Types.CondTree
import Distribution.Types.UnqualComponentName
import Distribution.Types.Dependency
import Distribution.Types.VersionRange
import Distribution.Types.PackageName
import Distribution.Types.Condition
import Distribution.System
import Distribution.Compiler


import Data.List (find, intercalate, partition)
import Data.Maybe (isJust, listToMaybe, catMaybes)
import Data.Bits (xor)

import Control.Monad (join)

import VabalError
import Control.Exception (bracket)
import FlagsUtils

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
    ]

newestGHCVersion :: Version
newestGHCVersion = snd $ last baseToGHCMap

-- TODO: Use binary search
getNewestGHCFromVersionRange :: VersionRange -> Maybe Version
getNewestGHCFromVersionRange vr = snd <$> find (versionInRange vr . fst) baseToGHCMap
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange

getAllGhcInVersionRange :: VersionRange -> [Version]
getAllGhcInVersionRange vr = snd <$> filter (versionInRange vr . fst) baseToGHCMap
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange

getFlagDefaultValue :: FlagName -> [Flag] -> Bool
getFlagDefaultValue name flags = case find (\f -> flagName f == name) flags of
    Nothing -> False -- This should be impossible, though
    Just flag -> flagDefault flag

resolveConfVar :: FlagAssignment -> [Flag] -> ConfVar -> Bool
resolveConfVar _  _ (OS os) = buildOS == os
resolveConfVar _ _ (Arch arch) = buildArch == arch
resolveConfVar flagsSet allFlags (Flag flag) = case lookupFlagAssignment flag flagsSet of
    Nothing -> getFlagDefaultValue flag allFlags
    Just val -> val

-- Pick the branch that guarantees the highest ghc version
resolveConfVar _ _ (Impl GHC versionRange) = withinRange newestGHCVersion versionRange

resolveConfVar _ _ _ = False


evalCondition :: FlagAssignment -> [Flag] -> Condition ConfVar -> Bool
evalCondition flagsSet allFlags (Var cv) = resolveConfVar flagsSet allFlags cv
evalCondition _ _ (Lit b)         = b
evalCondition flagsSet allFlags (CNot cond) = not (evalCondition flagsSet allFlags cond)
evalCondition flagsSet allFlags (COr cond1 cond2)  = evalCondition flagsSet allFlags cond1 || evalCondition flagsSet allFlags cond2
evalCondition flagsSet allFlags (CAnd cond1 cond2) = evalCondition flagsSet allFlags cond1 && evalCondition flagsSet allFlags cond2



-- Find the first base constraint resolving conditional statements
analyzeTarget :: FlagAssignment -> [Flag] -> CondTree ConfVar [Dependency] a -> Maybe VersionRange
analyzeTarget flagsSet allFlags deps = case getBaseConstraints (condTreeConstraints deps) of
                                          (Just baseVersion) -> Just baseVersion
                                          Nothing -> listToMaybe . catMaybes $ map analyzeConditionals (condTreeComponents deps)

    where analyzeConditionals :: CondBranch ConfVar [Dependency] a -> Maybe VersionRange
          analyzeConditionals branch = case evalCondition flagsSet allFlags (condBranchCondition branch) of
                                            True -> analyzeTarget flagsSet allFlags $ condBranchIfTrue branch
                                            False -> condBranchIfFalse branch >>= analyzeTarget flagsSet allFlags



getBaseConstraints :: [Dependency] -> Maybe VersionRange
getBaseConstraints deps = depVerRange <$> find isBase deps
    where isBase (Dependency packageName _) = unPackageName packageName == "base"


getBaseConstraintForAllComponents :: FlagAssignment
                            -> GenericPackageDescription
                            -> [Flag]
                            -> Maybe VersionRange
getBaseConstraintForAllComponents flags pkgDescr allFlags =
    let targetAnalyzer = analyzeTarget flags allFlags
        -- if there is a library, analyzeTarget
        -- This leverages the Monad instance for Maybe
        libBaseConstraint = condLibrary pkgDescr >>= targetAnalyzer
        subLibsBaseConstraints = map (targetAnalyzer . snd) (condSubLibraries pkgDescr)
        foreignLibsBaseConstraints = map (targetAnalyzer . snd) (condForeignLibs pkgDescr)
        executablesBaseConstraints = map (targetAnalyzer . snd) (condExecutables pkgDescr)
        testSuitesBaseConstraints = map (targetAnalyzer . snd) (condTestSuites pkgDescr)
        benchmarksBaseConstraints = map (targetAnalyzer . snd) (condBenchmarks pkgDescr)

        baseVersions = catMaybes $ libBaseConstraint : subLibsBaseConstraints
                                                     ++ foreignLibsBaseConstraints
                                                     ++ executablesBaseConstraints
                                                     ++ testSuitesBaseConstraints
                                                     ++ benchmarksBaseConstraints

    in case baseVersions of
        [] -> Nothing
        vrs -> Just $ foldr1 intersectVersionRanges vrs


isUnassignedFlag :: FlagAssignment -> Flag -> Bool
isUnassignedFlag flags f = case lookupFlagAssignment (flagName f) flags of
                               Nothing -> True
                               _       -> False

-- Generate all possible combination of flag assignments for the given set
allAssignments :: [Flag] -> [[Flag]]
allAssignments flags = let flags' = map (\f -> [f, flipFlag f]) flags
                       in foldr combine [[]] flags'

    where combine :: [Flag] -> [[Flag]] -> [[Flag]]
          combine alternatives accum = do
              val <- alternatives
              rest <- accum
              return (val : rest)

          flipFlag :: Flag -> Flag
          flipFlag flag = MkFlag (flagName flag)
                                 (flagDescription flag)
                                 (not $ flagDefault flag)
                                 (flagManual flag)


tryConfig :: FlagAssignment -> GenericPackageDescription -> [Flag] -> Maybe VersionRange
tryConfig flags pkgDescr next = getBaseConstraintForAllComponents flags pkgDescr next


firstValidConfig :: [Maybe a] -> Maybe a
firstValidConfig [] = Nothing
firstValidConfig (Nothing:rest) = firstValidConfig rest
firstValidConfig (c:_)          = c


analyzeAllTargets :: FlagAssignment -> FilePath -> (VersionRange -> Maybe a) -> IO (Maybe a)
analyzeAllTargets flags filepath analyzer = do
    res <- readGenericPackageDescription normal filepath

    let allFlags = genPackageFlags res

    let unassignedFlags = filter (isUnassignedFlag flags) allFlags

    -- nonManualUnassignedFlags are the flags we need to backtrack on,
    -- when finding a satisfying config
    let (nonManualUnassignedFlags, manualUnassignedFlags) = partition (not . flagManual) unassignedFlags

    let possibleFlagsConfigs = map (++ manualUnassignedFlags) $ allAssignments nonManualUnassignedFlags

    let product = firstValidConfig $
           map (\conf -> tryConfig flags res conf >>= analyzer) possibleFlagsConfigs

    return product



analyzeCabalFileAllTargets :: FlagAssignment -> FilePath -> IO Version
analyzeCabalFileAllTargets flags filepath = do
    product <- analyzeAllTargets flags filepath getNewestGHCFromVersionRange
    case product of
        Nothing -> throwVabalErrorIO "Error, could not satisfy constraints."
        Just product -> return product

checkIfGivenVersionWorksForAllTargets :: FlagAssignment -> FilePath -> Version -> IO Bool
checkIfGivenVersionWorksForAllTargets flags filepath ghcVersion = do
    res <- analyzeAllTargets flags filepath $ (\vr ->
                case ghcVersion `elem` getAllGhcInVersionRange vr of
                    True -> Just ghcVersion
                    False -> Nothing)

    return (isJust res)
