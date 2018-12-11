{-# LANGUAGE OverloadedStrings #-}
module CabalAnalyzer (analyzeCabalFileAllTargets, checkIfGivenVersionWorksForAllTargets) where


import Distribution.Types.GenericPackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.Configuration
import Distribution.Types.PackageDescription
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.Library
import Distribution.Types.Executable
import Distribution.Types.ForeignLib
import Distribution.Types.BuildInfo
import Distribution.Types.SetupBuildInfo
import Distribution.Types.TestSuite
import Distribution.Types.Benchmark
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
import Data.Maybe (isJust, maybeToList, listToMaybe, catMaybes)
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

compilerInfoFromVersion :: Version -> CompilerInfo
compilerInfoFromVersion v = unknownCompilerInfo (CompilerId GHC v) NoAbiTag

-- TODO: Use binary search
getNewestGHCFromVersionRange :: VersionRange -> Maybe Version
getNewestGHCFromVersionRange vr = snd <$> find (versionInRange vr . fst) baseToGHCMap
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange

getAllGhcInVersionRange :: VersionRange -> [Version]
getAllGhcInVersionRange vr = snd <$> filter (versionInRange vr . fst) baseToGHCMap
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange



getBaseConstraints :: [Dependency] -> Maybe VersionRange
getBaseConstraints deps = depVerRange <$> find isBase deps
    where isBase (Dependency packageName _) = unPackageName packageName == "base"


getBaseConstraintForAllComponents :: FlagAssignment
                            -> CompilerInfo
                            -> GenericPackageDescription
                            -> VersionRange
getBaseConstraintForAllComponents flags compInfo pkgDescr =
    let res = finalizePD flags
              (ComponentRequestedSpec True True)
              (const True)
              buildPlatform
              compInfo
              []
              pkgDescr

    in case res of
        Left _ -> throwVabalError "Error while analyzing the cabal file."
        Right (pd, _) ->
            let setupDependencies = setupDepends <$> maybeToList (setupBuildInfo pd)

                projectDependencies = map targetBuildDepends $ concat
                                    [ libBuildInfo <$> maybeToList (library pd)
                                    , libBuildInfo <$> subLibraries pd
                                    , buildInfo <$> executables pd
                                    , foreignLibBuildInfo <$> foreignLibs pd
                                    , testBuildInfo <$> testSuites pd
                                    , benchmarkBuildInfo <$> benchmarks pd
                                    ]

                dependencies = setupDependencies ++ projectDependencies
                constraints = catMaybes $ map getBaseConstraints dependencies
            -- If constraints is empty, then no constraint is imposed on `base`
            -- and thus, any version of ghc is fine
            in foldr intersectVersionRanges anyVersion constraints



analyzeCabalFileAllTargets :: FlagAssignment -> FilePath -> IO Version
analyzeCabalFileAllTargets flags filepath = do
    pkgDescr <- readGenericPackageDescription normal filepath

    let newestGHC = compilerInfoFromVersion newestGHCVersion
    let product = getBaseConstraintForAllComponents flags newestGHC pkgDescr
    case getNewestGHCFromVersionRange product of
        Nothing -> throwVabalErrorIO "Error, could not satisfy constraints."
        Just res -> return res

checkIfGivenVersionWorksForAllTargets :: FlagAssignment -> FilePath -> Version -> IO Bool
checkIfGivenVersionWorksForAllTargets flags filepath ghcVersion = do
    pkgDescr <- readGenericPackageDescription normal filepath

    let compInfo = compilerInfoFromVersion ghcVersion
    let product = getBaseConstraintForAllComponents flags compInfo pkgDescr
    return $ ghcVersion `elem` getAllGhcInVersionRange product

