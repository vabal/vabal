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
import Distribution.Version
import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.System
import Distribution.Compiler

import Data.Maybe (fromMaybe, maybeToList)

import qualified Data.ByteString as B

import Control.Monad (mplus, msum)

import VabalError

import GhcVersionChaser

import GhcDatabase
import VabalContext

unableToSatisfyConstraintsError :: a
unableToSatisfyConstraintsError = throwVabalError "Error, could not satisfy constraints."

cannotParseCabalFileError :: a
cannotParseCabalFileError = throwVabalError "Error while parsing cabal file."

makeCompilerInfo :: Version -> CompilerInfo
makeCompilerInfo  v = unknownCompilerInfo (CompilerId GHC v) NoAbiTag

isBase :: Dependency -> Bool
isBase (Dependency packageName _) = unPackageName packageName == "base"

extractBaseConstraints :: [Dependency] -> VersionRange
extractBaseConstraints deps =
    let constraints = depVerRange <$> filter isBase deps
    in foldr intersectVersionRanges anyVersion constraints

-- This is our dead simple dependency solver
-- that helps us choose a base version that satisfies constraints imposed externally
-- (i.e. by the user or by the ghc we are trying to use to configure the package)
queryDependency :: VersionRange -> Dependency -> Bool
queryDependency allowedBaseRange dep@(Dependency _ range)
   | isBase dep =  not . isNoVersion $ intersectVersionRanges range allowedBaseRange
   | otherwise  = True

constraintsForBase :: FlagAssignment
                   -> GenericPackageDescription
                   -> VersionRange
                   -> CompilerInfo
                   -> Maybe VersionRange
constraintsForBase flags pkgDescr otherBaseConstraints compiler =
    let finalizedPkgDescr = finalizePD flags
                            (ComponentRequestedSpec True True)
                            (queryDependency otherBaseConstraints)
                            buildPlatform
                            compiler
                            []
                            pkgDescr

    in case finalizedPkgDescr of
        Left _ -> Nothing -- throwVabalError "Error while analyzing the cabal file."
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
                baseConstraints = map extractBaseConstraints dependencies
            -- If constraints is empty, then no constraint is imposed on `base`
            -- and thus, any version of ghc is fine
            in Just $ foldr intersectVersionRanges anyVersion baseConstraints


analyzeCabalFileAllTargets :: FlagAssignment
                           -> VabalContext
                           -> Maybe Version
                           -> B.ByteString
                           -> Version
analyzeCabalFileAllTargets flags ctx  baseVersionConstraint cabalFile =
    case parseGenericPackageDescriptionMaybe cabalFile of
        Nothing -> cannotParseCabalFileError
        Just pkgDescr -> {-# SCC "vabal-core" #-}
            let otherBaseConstraints = maybe anyVersion thisVersion baseVersionConstraint

                -- Get all the ghc we should try
                -- Each candidate is a pair (BaseConstraints, CompilerInfo)
                candidates = map (fmap makeCompilerInfo) -- turn a GhcVersion in CompilerInfo
                           $ findGhcVersions (allGhcInfo ctx) otherBaseConstraints pkgDescr

                allBaseConstraints = map (uncurry $ constraintsForBase flags pkgDescr) candidates

                newestGhcCandidate =
                        -- Get first compatible compiler
                        msum $ map (>>= newestGhcVersionIn (allGhcInfo ctx)) allBaseConstraints

                availableGhcCandidate = 
                        -- Get first compatible compiler, amongst the available
                        msum $ map (>>= newestGhcVersionIn (availableGhcs ctx)) allBaseConstraints

                ghcCandidate =
                    if alwaysNewestGhc ctx then
                        newestGhcCandidate
                    else
                        -- If no available ghc is good, just find the newest
                        availableGhcCandidate `mplus` newestGhcCandidate

            in fromMaybe unableToSatisfyConstraintsError ghcCandidate

checkIfGivenVersionWorksForAllTargets :: FlagAssignment
                                      -> VabalContext
                                      -> B.ByteString
                                      -> Version
                                      -> Bool
checkIfGivenVersionWorksForAllTargets flags ctx cabalFile selectedGhcVersion =
    case parseGenericPackageDescriptionMaybe cabalFile of
        Nothing -> cannotParseCabalFileError
        Just pkgDescr ->
            let ghc = makeCompilerInfo selectedGhcVersion
            in case constraintsForBase flags pkgDescr anyVersion ghc of
                Nothing -> unableToSatisfyConstraintsError
                Just suggestedBaseVersionRange ->
                    -- Check if the selected ghc is one of the suggested ones
                    -- If we can't find the base version for the selected ghc,
                    -- then we don't recognize it and we say it may not be good.
                    -- Otherwise we check if its base version fits inside
                    -- the suggested base version range
                    maybe False (`withinRange` suggestedBaseVersionRange) $
                          baseVersionForGhc (allGhcInfo ctx) selectedGhcVersion

