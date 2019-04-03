{- | This is the central package of vabal-lib,
     it contains the functions to analyze a cabal package and determine
     which versions of ghc can comply with the constraints
     imposed on the following dependencies:

     * base

     * Cabal (only constraints in the setup-depends stanza are important)
-}
module PackageSolver
( analyzePackage
, doesGhcVersionSupportPackage
) where

import           Distribution.Types.GenericPackageDescription
import           Distribution.PackageDescription.Configuration
import           Distribution.Types.PackageDescription
import           Distribution.Types.ComponentRequestedSpec
import           Distribution.Types.Library
import           Distribution.Types.Executable
import           Distribution.Types.ForeignLib
import           Distribution.Types.BuildInfo
import           Distribution.Types.SetupBuildInfo
import           Distribution.Types.TestSuite
import           Distribution.Types.Benchmark
import           Distribution.Version
import           Distribution.Types.Dependency
import           Distribution.Types.PackageName
import           Distribution.System
import           Distribution.Compiler
import           Data.Maybe (fromMaybe, maybeToList)
import           Control.Arrow (second)
import           CompilerConditions
import           GhcDatabase
import qualified Data.Set as S

makeCompilerInfo :: Version -> CompilerInfo
makeCompilerInfo v = unknownCompilerInfo (CompilerId GHC v) NoAbiTag

isBase :: Dependency -> Bool
isBase (Dependency packageName _) = unPackageName packageName == "base"

isCabalLib :: Dependency -> Bool
isCabalLib (Dependency packageName _) = unPackageName packageName == "Cabal"

extractConstraints :: (Dependency -> Bool) -> [Dependency] -> VersionRange
extractConstraints predicate deps =
    let constraints = depVerRange <$> filter predicate deps
    in foldr intersectVersionRanges anyVersion constraints

-- | This is our dead simple dependency solver
-- that helps us choose a base version that satisfies constraints imposed externally
-- (i.e. by the user or by the ghc we are trying to use to configure the package)
queryDependency :: VersionRange -> Dependency -> Bool
queryDependency allowedBaseRange dep@(Dependency _ range)
   | isBase dep =  not . isNoVersion $ intersectVersionRanges range allowedBaseRange
   | otherwise  = True

configurePackage :: FlagAssignment
                 -> VersionRange
                 -> CompilerInfo
                 -> GenericPackageDescription
                 -> Maybe PackageDescription
configurePackage flags allowedBaseRange compiler pkgDescr =
    let configuredPkg = finalizePD flags
                        (ComponentRequestedSpec True True)
                        (queryDependency allowedBaseRange)
                        buildPlatform
                        compiler
                        []
                        pkgDescr
    in case configuredPkg of
         Left _ -> Nothing
         Right (pd, _) -> Just pd


-- | Extracts all base constraints in the Package
constraintsForBase :: PackageDescription -> VersionRange
constraintsForBase pkgDescr =
    let setupDependencies = setupDepends <$> maybeToList (setupBuildInfo pkgDescr)

        projectDependencies = map targetBuildDepends $ concat
                            [ libBuildInfo <$> maybeToList (library pkgDescr)
                            , libBuildInfo <$> subLibraries pkgDescr
                            , buildInfo <$> executables pkgDescr
                            , foreignLibBuildInfo <$> foreignLibs pkgDescr
                            , testBuildInfo <$> testSuites pkgDescr
                            , benchmarkBuildInfo <$> benchmarks pkgDescr
                            ]

        dependencies = setupDependencies ++ projectDependencies
        baseConstraints = map (extractConstraints isBase) dependencies
    in foldr intersectVersionRanges anyVersion baseConstraints

-- | Extracts Cabal constraints in the setup-depends section of the Package
constraintsForCabalInSetupDepends :: PackageDescription -> VersionRange
constraintsForCabalInSetupDepends pkgDescr =
    let setupDependencies = setupDepends <$> maybeToList (setupBuildInfo pkgDescr)
        cabalConstraints = map (extractConstraints isCabalLib) setupDependencies
    in foldr intersectVersionRanges anyVersion cabalConstraints


-- | Given a full configuration (FlagAssignment + Compiler version range),
-- find ghc versions compatible with the constraints.
findCandidate :: FlagAssignment
              -> GenericPackageDescription
              -> GhcDatabase
              -> (VersionRange, CompilerInfo)
              -> S.Set Version
findCandidate flags pkgDescr db (vr, ci) = fromMaybe mempty $ do
    configuredPkg <- configurePackage flags vr ci pkgDescr
    let baseConstraints = constraintsForBase configuredPkg
    let cabalLibConstraints = constraintsForCabalInSetupDepends configuredPkg
    let db' = filterBaseVersionIn db baseConstraints
    let db'' = filterMinCabalVersionIn db' cabalLibConstraints
    return $ ghcVersions db''

-- | 'analyzePackage' uses the provided flag assignment
-- to configure the package description.
-- It returns a set of all ghc versions found in the "GhcDatabase" that satisfy
-- the base and Cabal (only Cabal in setup-depends is considered) constraints.
analyzePackage :: FlagAssignment              -- ^ Flags to apply when configuring the project
               -> GhcDatabase                 -- ^ Database containing metadata for all ghc versions to consider
               -> GenericPackageDescription   -- ^ The raw project description cabal file
               -> S.Set Version               -- ^ The versions of ghc that comply with the constraints of base and Cabal
analyzePackage flags ghcDb pkgDescr = {-# SCC "vabal-core" #-}
    let compilers = map (second makeCompilerInfo) -- second makeCompiler :: (VersionRange, Version) -> (VersionRange, CompilerInfo)
                  $ genCompilerAssignments ghcDb pkgDescr

    in mconcat $ map (findCandidate flags pkgDescr ghcDb) compilers


-- | 'doesGhcVersionSupportPackage' checks that the provided compiler version
-- is able to comply with the constraints of
-- base and Cabal (only Cabal in setup-depends is considered) of the package.
doesGhcVersionSupportPackage :: FlagAssignment              -- ^ Flags to apply when configuring the project
                             -> GhcDatabase                 -- ^ Database containing metadata for all known ghc versions
                             -> GenericPackageDescription   -- ^ The raw project description cabal file
                             -> Version                     -- ^ The version of ghc the user wants to use
                             -> Bool                        -- ^ Returns whether the version of ghc the user wants is fine
doesGhcVersionSupportPackage flags ghcDb pkgDescr selectedGhcVersion = fromMaybe False $ do
    let ghc = makeCompilerInfo selectedGhcVersion
    configuredPkg <- configurePackage flags anyVersion ghc pkgDescr
    -- Check if the selected ghc is one of the suggested ones
    -- If we can't find the base version for the selected ghc,
    -- then we don't recognize it and we say it may not be good.
    -- Otherwise we check if its base version fits inside
    -- the suggested base version range
    let suggestedBaseVersionRange = constraintsForBase configuredPkg
        suggestedCabalLibVersionRange = constraintsForCabalInSetupDepends configuredPkg
        selectedGhcBaseVersion = baseVersionForGhc ghcDb selectedGhcVersion
        selectedGhcCabalLibRange = cabalLibRangeForGhc ghcDb selectedGhcVersion

    
    baseVersionIsFine <- (`withinRange` suggestedBaseVersionRange)
                      <$> selectedGhcBaseVersion

    cabalVersionIsFine <- not
                     . isNoVersion
                     . intersectVersionRanges suggestedCabalLibVersionRange
                     <$> selectedGhcCabalLibRange

    return $ baseVersionIsFine && cabalVersionIsFine

