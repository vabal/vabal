{-# LANGUAGE OverloadedStrings #-}
module CabalAnalyzer (analyzeCabalFileDefaultTarget) where


import qualified Cabal.Plan as P
    
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

import System.Exit
import System.Posix.Temp
import System.Directory
import System.Environment
import Data.List (find, intercalate)
import Data.Maybe (isJust, listToMaybe, catMaybes)
import Data.Bits (xor)

import qualified Data.Map.Strict as M

import VabalError
import ProcessUtils
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
newestGHCVersion = mkVersion [8,6,2]

-- TODO: Use binary search
getNewestGHCFromVersionRange :: VersionRange -> Maybe Version
getNewestGHCFromVersionRange vr = snd <$> find (versionInRange vr . fst) baseToGHCMap
    where versionInRange :: VersionRange -> Version -> Bool
          versionInRange = flip withinRange


prettyPrintVersion :: Version -> String
prettyPrintVersion ver = intercalate "." $ map show (versionNumbers ver)

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


analyzeCabalFileDefaultTargetShallow :: FlagAssignment -> FilePath -> IO VersionRange
analyzeCabalFileDefaultTargetShallow flags filepath = do
    res <- readGenericPackageDescription normal filepath
    let canDetermineDefaultTarget = isJust (condLibrary res) `xor` (not . null $ condExecutables res)

    if not canDetermineDefaultTarget then
        throwVabalErrorIO "Can't determine default target"
    else do
        let baseVersion = case condLibrary res of
                            Just lib -> analyzeTarget flags (genPackageFlags res) lib
                            Nothing  -> analyzeTarget flags (genPackageFlags res) (snd . head $ condExecutables res)

        case baseVersion of
            Nothing -> throwVabalErrorIO "Error, no base package found"
            Just baseVersion -> return baseVersion



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

analyzeCabalFileDefaultTargetDeep :: FlagAssignment -> FilePath -> IO VersionRange
analyzeCabalFileDefaultTargetDeep flags filepath = do
    -- We need absolute path of cabal file
    absoluteFilePath <- makeAbsolute filepath
    bracket (mkdtemp "/tmp/package-config-dir") removeDirectoryRecursive $ \tmpDir -> do
        withCurrentDirectory tmpDir $ do
            writeFile "cabal.project" $ "packages: " ++ absoluteFilePath ++ "\npackage base\n    flags: +integer-gmp"

            let cabalFlags = makeCabalArguments flags
            res <- runExternalProcess "cabal" $ ["new-configure", "--allow-boot-library-installs"] ++ cabalFlags
            case res of
                ExitFailure _ -> throwVabalErrorIO "Could not determine best base version."
                ExitSuccess   -> do
                    plan <- P.findAndDecodePlanJson (P.ProjectRelativeToDir tmpDir)
                    let deps = map (P.uPId . snd) $ M.toList (P.pjUnits plan)
                        isBasePackage (P.PkgId (P.PkgName name) _) = name == "base"

                    case find isBasePackage deps of
                        Nothing -> throwVabalErrorIO "Could not determine base version."
                        Just (P.PkgId _ (P.Ver v)) -> return (thisVersion (mkVersion v))


analyzeCabalFileDefaultTarget :: FlagAssignment -> FilePath -> IO String
analyzeCabalFileDefaultTarget flags filepath = do
    baseVersion <- analyzeCabalFileDefaultTargetShallow flags filepath
    case getNewestGHCFromVersionRange baseVersion of
        Nothing -> throwVabalErrorIO "Error, could not satisfy constraints"
        Just version -> return $ prettyPrintVersion version
