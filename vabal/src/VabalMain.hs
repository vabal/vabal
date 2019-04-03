module VabalMain where

import ArgumentParsers
import MetadataManager
import VabalContext

import GhcDatabase

import Distribution.Types.GenericPackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity
import Options.Applicative

import System.Directory
import System.FilePath
import System.Process
import System.Exit (ExitCode(..))

import VabalError

import CabalFileRetriever

import GhcupProgram

import PackageSolver
import UserInterface

import Distribution.Version

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.List (groupBy, uncons)

import qualified Data.Set as S

data VabalMainArguments = VabalMainArguments
               { versionSpecification :: VersionSpecification
               , configFlags          :: FlagAssignment
               , cabalFile            :: Maybe FilePath
               , noInstallFlag        :: Bool
               , alwaysNewestFlag     :: Bool
               , accurancyLevel       :: AccurancyLevel
               }
               deriving(Show)

mainArgumentsParser :: Parser VabalMainArguments
mainArgumentsParser = VabalMainArguments
                   <$> versionSpecificationOptions
                   <*> flagsOption
                   <*> cabalFileOption
                   <*> noInstallSwitch
                   <*> alwaysNewestSwitch
                   <*> accurancySwitches

mainProgDesc :: String
mainProgDesc = "Finds a version of GHC that is compatible with \
               \ the constraints imposed on base package found \
               \ in the cabal file analyzed, \
               \ then uses ghcup to obtain it (possibly downloading it). \
               \ Finally it executes the command specified after '--' \
               \ feeding it the options to use the obtained GHC compiler. \
               \ If no command is specified after '--', echo is used. \
               \ WARNING: Probably this is not what you want to use, \
               \ See \"vabal configure --help\" for info about how to \
               \ directly configure your project to use the found GHC compiler."

makeVabalContext :: IO VabalContext
makeVabalContext = do
    ghcMetadataDir <- getGhcMetadataDir
    let ghcMetadataPath = ghcMetadataDir </> ghcMetadataFilename
    ghcDb <- readGhcDatabase ghcMetadataPath
    installedGhcs <- S.fromList <$> getInstalledGhcs

    return $ VabalContext installedGhcs ghcDb

-- Intersect all compatible versions for all packages,
-- we want all of them to be compilable
compatibleVersionsFor :: FlagAssignment
                      -> VabalContext
                      -> [GenericPackageDescription]
                      -> S.Set Version
compatibleVersionsFor flags ctx pkgDescrs =
    case map (analyzePackage flags (ghcDatabase ctx)) pkgDescrs of
        [] -> mempty
        (x:xs) -> foldr S.intersection x xs

runSolver :: VabalMainArguments
          -> FlagAssignment
          -> VabalContext
          -> [GenericPackageDescription]
          -> Version
runSolver args flags ctx pkgDescrs
    | S.null compatibleVersions = throwVabalError "Could not solve constraints."
    | alwaysNewestFlag args     = newestCompatibleVersion
    | otherwise =
            let availableCompatibleVersions = S.intersection
                                              (availableGhcs ctx)
                                              compatibleVersions

            in fromMaybe newestCompatibleVersion
             $ S.lookupMax availableCompatibleVersions

    where compatibleVersions = compatibleVersionsFor flags ctx pkgDescrs
          newestCompatibleVersion = S.findMax compatibleVersions




-- Check whether the provided ghc version works for configuring the package
validateConfiguration :: Version
                      -> Maybe FilePath
                      -> FlagAssignment
                      -> IO Bool
validateConfiguration ver cabalFilePath flags = do
    homeDir <- getHomeDirectory
    let ghcLocation = homeDir </> ".vabal" </> "fake-ghc" </> prettyPrintVersion ver </> "ghc"
    let flagsOutput = unwords . map showFlagValue $ unFlagAssignment flags
    let packageTarget = case cabalFilePath of
                          Nothing -> ["all"] -- We want to consider all the packages
                          Just path -> ["--cabal-file", path]

    let cabalProcess = (proc "cabal" ([ "v2-build", "--dry-run"
                                      , "--flags", flagsOutput
                                      , "--with-compiler", ghcLocation
                                      , "-vnormal+nowrap"
                                      ]
                                      ++ packageTarget
                                      )) { std_out = CreatePipe, std_err = CreatePipe }

    (_, _, _, cabalProcHandle) <- createProcess cabalProcess
    exitCode <- waitForProcess cabalProcHandle

    return $ exitCode == ExitSuccess


runDeepSolver :: Bool
              -> PackageSpec
              -> VabalMainArguments
              -> FlagAssignment
              -> VabalContext
              -> [GenericPackageDescription]
              -> IO Version

runDeepSolver superAccurate cabalFiles args flags ctx pkgDescrs
    | S.null compatibleVersions = throwVabalErrorIO "Could not solve constraints."
    | otherwise = do
            let versions = if alwaysNewestFlag args then
                               S.toDescList compatibleVersions
                           else S.toDescList availableCompatibleVersions
                                ++ S.toDescList unavailableCompatibleVersions

            -- milestone versions
            let milestonedVersions = map head
                                   $ groupBy (\v1 v2 -> metadataForGhc (ghcDatabase ctx) v1 == metadataForGhc (ghcDatabase ctx) v2) versions


            v <- if superAccurate then -- Try all versions if we must be super accurate
                     findFirstSuccessfulVersion versions
                 else findFirstSuccessfulVersion milestonedVersions

            case v of
                Nothing -> throwVabalErrorIO "Could not find a ghc that solves the constraints."
                Just res -> return res

    where -- When not using a project we want to tell cabal which is the cabal file to use
          cabalFilePath = case cabalFiles of
                             ProjectSpec _ -> Nothing
                             CabalFile c   -> Just c

          compatibleVersions = compatibleVersionsFor flags ctx pkgDescrs
          availableCompatibleVersions = S.intersection (availableGhcs ctx) compatibleVersions
          unavailableCompatibleVersions  = S.difference compatibleVersions (availableGhcs ctx)

          findFirstSuccessfulVersion [] = return Nothing
          findFirstSuccessfulVersion (v:vs) = do
              putStrLn $ "Trying: ghc " ++ prettyPrintVersion v
              res <- validateConfiguration v cabalFilePath flags
              if res then
                  return $ Just v
              else findFirstSuccessfulVersion vs


vabalFindGhcVersion :: VabalMainArguments -> VabalContext -> IO Version
vabalFindGhcVersion args vabalCtx = do
    cabalFiles <- case cabalFile args of
                    Just cf -> return $ CabalFile cf
                    Nothing -> getCabalFiles

    pkgDescrs <- mapM (readGenericPackageDescription normal) (filesList cabalFiles)

    let flags = configFlags args

    let solver a f c p = case accurancyLevel args of
                             TryHard      -> runDeepSolver False cabalFiles a f c p
                             TrySuperHard -> runDeepSolver True cabalFiles a f c p
                             Normal       -> return $ runSolver a f c p

    case versionSpecification args of
        -- If the specified ghc version is known we can do some further checks
        GhcVersion ghcVer | hasGhcVersion (ghcDatabase vabalCtx) ghcVer -> do
            let checkPkg p = doesGhcVersionSupportPackage flags
                                                          (ghcDatabase vabalCtx)
                                                          p
                                                          ghcVer
            let res = all checkPkg pkgDescrs
            unless res $
                writeWarning "VabalWarning: The specified ghc version probably won't work."
            return ghcVer

        -- If the specified ghc version is not known, then we report the warning to the user
        -- and simply use that ghc version without further checks.
        GhcVersion ghcVer | otherwise -> do
            writeWarning "VabalWarning: The specified ghc version is not known by vabal, try running `vabal update` or report this."
            return ghcVer

        BaseVersion baseVer -> do
            -- Restrict the database to only those ghcs with the required baseVersion
            let db' = filterBaseVersionIn (ghcDatabase vabalCtx) (thisVersion baseVer)
            let ctx' = vabalCtx { ghcDatabase = db' }
            solver args flags ctx' pkgDescrs

        NoSpecification -> solver args flags vabalCtx pkgDescrs


vabalMain :: [String] -> VabalMainArguments -> IO ()
vabalMain cmd args = do
    metadataFound <- hasGhcMetadata

    if metadataFound then do
        vabalCtx <- makeVabalContext
        version <- vabalFindGhcVersion args vabalCtx
        writeMessage $ "Selected GHC version: " ++ prettyPrintVersion version
        ghcLocation <- requireGHC (availableGhcs vabalCtx) version (noInstallFlag args)

        -- Generate options determined by vabal
        let extraOptions = generateCabalOptions args ghcLocation

        -- If no command was specified, use echo
        let (commandName, commandArgs) = fromMaybe ("echo", []) $ uncons cmd


        let procDesc = proc commandName (commandArgs ++ extraOptions)
        (_, _, _, procHandle) <- createProcess procDesc
        exitCode <- waitForProcess procHandle
        case exitCode of
            ExitFailure _ -> throwVabalErrorIO "Error while running specified command."
            ExitSuccess   -> return ()

    else
        throwVabalErrorIO "Ghc metadata not found, run `vabal update` to download it."


generateCabalOptions :: VabalMainArguments -> FilePath -> [String]
generateCabalOptions args ghcLocation =
    let flagsOutput = unwords
              . map showFlagValue $ unFlagAssignment (configFlags args)

        outputGhcLocationArg = ["-w", ghcLocation]
        outputFlagsArg = if null flagsOutput then
                            []
                         else
                           -- we don't escape flags because we are sure no invalid
                           -- sequence is in them, since otherwise they weren't
                           -- parsed when passed as arguments
                           ["--flags", "'" ++ flagsOutput ++ "'"]

        outputCabalFile = case cabalFile args of
                             Nothing -> []
                             Just cabalFilePath -> ["--cabal-file", cabalFilePath]

    in outputGhcLocationArg ++ outputFlagsArg ++ outputCabalFile
