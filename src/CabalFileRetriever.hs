module CabalFileRetriever where

import System.Directory
import System.FilePath ((</>))
import VabalError

import Glob
import Project

import Data.Either (isRight, rights)
import Control.Monad (filterM)

import qualified Data.ByteString as B

findCabalFileIn :: FilePath -> IO FilePath
findCabalFileIn path = withCurrentDirectory path $ do
    let Right glob = parseFilePathGlobRel "*.cabal"
    res <- matchFileGlobRel path glob
    files <- filterM doesFileExist res -- Ignore directories
    case files of
        [] -> throwVabalErrorIO $ "No cabal file found in " ++ path
        (c:_) -> return c -- Only take the first cabal file, is it the correct behavior?

findLocations :: FilePath -> FilePathGlob -> IO [FilePath]
findLocations currDir glob = do
        paths <- matchFileGlob currDir glob
        mapM analyzePath paths

    where analyzePath :: FilePath -> IO FilePath
          analyzePath p = do
              isDir <- doesDirectoryExist p
              if isDir then
                  findCabalFileIn p
              else
                  return p



findCabalFiles :: Project -> IO [FilePath]
findCabalFiles proj = do
    let locations = map parseFilePathGlob $ packages proj
    currDir <- getCurrentDirectory
    if all isRight locations then
        concat <$> mapM (findLocations currDir) (rights locations)
    else do
        throwVabalErrorIO $ "Error while parsing glob file path in cabal.project."

findCabalFile :: IO FilePath
findCabalFile = do
    currDir <- getCurrentDirectory
    findCabalFileIn currDir

getCabalProject :: IO (Maybe Project)
getCabalProject = do
    let filename = "cabal.project"
    hasCabalProject <- doesFileExist filename
    if not hasCabalProject then
        return Nothing
    else do
        contents <- B.readFile filename
        case parseProject contents of
            Left err -> throwVabalErrorIO $ "Error while parsing cabal.project: " ++ err
            Right proj -> return (Just proj)


data PackageSpec = ProjectSpec [FilePath]
                 | CabalFile   FilePath

filesList :: PackageSpec -> [FilePath]
filesList (ProjectSpec cs) = cs
filesList (CabalFile c) = [c]

getCabalFiles :: IO PackageSpec
getCabalFiles = do
    proj <- getCabalProject
    case proj of
        Nothing -> CabalFile <$> findCabalFile
        Just p -> ProjectSpec <$> findCabalFiles p

