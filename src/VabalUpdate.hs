module VabalUpdate where

import GhcMetadata
import System.Directory
import System.FilePath
import UserInterface

updateProgDesc :: String
updateProgDesc = "Download updated metadata about ghc and base versions."

vabalUpdate :: IO ()
vabalUpdate = do
    dir <- getGhcMetadataDir
    createDirectoryIfMissing True dir
    downloadGhcMetadata (dir </> ghcMetadataFilename)
    writeOutput "Vabal successfully updated."

