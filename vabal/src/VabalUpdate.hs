module VabalUpdate where

import MetadataManager
import System.Directory
import UserInterface

updateProgDesc :: String
updateProgDesc = "Download updated metadata about ghc and base versions."

vabalUpdate :: IO ()
vabalUpdate = do
    dir <- getGhcMetadataDir
    createDirectoryIfMissing True dir
    downloadMetadata dir
    writeOutput "Vabal successfully updated."

