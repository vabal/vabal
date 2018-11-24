module InstallationContext where

import qualified Network.HTTP.Client as N

data InstallationContext = InstallationContext
                         { buildType          :: String
                         , version            :: String
                         , installDir         :: FilePath
                         , tmpDir             :: FilePath
                         , ghcArchiveFilename :: FilePath
                         , netManager         :: N.Manager
                         }

