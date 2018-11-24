module BuildTypeRecognizer (detectGhcBuildType) where

import System.Info
import System.Process
import System.Exit
import System.IO

import VabalError

-- This module tries to determine the system we're running on and the best GHC binary type to download

data SupportedOS = Linux
                 | Darwin

data SupportedArch = X86_64
                   | I386


throwUnsupportedPlatform :: IO a
throwUnsupportedPlatform = throwVabalErrorIO "Unsupported platform."

getSupportedOS :: IO SupportedOS
getSupportedOS = case os of
    "linux" -> return Linux
    "darwin" -> return Darwin
    _        -> throwUnsupportedPlatform

getSupportedArch :: IO SupportedArch
getSupportedArch = case arch of
    "x86_64" -> return X86_64
    "i386"   -> return I386
    "i686"   -> return I386
    _        -> throwUnsupportedPlatform


-- Use pkg-config to determine ncurses version
linuxGetNcursesVersion :: IO String
linuxGetNcursesVersion = do
    let processDescr = (proc "pkg-config" ["--modversion", "ncurses"])
                     { std_out = CreatePipe
                     }

    (_, Just outHandle, _, procHandle) <- createProcess processDescr
    version <- hGetLine outHandle
    res <- waitForProcess procHandle
    case res of
        ExitSuccess -> return version
        ExitFailure _ -> throwVabalErrorIO "Could not find ncurses infos."

-- Get the ghc build type to look for
getBuildType :: SupportedOS -> SupportedArch -> IO String
getBuildType Linux I386    = return "i386-deb8-linux"
getBuildType Darwin X86_64 = return "x86_64-apple-darwin"
getBuildType Linux X86_64  = do
    version <- linuxGetNcursesVersion
    if version >= "6" then
        return "x86_64-unknown-linux"
    else
        return "x86_64-deb8-linux"

getBuildType _ _           = throwUnsupportedPlatform


detectGhcBuildType :: IO String
detectGhcBuildType = do
    os <- getSupportedOS
    arch <- getSupportedArch
    getBuildType os arch

