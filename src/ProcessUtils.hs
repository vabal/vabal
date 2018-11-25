module ProcessUtils where

import System.Process
import System.Exit

runExternalProcess :: FilePath -> [String] -> IO ExitCode
runExternalProcess bin args = do
    let processDescr = (proc bin args)
    (_, _, _, procHandle) <- createProcess processDescr
    waitForProcess procHandle

