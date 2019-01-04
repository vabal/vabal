module Utils where

import System.Process
import System.Exit
import Distribution.Types.Version
import System.IO (stderr)
import Data.List (intercalate)

runExternalProcess :: FilePath -> [String] -> IO ExitCode
runExternalProcess bin args = do
    let processDescr = (proc bin args)
                     { std_out = UseHandle stderr
                     , std_err = UseHandle stderr
                     }
    (_, _, _, procHandle) <- createProcess processDescr
    waitForProcess procHandle

removeTrailingNewlines :: String -> String
removeTrailingNewlines = reverse . dropWhile (== '\n') . reverse

prettyPrintVersion :: Version -> String
prettyPrintVersion ver = intercalate "." $ map show (versionNumbers ver)

trimVersionString :: String -> String
trimVersionString = dropWhile (== ' ')

