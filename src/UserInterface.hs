module UserInterface where

import System.IO (hPutStrLn, stderr)

-- TODO: Add colors etc

writeMessage :: String -> IO ()
writeMessage = putStrLn

writeWarning :: String -> IO ()
writeWarning = putStrLn

writeError :: String -> IO ()
writeError = hPutStrLn stderr

writeOutput :: String -> IO ()
writeOutput = putStrLn
