module Main where

import XArgsEscape
import System.Process (readProcess)
import System.Exit

main :: IO ()
main = do
    let inputString = "home/ciao come stai/io \"bene\ne tu'\n"
    putStrLn $ escapeForXArgs inputString

    res <- readProcess "xargs" [] (escapeForXArgs inputString)
    if res == (inputString  ++ "\n") then
        exitWith ExitSuccess
    else exitWith $ ExitFailure 1

