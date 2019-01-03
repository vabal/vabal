module Main where

import qualified Data.ByteString as B

import Distribution.Types.GenericPackageDescription

import System.FilePath

import CabalAnalyzer

import VabalContext
import GhcDatabase

import System.Exit

import Distribution.Types.Version
import qualified Data.Map.Strict as M
import qualified Data.Set as S

testDb :: GhcDatabase
testDb = M.fromList
    [ (mkVersion [8,6,3], GhcMetadata (mkVersion [4,12,0,0]) (mkVersion [2,4]))
    , (mkVersion [8,6,2], GhcMetadata (mkVersion [4,12,0,0]) (mkVersion [2,4]))
    , (mkVersion [8,6,1], GhcMetadata (mkVersion [4,12,0,0]) (mkVersion [2,4]))
    , (mkVersion [8,4,4], GhcMetadata (mkVersion [4,11,1,0]) (mkVersion [2,2]))
    , (mkVersion [8,4,3], GhcMetadata (mkVersion [4,11,1,0]) (mkVersion [2,2]))
    , (mkVersion [8,4,2], GhcMetadata (mkVersion [4,11,1,0]) (mkVersion [2,2]))
    , (mkVersion [8,4,1], GhcMetadata (mkVersion [4,11,0,0]) (mkVersion [2,2]))
    , (mkVersion [8,2,2], GhcMetadata (mkVersion [4,10,1,0]) (mkVersion [2,0]))
    , (mkVersion [8,2,1], GhcMetadata (mkVersion [4,10,0,0]) (mkVersion [2,0]))
    , (mkVersion [8,0,2], GhcMetadata (mkVersion [4,9,1,0]) (mkVersion [1,24]))
    , (mkVersion [8,0,1], GhcMetadata (mkVersion [4,9,0,0]) (mkVersion [1,24]))
    , (mkVersion [7,10,3], GhcMetadata (mkVersion [4,8,2,0]) (mkVersion [1,22]))
    , (mkVersion [7,10,2], GhcMetadata (mkVersion [4,8,1,0]) (mkVersion [1,22]))
    , (mkVersion [7,10,1], GhcMetadata (mkVersion [4,8,0,0]) (mkVersion [1,22]))
    , (mkVersion [7,8,4], GhcMetadata (mkVersion [4,7,0,2]) (mkVersion [0]))
    , (mkVersion [7,8,3], GhcMetadata (mkVersion [4,7,0,1]) (mkVersion [0]))
    , (mkVersion [7,8,1], GhcMetadata (mkVersion [4,7,0,0]) (mkVersion [0]))
    , (mkVersion [7,6,2], GhcMetadata (mkVersion [4,6,0,1]) (mkVersion [0]))
    , (mkVersion [7,6,1], GhcMetadata (mkVersion [4,6,0,0]) (mkVersion [0]))
    , (mkVersion [7,4,2], GhcMetadata (mkVersion [4,5,1,0]) (mkVersion [0]))
    , (mkVersion [7,4,1], GhcMetadata (mkVersion [4,5,0,0]) (mkVersion [0]))
    , (mkVersion [7,2,2], GhcMetadata (mkVersion [4,4,1,0]) (mkVersion [0]))
    , (mkVersion [7,2,1], GhcMetadata (mkVersion [4,4,0,0]) (mkVersion [0]))
    , (mkVersion [7,0,2], GhcMetadata (mkVersion [4,3,1,0]) (mkVersion [0]))
    , (mkVersion [7,0,1], GhcMetadata (mkVersion [4,3,0,0]) (mkVersion [0]))
    , (mkVersion [6,12,3], GhcMetadata (mkVersion [4,2,0,2]) (mkVersion [0]))
    , (mkVersion [6,12,2], GhcMetadata (mkVersion [4,2,0,1]) (mkVersion [0]))
    , (mkVersion [6,12,1], GhcMetadata (mkVersion [4,2,0,0]) (mkVersion [0]))
    , (mkVersion [6,10,2], GhcMetadata (mkVersion [4,1,0,0]) (mkVersion [0]))
    , (mkVersion [6,10,1], GhcMetadata (mkVersion [4,0,0,0]) (mkVersion [0]))
    ]

data CabalFileTest = CabalFileTest
                   { cabalFilePath :: FilePath
                   , acceptableResults :: S.Set Version
                   }

performTest :: GhcDatabase -> CabalFileTest -> IO ()
performTest ghcDb (CabalFileTest filePath acceptableRes) = do
    putStrLn $ "Testing: " ++ filePath

    let vabalCtx = VabalContext mempty ghcDb False
    contents <- B.readFile filePath
    let version = analyzeCabalFileAllTargets (mkFlagAssignment []) vabalCtx Nothing contents
    if version `S.member` acceptableRes then
        putStrLn "Test passed!"
    else do
        putStrLn $ "Test for package: " ++ filePath ++ " failed!"
        putStrLn $ "Got GHC Version: " ++ show version
        putStrLn $ "Expected one of: " ++ show acceptableRes

        exitFailure

 

main :: IO ()
main = do

    let cabalFileTests = [ CabalFileTest ("tests" </> "testCabalFiles" </> "lens.cabal")
                                         (S.fromList [mkVersion [8,6,3]])

                         , CabalFileTest ("tests" </> "testCabalFiles" </> "cairo.cabal")
                                         (S.fromList [mkVersion [8,4,4]])

                         , CabalFileTest ("tests" </> "testCabalFiles" </> "sdl2.cabal")
                                         (S.fromList [mkVersion [8,6,3]])
                         ]

    mapM_ (performTest testDb) cabalFileTests
