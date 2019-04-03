module Main where

import Distribution.Types.GenericPackageDescription
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity

import PackageSolver

import GhcDatabase

import System.Exit

import Distribution.Types.Version
import qualified Data.Set as S

import Data.Maybe (fromMaybe)

import Paths_vabal_lib (getDataFileName)

testDb :: GhcDatabase
testDb = dbFromList
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
                   { cabalFile :: FilePath
                   , acceptableResults :: S.Set Version
                   }

performTest :: GhcDatabase -> CabalFileTest -> IO ()
performTest ghcDb (CabalFileTest cf acceptableRes) = do
    filePath <- getDataFileName cf
    putStrLn $ "Testing: " ++ filePath

    pkgDescr <- readGenericPackageDescription normal filePath
    -- The null version means that no legit ghc version is compatible with the package
    let ghcVer = fromMaybe nullVersion
               . S.lookupMax
               $ analyzePackage (mkFlagAssignment []) ghcDb pkgDescr

    if ghcVer `S.member` acceptableRes then
        putStrLn "Test passed!"
    else do
        putStrLn $ "Test for package: " ++ filePath ++ " failed!"
        putStrLn $ "Got GHC Version: " ++ show ghcVer
        putStrLn $ "Expected one of: " ++ show acceptableRes

        exitFailure

 

main :: IO ()
main = do

    let cabalFileTests = [ CabalFileTest "tests/testCabalFiles/lens.cabal"
                                         (S.fromList [mkVersion [8,6,3]])

                         , CabalFileTest "tests/testCabalFiles/cairo.cabal"
                                         (S.fromList [mkVersion [8,4,4]])

                         , CabalFileTest "tests/testCabalFiles/sdl2.cabal"
                                         (S.fromList [mkVersion [8,6,3]])
                         ]

    mapM_ (performTest testDb) cabalFileTests
