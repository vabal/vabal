module XArgsEscape (escapeForXArgs) where

import Data.Char (isSpace)

escapeSpecialChar :: Char -> String -> String
escapeSpecialChar '\'' accum = '\\': '\'' : accum
escapeSpecialChar '"' accum = '\\' : '"' : accum
escapeSpecialChar '\n' accum = '\\': '\n' : accum
escapeSpecialChar c accum
    | isSpace c = '\\' : c : accum
    | otherwise = c : accum

escapeForXArgs :: String -> String
escapeForXArgs = foldr escapeSpecialChar ""
