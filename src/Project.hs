{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}
module Project where
    
-- cabal.project file support

import Distribution.Parsec.Parser
import Distribution.FieldGrammar
import Distribution.Compat.Lens
import Distribution.Compat.CharParsing
import Distribution.Parsec.ParseResult
import Distribution.CabalSpecVersion

import Data.Char (isSpace)

import Distribution.Parsec.Newtypes
import Distribution.Compat.Newtype
import Distribution.Parsec.Class
import Distribution.Pretty
import qualified Data.ByteString as B

import qualified Text.PrettyPrint as PP

newtype PackageLocation = PackageLocation String

instance Newtype PackageLocation String where
    pack = PackageLocation
    unpack (PackageLocation s) = s

instance Parsec PackageLocation where
    parsec = PackageLocation <$> outerTerm
      where
        outerTerm = ($ "") <$> outerChars

        outerChars, outerChar, innerChars, innerChar :: CabalParsing m => m ShowS
        outerChars = foldr (.) id <$> some outerChar
        innerChars = foldr (.) id <$> many innerChar

        outerChar = do
            c <- satisfy $ \c -> not (isSpace c || c == '}' || c == ',')
            kont c

        innerChar = do
            c <- satisfy $ \c -> not (isSpace c || c == '}')
            kont c

        kont :: CabalParsing m => Char -> m ShowS
        kont c = case c of
           '{' -> do
               cs <- innerChars
               c' <- char '}'
               return (showChar c . cs . showChar c')
           _   -> return $ showChar c


instance Pretty PackageLocation where
    pretty (PackageLocation p) = PP.text p


data Project = Project
             { packages :: [String]
             }
             deriving(Show)

packagesLens :: ALens' Project [String]
packagesLens f p = (\a -> p { packages = a }) <$> f (packages p)

projectGrammar :: ParsecFieldGrammar Project Project
projectGrammar = Project <$>
    monoidalFieldAla "packages" (alaList' FSep PackageLocation) packagesLens

parseProject :: B.ByteString -> Either String Project
parseProject bs = do
    parsedFields <- either (Left . show) Right $ readFields bs
    let (fields, _) = partitionFields parsedFields
    -- let knownFields = fieldGrammarKnownFieldList projectGrammar
    case runParseResult (parseFieldGrammar cabalSpecLatest fields projectGrammar) of
        (_, Left _)     -> Left "Error while parsing cabal.project file."
        (_, Right proj) -> return proj
