--TODO: [code cleanup] plausibly much of this module should be merged with
-- similar functionality in Cabal.
module Glob
    ( FilePathGlob(..)
    , FilePathRoot(..)
    , FilePathGlobRel(..)
    , Glob
    , GlobPiece(..)
    , matchFileGlob
    , matchFileGlobRel
    , matchGlob
    , isTrivialFilePathGlob
    , getFilePathRootDirectory
    , parseFilePathGlob
    , parseFilePathGlobRel
    , dispFilePathGlob
    ) where


import Distribution.Parsec.Class
import Distribution.Compat.CharParsing
import Control.Applicative

import           Data.List (stripPrefix)
import           Data.Char (toUpper)
import           Control.Monad (filterM, when)

import qualified Text.PrettyPrint as Disp

import           System.FilePath
import           System.Directory


-- | A file path specified by globbing
--
data FilePathGlob = FilePathGlob FilePathRoot FilePathGlobRel
  deriving (Eq, Show)

data FilePathGlobRel
   = GlobDir  !Glob !FilePathGlobRel
   | GlobFile !Glob
   | GlobDirTrailing                -- ^ trailing dir, a glob ending in @/@
  deriving (Eq, Show)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece = WildCard
               | Literal String
               | Union [Glob]
  deriving (Eq, Show)

data FilePathRoot
   = FilePathRelative
   | FilePathRoot FilePath -- ^ e.g. @"/"@, @"c:\"@ or result of 'takeDrive'
   | FilePathHomeDir
  deriving (Eq, Show)

-- | Check if a 'FilePathGlob' doesn't actually make use of any globbing and
-- is in fact equivalent to a non-glob 'FilePath'.
--
-- If it is trivial in this sense then the result is the equivalent constant
-- 'FilePath'. On the other hand if it is not trivial (so could in principle
-- match more than one file) then the result is @Nothing@.
--
isTrivialFilePathGlob :: FilePathGlob -> Maybe FilePath
isTrivialFilePathGlob (FilePathGlob root pathglob) =
    case root of
      FilePathRelative       -> go []      pathglob
      FilePathRoot root'     -> go [root'] pathglob
      FilePathHomeDir        -> Nothing
  where
    go paths (GlobDir  [Literal path] globs) = go (path:paths) globs
    go paths (GlobFile [Literal path]) = Just (joinPath (reverse (path:paths)))
    go paths  GlobDirTrailing          = Just (addTrailingPathSeparator
                                                 (joinPath (reverse paths)))
    go _ _ = Nothing

-- | Get the 'FilePath' corresponding to a 'FilePathRoot'.
--
-- The 'FilePath' argument is required to supply the path for the
-- 'FilePathRelative' case.
--
getFilePathRootDirectory :: FilePathRoot
                         -> FilePath      -- ^ root for relative paths
                         -> IO FilePath
getFilePathRootDirectory  FilePathRelative   root = return root
getFilePathRootDirectory (FilePathRoot root) _    = return root
getFilePathRootDirectory  FilePathHomeDir    _    = getHomeDirectory


------------------------------------------------------------------------------
-- Matching
--

-- | Match a 'FilePathGlob' against the file system, starting from a given
-- root directory for relative paths. The results of relative globs are
-- relative to the given root. Matches for absolute globs are absolute.
--
matchFileGlob :: FilePath -> FilePathGlob -> IO [FilePath]
matchFileGlob relroot (FilePathGlob globroot glob) = do
    root <- getFilePathRootDirectory globroot relroot
    matches <- matchFileGlobRel root glob
    case globroot of
      FilePathRelative -> return matches
      _                -> return (map (root </>) matches)

-- | Match a 'FilePathGlobRel' against the file system, starting from a
-- given root directory. The results are all relative to the given root.
--
matchFileGlobRel :: FilePath -> FilePathGlobRel -> IO [FilePath]
matchFileGlobRel root glob0 = go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (matchGlob glob) entries
      return (map (dir </>) files)

    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <- filterM (\subdir -> doesDirectoryExist
                                       (root </> dir </> subdir))
               $ filter (matchGlob glob) entries
      concat <$> mapM (\subdir -> go globPath (dir </> subdir)) subdirs

    go GlobDirTrailing dir = return [dir]


-- | Match a globbing pattern against a file path component
--
matchGlob :: Glob -> String -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard:_) ('.':_)  = False
    goStart (Union globs:rest) cs = any (\glob -> goStart (glob ++ rest) cs)
                                        globs
    goStart rest               cs = go rest cs

    go []                 ""    = True
    go (Literal lit:rest) cs
      | Just cs' <- stripPrefix lit cs
                                = go rest cs'
      | otherwise               = False
    go [WildCard]         ""    = True
    go (WildCard:rest)   (c:cs) = go rest (c:cs) || go (WildCard:rest) cs
    go (Union globs:rest)   cs  = any (\glob -> go (glob ++ rest) cs) globs
    go []                (_:_)  = False
    go (_:_)              ""    = False


------------------------------------------------------------------------------
-- Parsing & printing
--

parseFilePathGlob :: String -> Either String FilePathGlob
parseFilePathGlob = eitherParsec

parseFilePathGlobRel :: String -> Either String FilePathGlobRel
parseFilePathGlobRel = eitherParsec

instance Parsec FilePathGlob where
    parsec = do
            root <- parsec
            (FilePathGlob root <$> parsec) <|> alt2 root

        where alt2 root = do
                  when (root == FilePathRelative) (fail "Unexpected relative path")
                  return (FilePathGlob root GlobDirTrailing)

dispFilePathGlob :: FilePathGlob -> Disp.Doc
dispFilePathGlob (FilePathGlob root pathglob) = dispFilePathRoot root Disp.<> dispFilePathGlobRel pathglob


instance Parsec FilePathRoot where
    parsec = (char '/' *> return (FilePathRoot "/"))
             <|> (char '~' *> char '/' *> return FilePathHomeDir)
             <|> parseDrive
             <|> return FilePathRelative

        where isAsciiAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
              parseDrive = do
                  drive <- satisfy isAsciiAlpha
                  _ <- char ':'
                  _ <- char '/' <|> char '\\'
                  return (FilePathRoot (toUpper drive : ":\\"))

dispFilePathRoot :: FilePathRoot -> Disp.Doc
dispFilePathRoot  FilePathRelative    = Disp.empty
dispFilePathRoot (FilePathRoot root)  = Disp.text root
dispFilePathRoot FilePathHomeDir      = Disp.char '~' Disp.<> Disp.char '/'


instance Parsec FilePathGlobRel where
    parsec = parsePath
        where parsePath = do
                globpieces <- parseGlob
                try (asDir globpieces) <|> asTDir globpieces <|> asFile globpieces

              asDir glob = do dirSep
                              globs <- parsePath
                              return (GlobDir glob globs)

              asTDir glob = do dirSep
                               return (GlobDir glob GlobDirTrailing)

              asFile glob = return (GlobFile glob)

              dirSep      = (char '/' *> return ()) <|> notEscapingBackslash

              notEscapingBackslash = do
                  _ <- char '\\' *> notFollowedBy (satisfy isGlobEscapedChar)
                  return ()

dispFilePathGlobRel :: FilePathGlobRel -> Disp.Doc
dispFilePathGlobRel (GlobDir  glob pathglob) = dispGlob glob
                      Disp.<> Disp.char '/'
                      Disp.<> dispFilePathGlobRel pathglob

dispFilePathGlobRel (GlobFile glob)          = dispGlob glob
dispFilePathGlobRel  GlobDirTrailing         = Disp.empty


dispGlob :: Glob -> Disp.Doc
dispGlob = Disp.hcat . map dispPiece
  where
    dispPiece WildCard      = Disp.char '*'
    dispPiece (Literal str) = Disp.text (escape str)
    dispPiece (Union globs) = Disp.braces
                                (Disp.hcat (Disp.punctuate
                                             (Disp.char ',')
                                             (map dispGlob globs)))
    escape []               = []
    escape (c:cs)
      | isGlobEscapedChar c = '\\' : c : escape cs
      | otherwise           =        c : escape cs

parseGlob :: CharParsing m => m Glob
parseGlob = some parsePiece
  where
    parsePiece = literal <|> wildcard <|> union

    wildcard = char '*' *> pure WildCard

    union = between (char '{') (char '}') $
              fmap Union (sepBy1 parseGlob (char ','))

    literal = Literal `fmap` litchars1

    litchar = normal <|> escape

    normal  = satisfy (\c -> not (isGlobEscapedChar c)
                                && c /= '/' && c /= '\\')
    escape  = char '\\' *> satisfy isGlobEscapedChar

    litchars1 = liftA2 (:) litchar litchars

    litchars = litchars1 <|> pure []

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*'  = True
isGlobEscapedChar '{'  = True
isGlobEscapedChar '}'  = True
isGlobEscapedChar ','  = True
isGlobEscapedChar _    = False

