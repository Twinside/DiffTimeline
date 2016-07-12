{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Difftimeline.GitIgnore ( IgnoredSet
                              , loadIgnoreFile
                              , isPathIgnored
                              ) where

import Prelude
{-import System.FilePath.Glob( Pattern, compile, match )-}
import System.FilePath( pathSeparator )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S

{-import Debug.Trace-}

type IgnoredSet = ()

loadIgnoreFile :: FilePath -> IO IgnoredSet
loadIgnoreFile path = -- (\a -> trace (show a) a) <$>
   -- IgnoredSet . fmap (compile . changeGlobalGlob) . lines <$> readFile path
   return ()

sepChanger :: Char -> Char
sepChanger '/' = pathSeparator
sepChanger '\\' = pathSeparator
sepChanger a = a

changeGlobalGlob :: String -> String
changeGlobalGlob a@('*':'*':'/':_) = a
changeGlobalGlob ('*':xs) = "**/*" ++ xs
changeGlobalGlob a = a


isPathIgnored :: IgnoredSet -> FilePath -> Bool
isPathIgnored _ _path =
    return False

data AcceptedCharSet = AcceptedCharSet
   { _acceptedInverse :: !Bool
     -- | Must be sorted
   , _acceptedSingle  :: !(VU.Vector Char)
     -- | Must be sorted
   , _acceptedRange   :: !(VU.Vector (Char, Char))
   }
   deriving (Eq, Show)

data FilePatternPart str
   = LiteralPattern !str
   | Range !AcceptedCharSet
   | FileWildcard
   deriving (Eq, Show)

type FilePattern str = V.Vector (FilePatternPart str)

data PathPattern str
   = PathWildcard
   | PathSequence !(FilePattern str)
   deriving (Eq, Show)

data Pattern = Pattern
   { _filePatterns       :: ![FilePattern]
   , _patternInheritable :: ![Pattern]
   , _patternChildren    :: ![(PathPattern, Pattern)]
   }
   deriving (Eq, Show)

digitRange :: (Char, Char)
digitRange = ('0', '9')
alphaUpper = ('A', 'Z')
alphaLower = ('a', 'z')

punctuation, blanks, spaces :: [(Char, Char)]
punctuation = [('!','/'), (':','@'), ('[','`'), ('{','~')]
blanks = [('\t', '\t'), (' ', ' ')]
spaces = [('\t', '\r'), (' ', ' ')]
hexaF = [('A', 'F'), ('a', 'f')]

infixr 5 :<

pattern x :< xs <- (B.uncons -> Just (x, xs))
pattern Nil <- (B.uncons -> Nothing)

splitFolders :: B.ByteString -> [B.ByteString]
splitFolders = B.splitWith (\c -> c == '\t' || c == ' ')

tokenize :: B.ByteString -> Either String Pattern
tokenize = go . splitFolders
 where
   err _ c cs | errorRecovery opts = Right (Literal c) : go cs
   err s _ _ = [Left s]

   go Nil = return []
   go ('?' :< cs) | wcs = (NonPathSeparator :) <$> go cs
   go ('*' :< '*' :< '/' :< xs) =
       (AnyDirectory:) <$> go xs
   go ('*' :< xs) =
      (AnyNonPathSeparator:) <$> go xs

   go ('[' :< cs) =
       let (range,rest) = charRange opts cs in
       case range of
         Left s -> err s '[' cs
         r      -> r : go rest

   go (c :< cs)
      | isPathSeparator c = Right PathSeparator : go cs
      | isExtSeparator  c = Right  ExtSeparator : go cs
      | otherwise         = Right (Literal c)   : go cs

charRange = undefined

charRange :: B.ByteString -> (Either String Token, String)
charRange zs = case zs of
  y:ys | y `elem` "^!" ->
     case ys of
          -- [!-#] is not the inverse of [-#], it is the range ! through
          -- #
          '-':']':xs -> (Right (CharRange False [Left '-']), xs)
          '-'    :_  -> first (fmap (CharRange True )) (start zs)
          xs         -> first (fmap (CharRange False)) (start xs)
  _                  -> first (fmap (CharRange True )) (start zs)
 where
   start :: String -> (Either String CharRange, String)
   start (']':xs) = run $ char ']' xs
   start ('-':xs) = run $ char '-' xs
   start xs       = run $ go xs

   run :: ExceptT String (Writer CharRange) String
       -> (Either String CharRange, String)
   run m = case runWriter.runExceptT $ m of
                (Left   err,  _) -> (Left err, [])
                (Right rest, cs) -> (Right cs, rest)

   go :: String -> ExceptT String (Writer CharRange) String
   go ('[':':':xs) | characterClasses opts = readClass xs
   go (    ']':xs) = return xs
   go (      c:xs) =
      if not (pathSepInRanges opts) && isPathSeparator c
         then throwE "compile :: path separator within []"
         else char c xs
   go []           = throwE "compile :: unclosed [] in pattern"

   char :: Char -> String -> ExceptT String (Writer CharRange) String
   char c ('-':x:xs) =
      if x == ']'
         then ltell [Left c, Left '-'] >> return xs
         else ltell [Right (c,x)]      >>     go xs

   char c xs = ltell [Left c] >> go xs

   readClass :: String -> ExceptT String (Writer CharRange) String
   readClass xs = let (name,end) = span isAlpha xs
                   in case end of
                           ':':']':rest -> charClass name            >> go rest
                           _            -> ltell [Left '[',Left ':'] >> go xs


   digit  = Right ('0','9')
   upper  = Right ('A','Z')
   lower  = Right ('a','z')
   punct  = map Right [('!','/'), (':','@'), ('[','`'), ('{','~')]
   blanks = [Left '\t',         Left ' ']
   spaces = [Right ('\t','\r'), Left ' ']

   ltell = lift . tell

charClass :: String -> ExceptT String (Writer CharRange) ()
charClass name = case name of
  "alnum" -> pure [digitRange, alphaUpper, alphaLower]
  "alpha" -> pure [alphaUpper, alphaLower]
  "blank" -> pure blanks
  "cntrl" -> pure [('\0','\x1f'), ('\x7f', '\x7f')]
  "digit" -> pure [digitRange]
  "graph" -> pure [('!','~')]
  "lower" -> pure [alphaLower]
  "print" -> pure [(' ','~')]
  "punct" -> pure punctuation
  "space" -> pure spaces
  "upper" -> pure [alphaUpper]
  "xdigit"-> pure $ digitRange : hexaF
  _        ->
     throwE ("compile :: unknown character class '" ++name++ "'")
{-
sortCharRange :: [Either Char (Char,Char)] -> [Either Char (Char,Char)]
sortCharRange = sortBy cmp
 where
   cmp (Left   a)    (Left   b)    = compare a b
   cmp (Left   a)    (Right (b,_)) = compare a b
   cmp (Right (a,_)) (Left   b)    = compare a b
   cmp (Right (a,_)) (Right (b,_)) = compare a b
-- -}
