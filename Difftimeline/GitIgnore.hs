{-# LANGUAGE PatternSynonyms #-}
module Difftimeline.GitIgnore ( IgnoredSet
                              , loadIgnoreFile
                              , isPathIgnored
                              , emptyIgnoreSet
                              ) where

import Prelude
import System.FilePath.Glob( Pattern, compile, match )
import System.FilePath( pathSeparator )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString as B
import qualified Data.Set as S

{-import Debug.Trace-}

newtype IgnoredSet = IgnoredSet [Pattern]
    deriving Show

instance Monoid IgnoredSet where
    mempty = IgnoredSet mempty
    mappend (IgnoredSet a) (IgnoredSet b) =
        IgnoredSet $ mappend a b

data LevelFilters = LevelFilters
  { _suffixPatterns :: !(V.Vector B.ByteString)
  , _wholePatterns  :: !(S.Set B.ByteString)
  }


loadIgnoreFile :: FilePath -> IO IgnoredSet
loadIgnoreFile path = -- (\a -> trace (show a) a) <$>
   IgnoredSet . fmap (compile . changeGlobalGlob) . lines <$> readFile path

sepChanger :: Char -> Char
sepChanger '/' = pathSeparator
sepChanger '\\' = pathSeparator
sepChanger a = a

changeGlobalGlob :: String -> String
changeGlobalGlob a@('*':'*':'/':_) = a
changeGlobalGlob ('*':xs) = "**/*" ++ xs
changeGlobalGlob a = a


isPathIgnored :: IgnoredSet -> FilePath -> Bool
isPathIgnored (IgnoredSet lst) path = -- (\a -> trace ("# " ++ path ++ " " ++ show a) a) $
  any isMatching lst
    where isMatching r = match r preparedMatch
          preparedMatch = map sepChanger path

emptyIgnoreSet :: IgnoredSet
emptyIgnoreSet = IgnoredSet []

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

{- 
data Pattern = Pattern
   { _filePatterns       :: ![FilePattern]
   , _patternInheritable :: ![Pattern]
   , _patternChildren    :: ![(PathPattern, Pattern)]
   }
   deriving (Eq, Show)

infixr 5 :<
pattern x :< xs <- (uncons -> Just (x, xs))
pattern Nil <- (uncons -> Nothing)

tokenize :: CompOptions -> String -> Either String Pattern
tokenize opts = fmap Pattern . sequence . go
 where
   err _ c cs | errorRecovery opts = Right (Literal c) : go cs
   err s _ _                       = [Left s]

   go Nil = []
   go ('?' :< cs) | wcs = Right NonPathSeparator : go cs
   go ('*' :< '*' :< '/' :< xs) =
       Right AnyDirectory : go xs
   go ('*' :< xs) =
       Right AnyNonPathSeparator : go xs

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
-- -}
{- 

type CharRange = [Either Char (Char,Char)]

charRange :: CompOptions -> String -> (Either String Token, String)
charRange opts zs =
   case zs of
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

   charClass :: String -> ExceptT String (Writer CharRange) ()
   charClass name =
      -- The POSIX classes
      --
      -- TODO: this is ASCII-only, not sure how this should be extended
      --       Unicode, or with a locale as input, or something else?
      case name of
           "alnum"  -> ltell [digit,upper,lower]
           "alpha"  -> ltell [upper,lower]
           "blank"  -> ltell blanks
           "cntrl"  -> ltell [Right ('\0','\x1f'), Left '\x7f']
           "digit"  -> ltell [digit]
           "graph"  -> ltell [Right ('!','~')]
           "lower"  -> ltell [lower]
           "print"  -> ltell [Right (' ','~')]
           "punct"  -> ltell punct
           "space"  -> ltell spaces
           "upper"  -> ltell [upper]
           "xdigit" -> ltell [digit, Right ('A','F'), Right ('a','f')]
           _        ->
              throwE ("compile :: unknown character class '" ++name++ "'")

   digit  = Right ('0','9')
   upper  = Right ('A','Z')
   lower  = Right ('a','z')
   punct  = map Right [('!','/'), (':','@'), ('[','`'), ('{','~')]
   blanks = [Left '\t',         Left ' ']
   spaces = [Right ('\t','\r'), Left ' ']

   ltell = lift . tell


------------------------------------------
-- OPTIMIZATION
------------------------------------------


optimizeCharRange :: Token -> Token
optimizeCharRange (CharRange b_ rs) = fin b_ . go . sortCharRange $ rs
 where
   -- [/] is interesting, it actually matches nothing at all
   -- [.] can be Literalized though, just don't make it into an ExtSeparator so
   --     that it doesn't match a leading dot
   fin True [Left  c] | not (isPathSeparator c) = Literal c
   fin True [Right r] | r == (minBound,maxBound) = NonPathSeparator
   fin b x = CharRange b x

   go [] = []

   go (x@(Left c) : xs) =
      case xs of
           [] -> [x]
           y@(Left d) : ys
              -- [aaaaa] -> [a]
              | c == d      -> go$ Left c : ys
              | d == succ c ->
                 let (ls,rest)        = span isLeft xs -- start from y
                     (catable,others) = increasingSeq (map fromLeft ls)
                     range            = (c, head catable)

                  in -- three (or more) Lefts make a Right
                     if null catable || null (tail catable)
                        then x : y : go ys
                        -- [abcd] -> [a-d]
                        else go$ Right range : map Left others ++ rest

              | otherwise -> x : go xs

           Right r : ys ->
              case addToRange r c of
                   -- [da-c] -> [a-d]
                   Just r' -> go$ Right r' : ys
                   Nothing -> x : go xs

   go (x@(Right r) : xs) =
      case xs of
           [] -> [x]
           Left c : ys ->
              case addToRange r c of
                   -- [a-cd] -> [a-d]
                   Just r' -> go$ Right r' : ys
                   Nothing -> x : go xs

           Right r' : ys ->
              case overlap r r' of
                   -- [a-cb-d] -> [a-d]
                   Just o  -> go$ Right o : ys
                   Nothing -> x : go xs
optimizeCharRange _ = error "Glob.optimizeCharRange :: internal error"

sortCharRange :: [Either Char (Char,Char)] -> [Either Char (Char,Char)]
sortCharRange = sortBy cmp
 where
   cmp (Left   a)    (Left   b)    = compare a b
   cmp (Left   a)    (Right (b,_)) = compare a b
   cmp (Right (a,_)) (Left   b)    = compare a b
   cmp (Right (a,_)) (Right (b,_)) = compare a b
-- -}
