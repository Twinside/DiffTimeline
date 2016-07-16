{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
module Difftimeline.GitIgnore ( IgnoredSet
                              , loadIgnoreFile
                              , isPathIgnored
                              , emptyIgnoreSet
                              ) where

import Prelude
{-import System.FilePath.Glob( Pattern, compile, match )-}
import Data.Foldable( fold )
import Data.Monoid( (<>) )
import Data.List( intersperse, partition )
import System.FilePath( isPathSeparator )
import Text.Regex.TDFA( Regex, makeRegex, matchTest )
import qualified Data.ByteString.Char8 as B

{-import Debug.Trace-}

data IgnoredSet = IgnoredSet
  { _ignored     :: !Regex
  , _whiteListed :: !Regex
  }

emptyIgnoreSet :: IgnoredSet
emptyIgnoreSet = IgnoredSet r r where r = makeRegex ("^$" :: String)

isPathIgnored :: IgnoredSet -> FilePath -> Bool
isPathIgnored set p =
   _ignored set `matchTest` p && not (_whiteListed set `matchTest` p)

isGitIgnorePattern :: ParseString -> Bool
isGitIgnorePattern ('#' :< _) = False
isGitIgnorePattern s 
  | B.length s == 0 = False
  | otherwise = not $ B.all (\v -> v == '\t' || v == ' ') s

isGitInverse :: ParseString -> Bool
isGitInverse ('!' :< _) = True
isGitInverse _ = False

cleanLine :: ParseString -> ParseString
cleanLine s 
  | B.length s == 0 = s
  | B.last s == '\r' = B.take (B.length s - 1) s
  | otherwise = s

toFullRegexp :: [String] -> Regex
toFullRegexp lst = makeRegex $ "^(" <> combined <> ")$" where
  combined = fold $ intersperse "|" lst

loadIgnoreFile :: FilePath -> IO IgnoredSet
loadIgnoreFile path = do
  patternLines <-  B.lines <$> B.readFile path
  let cleanedPatterns = cleanLine <$> patternLines
      patternString = filter isGitIgnorePattern cleanedPatterns
      (invPatterns, patterns) = partition isGitInverse patternString
  pure $ IgnoredSet (toFullRegexp $ toRegexp <$> patterns)
                    (toFullRegexp $ toRegexp <$> invPatterns)


toRegexp :: ParseString -> String
toRegexp wholeString = go wholeString where
  generalPattern = not $ B.any isPathSeparator wholeString

  go str = case str of
    '*' :< '*' :< '/' :< after -> ".*/" <> go after
    '*' :< after -> 
      if generalPattern then ".*" <> go after
      else "[^/]*" <> go after
    '?' :< after -> '.' : go after
    '.' :< after -> "\\." <> go after
    c   :< after -> c : go after
    _ -> []

infixr 5 :<

type ParseString = B.ByteString
pattern x :< xs <- (B.uncons -> Just (x, xs))
{-pattern Nil <- (B.uncons -> Nothing)-}

