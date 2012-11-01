module Difftimeline.GitIgnore ( IgnoredSet
                              , loadIgnoreFile
                              , isPathIgnored
                              , emptyIgnoreSet
                              ) where

import Prelude
import Data.Monoid
import Control.Applicative( (<$>) )
import System.FilePath.Glob( Pattern, compile, match )
import System.FilePath( pathSeparator )

{-import Debug.Trace-}

newtype IgnoredSet = IgnoredSet [Pattern]
    deriving Show

instance Monoid IgnoredSet where
    mempty = IgnoredSet mempty
    mappend (IgnoredSet a) (IgnoredSet b) =
        IgnoredSet $ mappend a b

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

