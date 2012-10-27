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

newtype IgnoredSet = IgnoredSet [Pattern]
    deriving Show

instance Monoid IgnoredSet where
    mempty = IgnoredSet mempty
    mappend (IgnoredSet a) (IgnoredSet b) =
        IgnoredSet $ mappend a b

loadIgnoreFile :: FilePath -> IO IgnoredSet
loadIgnoreFile path =
     IgnoredSet . fmap compile. lines <$> readFile path

sepChanger :: Char -> Char
sepChanger '/' = pathSeparator
sepChanger '\\' = pathSeparator
sepChanger a = a

isPathIgnored :: IgnoredSet -> FilePath -> Bool
isPathIgnored (IgnoredSet lst) path = any isMatching lst
    where isMatching r = match r preparedMatch
          preparedMatch = map sepChanger path

emptyIgnoreSet :: IgnoredSet
emptyIgnoreSet = IgnoredSet []

