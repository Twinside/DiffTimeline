{-# LANGUAGE ViewPatterns #-}
module Difftimeline.Types
    ( DiffTimeline (..)
    , Command( .. )
    ) where

import Prelude
import Data.Git( Git )
import Difftimeline.GitIgnore( IgnoredSet )

data Command
    = DiffCompare String !String
    | DiffBlame !String
    | DiffFile !String
    | DiffWorking
    deriving Show

data DiffTimeline = DiffTimeline
    { getDevMode :: Maybe FilePath
    , getRepository :: Git
    , initialCommand :: Command
    , getIgnoreSet :: IgnoredSet
    }

