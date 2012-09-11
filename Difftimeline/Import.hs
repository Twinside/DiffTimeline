module Difftimeline.Import
    ( (<>)
    , Text
    , module X
    ) where

import Prelude as X hiding (writeFile, readFile)
import Difftimeline.Foundation as X
import Data.Monoid as X (Monoid (mappend, mempty, mconcat))
import Control.Applicative as X ((<$>), (<*>), pure)
import Data.Text (Text)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
