module Difftimeline.Contract
    ( CommitTreeDiff( .. )
    ) where

import Prelude

import qualified Data.Text as T
import qualified Data.Vector as V
import Difftimeline.Diff
import Data.Git( Ref )

data CommitTreeDiff
    = AddElement !T.Text !Ref
    | DelElement !T.Text !Ref
    | NeutralElement !T.Text !Ref
    | TreeElement !T.Text !Ref ![CommitTreeDiff]
    | ModifyElement !T.Text !Ref ![(DiffCommand, V.Vector T.Text)]
    | ModifyBinaryElement !T.Text !Ref
    deriving (Eq, Show)

instance Invertible CommitTreeDiff where
    invertWay e@(NeutralElement _ _) = e
    invertWay e@(ModifyBinaryElement _ _)  = e
    invertWay (AddElement t r) = DelElement t r
    invertWay (DelElement t r) = AddElement t r
    invertWay (TreeElement t r sub) =
        TreeElement t r $ invertWay <$> sub
    invertWay (ModifyElement t r lst) =
        ModifyElement t r $ invertDiff <$> lst
          where invertDiff (d, v) = (invertWay d, v)

