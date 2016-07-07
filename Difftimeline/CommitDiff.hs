{-# LANGUAGE TupleSections #-}
module Difftimeline.CommitDiff ( createCommitDiff ) where

import Prelude

import Data.List( sortBy )
import Data.Byteable( toBytes )
import Control.Monad.Trans.Except( ExceptT, throwE, catchE )
import Control.Monad.IO.Class( liftIO )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Git 
    ( Commit( .. )
    , Tree( .. )
    , Blob( .. )
    , Git
    , Ref
    )
import Data.Git.Ref( toHexString )
import Data.Git.Storage.Object( Object( .. ) )
import Data.Git.Types ( TreeEnt )

import Difftimeline.Diff
import Difftimeline.BaseLayer
import Difftimeline.Contract

-- | Compare two commits
createCommitDiff :: Git
                 -> Int  -- ^ Context size to embed
                 -> Bool -- ^ If we compute file diff
                 -> Ref  -- ^ Base commit
                 -> Ref  -- ^ Final commit (destination)
                 -> ExceptT String IO (Commit, CommitTreeDiff)
createCommitDiff repository contextSize deep prevRef ref = do
    ObjCommit thisCommit <- accessCommit "Error can't file commit" repository ref
    thisTree <- getObj "Error can't access commit tree" $ commitTreeish thisCommit

    (prevTreeRef, prevTree) <- if prevRef /= nullRef
            then do ObjCommit prevCommit <- accessCommit "Error can't file parent commit" repository prevRef
                    let prevTreeRef = commitTreeish prevCommit
                    (prevTreeRef,) <$> getObj "Error can't access previous commit tree" prevTreeRef
            else return (nullRef, ObjTree (Tree []))

    (,) thisCommit <$> inner "" prevTree prevTreeRef thisTree (commitTreeish thisCommit)
  where getObj reason = errorIO reason . accessObject repository
        inner name (ObjTree (Tree left)) _r1 (ObjTree (Tree right)) _r2 = 
          TreeElement (T.pack name) nullRef <$> diffTree name sortedLeft sortedRight
            where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
                  sortedRight = sortBy (\(_,a,_) (_,b,_) -> compare a b) right

        inner name (ObjBlob (Blob c1)) r1 (ObjBlob (Blob c2)) r2
            | r1 == r2  = return $ NeutralElement (T.pack name) r1
            | not deep  = return $ ModifyElement (T.pack name) r2 []
            | detectBinary c2  = return $ ModifyBinaryElement (T.pack name) r2
            | otherwise = return .
                ModifyElement (T.pack name) r2 $! computeTextScript contextSize txtLeft txtRight
                    where strictify = B.concat . L.toChunks
                          txtLeft = decodeUtf8 $ strictify c1
                          txtRight = decodeUtf8 $ strictify c2
        inner _ _ _ _ _ = throwE "Wrong git object kind"

        maySubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref
                   -> ExceptT String IO CommitTreeDiff
        maySubTree f name r = do
            sub <- liftIO $ accessObject repository r
            -- | Sometimes, there is nothing to see
            case sub of
               Nothing -> return $ f (T.pack name) r
               Just el  -> batchSubTree repository f name r el

        diffTree :: String -> [TreeEnt] -> [TreeEnt]
                 -> ExceptT String IO [CommitTreeDiff]
        diffTree _name   []     [] = return []
        diffTree _name    [] rights = sequence
            [maySubTree AddElement fullName r | (_, item, r) <- rights
                                              , let fullName = BC.unpack $ toBytes item]
        diffTree _name lefts     [] = sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = BC.unpack $ toBytes item ]
        diffTree name lefts@((_, lName, lRef):ls) rights@((_, rName, rRef):rs)
            | lName == rName = do
                maySubL <- liftIO $ accessObject repository lRef
                maySubR <- liftIO $ accessObject repository rRef

                case (maySubL, maySubR) of
                  -- This case should happen in presence of submodules.
                  (Nothing, Nothing) -> (thisElem :) <$> diffTree name ls rs
                        where thisElem | rRef == rRef = NeutralElement (decodeUtf8 $ toBytes lName) lRef
                                       | otherwise = ModifyElement (decodeUtf8 $ toBytes lName) lRef []

                  (Just subL, Just subR) ->
                        (((:) <$> inner (BC.unpack $ toBytes lName) subL lRef subR rRef)
                                `catchE` (\_ -> return id)) <*> diffTree name ls rs
                  (Nothing, _) ->
                      throwE $ "Cannot fetch parent sub tree (" ++ toHexString lRef ++ ")"

                  (_, Nothing) ->
                      throwE $ "Cannot fetch this sub tree (" ++ toHexString rRef ++ ")"

            | lName < rName =
                (:) <$> maySubTree DelElement (BC.unpack $ toBytes lName) lRef
                    <*> diffTree name ls rights

            | otherwise = (:) <$> maySubTree AddElement (BC.unpack $ toBytes rName) rRef
                              <*> diffTree name lefts rs

