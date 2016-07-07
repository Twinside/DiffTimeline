{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Difftimeline.CommitDiff ( createCommitDiff ) where

import Prelude

import Data.List( sortBy )
import Data.Byteable( toBytes )
import Control.Monad.Trans.Except( ExceptT, throwE, catchE )
import Control.Monad.Trans.Reader( ReaderT, runReaderT, mapReaderT, ask, asks )
import Control.Monad.Trans.Class( lift )
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

data DiffContext = DiffContext
  { _diffRepository :: !Git
  , _diffContextSize :: !Int
  , _diffDeep :: !Bool
  }

type ExceptIO = ExceptT String IO

type DiffMonad a = ReaderT DiffContext ExceptIO a

catchAppend :: ExceptIO a -> ExceptIO ([a] -> [a])
catchAppend m = ((:) <$> m) `catchE` (\_ -> return id)

catchAppendL :: forall a. DiffMonad a -> DiffMonad ([a] -> [a])
catchAppendL = mapReaderT catchAppend 

maySubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref
           -> DiffMonad CommitTreeDiff
maySubTree f name r = do
  repository <- asks _diffRepository
  sub <- liftIO $ accessObject repository r
  -- | Sometimes, there is nothing to see
  case sub of
     Nothing -> return $ f (T.pack name) r
     Just el -> lift $ batchSubTree repository f name r el

getCommit :: String -> Ref -> DiffMonad Object
getCommit msg ref = do
  repo <- asks _diffRepository
  lift $ accessCommit msg repo ref

-- | Compare two commits
createCommitDiff :: Git
                 -> Int  -- ^ Context size to embed
                 -> Bool -- ^ If we compute file diff
                 -> Ref  -- ^ Base commit
                 -> Ref  -- ^ Final commit (destination)
                 -> ExceptT String IO (Commit, CommitTreeDiff)
createCommitDiff repository contextSize deep prevRef ref = runReaderT go base where
  base = DiffContext repository contextSize deep

  go = do
    ObjCommit thisCommit <- getCommit "Error can't file commit" ref
    thisTree <- getObj "Error can't access commit tree" $ commitTreeish thisCommit
    (prevTreeRef, prevTree) <-
      if prevRef /= nullRef then do
        ObjCommit prevCommit <- getCommit  "Error can't file parent commit" prevRef
        let prevTreeRef = commitTreeish prevCommit
        (prevTreeRef,) <$> getObj "Error can't access previous commit tree" prevTreeRef
      else
        return (nullRef, ObjTree (Tree []))
    (thisCommit,) <$> diffObjects "" prevTree thisTree (commitTreeish thisCommit)
 
    

getObj :: String -> Ref -> DiffMonad Object
getObj reason ref = do
  repo <- asks _diffRepository
  lift $ errorIO reason $ accessObject repo ref

diffFileContent :: Bool -> Int -> FilePath -> Ref -> L.ByteString -> L.ByteString
                -> CommitTreeDiff
diffFileContent deep _ name ref c1 c2
  | not deep = ModifyElement (T.pack name) ref []
  | detectBinary c2 = ModifyBinaryElement (T.pack name) ref
diffFileContent deep contextSize name ref c1 c2 =
  ModifyElement (T.pack name) ref $! computeTextScript contextSize txtLeft txtRight
    where 
      strictify = B.concat . L.toChunks
      txtLeft = decodeUtf8 $ strictify c1
      txtRight = decodeUtf8 $ strictify c2

diffObjects :: FilePath -> Object -> Object -> Ref -> DiffMonad CommitTreeDiff
diffObjects name (ObjTree (Tree left)) (ObjTree (Tree right)) _r2 = 
  TreeElement (T.pack name) nullRef <$> diffTree name sortedLeft sortedRight
    where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
          sortedRight = sortBy (\(_,a,_) (_,b,_) -> compare a b) right
diffObjects name (ObjBlob (Blob c1)) (ObjBlob (Blob c2)) r = do
  deep <- asks _diffDeep
  context <- asks _diffContextSize
  return $ diffFileContent deep context name r c1 c2
diffObjects _ _ _ _= lift $ throwE "Wrong git object kind"

diffTree :: String -> [TreeEnt] -> [TreeEnt]
         -> DiffMonad [CommitTreeDiff]
diffTree _name   []     [] = return []
diffTree _name    [] rights = sequence
    [maySubTree AddElement fullName r | (_, item, r) <- rights
                                      , let fullName = BC.unpack $ toBytes item]
diffTree _name lefts     [] = sequence
    [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                      , let fullName = BC.unpack $ toBytes item ]
diffTree name lefts@((_, lName, lRef):ls) rights@((_, rName, rRef):rs)
    | lName == rName && lRef == rRef = 
       (NeutralElement (decodeUtf8 $ toBytes lName) lRef:) <$> diffTree name ls rs
    | lName == rName = do
        repo <- asks _diffRepository
        maySubL <- liftIO $ accessObject repo lRef
        maySubR <- liftIO $ accessObject repo rRef
        case (maySubL, maySubR) of
          -- This case should happen in presence of submodules.
          (Nothing, Nothing) -> (thisElem :) <$> diffTree name ls rs
             where thisElem = ModifyElement (decodeUtf8 $ toBytes lName) lRef []
          (Just subL, Just subR) -> do
              diffInfo <- catchAppendL $ diffObjects (BC.unpack $ toBytes lName) subL subR rRef
              diffInfo <$> diffTree name ls rs
          (Nothing, _) ->
              lift . throwE $ "Cannot fetch parent sub tree (" ++ toHexString lRef ++ ")"

          (_, Nothing) ->
              lift . throwE $ "Cannot fetch this sub tree (" ++ toHexString rRef ++ ")"

    | lName < rName =
        (:) <$> maySubTree DelElement (BC.unpack $ toBytes lName) lRef
            <*> diffTree name ls rights

    | otherwise = (:) <$> maySubTree AddElement (BC.unpack $ toBytes rName) rRef
                      <*> diffTree name lefts rs

