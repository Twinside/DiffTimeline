{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Difftimeline.DiffWorkingDirectory
    ( diffWorkingDirectory
    ) where

import Prelude

import Data.List( sortBy )
import Data.Byteable( toBytes )
import Control.Monad.Trans.Except( ExceptT, throwE, catchE )
import Control.Monad.IO.Class( liftIO )
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T

import Data.Time.Clock
    ( UTCTime
    , addUTCTime
    , picosecondsToDiffTime 
    )

import Data.Time.Clock.POSIX( posixSecondsToUTCTime )

import System.Directory( getModificationTime )
import Data.Git 
    ( Commit( .. )
    , Tree( .. )
    , Blob( .. )
    , Git
    , Ref
    )
import Data.Git.Storage
    ( findFileInIndex
    , IndexEntry( _mtime, _mtimeNano )
    )
import Data.Git.Storage.Object( Object( .. ) )
import Data.Git.Types( TreeEnt )

import Difftimeline.BaseLayer
import Difftimeline.Contract
import Difftimeline.Diff
import Difftimeline.GitIgnore

{-import Text.Printf-}
{-import Debug.Trace-}

-- | Want same behaviour between windows & Unix
(</>) :: FilePath -> FilePath -> FilePath
"" </> b = b
a  </> b = a ++ "/" ++ b

(<///>) :: FilePath -> FilePath -> FilePath
"" <///> b = b
a  <///> b = a </> b

wasEdited :: Git -> UTCTime -> FilePath -> IO Bool
wasEdited repository maxTime path = check `E.catch` onError where
  onError :: E.SomeException -> IO Bool
  onError _e = return True

  check = do
    indexEntry <- findFileInIndex repository (BC.pack path)
    case indexEntry of
      Nothing -> return True
      Just ix -> do
        modTime <- getModificationTime path
        let diffToTime = fromRational . toRational
            nanoTime = diffToTime $ picosecondsToDiffTime (fromIntegral (_mtimeNano ix) * 1000)
            secondTime = posixSecondsToUTCTime . realToFrac $ _mtime ix
            time = addUTCTime nanoTime secondTime
            latestTime = max time maxTime
        return $ latestTime < modTime

fileComparer :: Int -> FilePath -> L.ByteString -> Ref -> L.ByteString
             -> CommitTreeDiff
fileComparer contextSize name bin1 r1 bin2
  | bin1 == bin2 = NeutralElement (T.pack name) r1
  | detectBinary bin2 = ModifyBinaryElement (T.pack name) r1
  | otherwise =  case computeTextScript contextSize bin1 bin2 of
      [] -> NeutralElement (T.pack name) r1
      lst -> ModifyElement (T.pack name) r1 $ unpackDiff lst
         where unpackDiff = fmap (\(a, t) -> (a, fmap decodeUtf8Lazy t))

maySubTree :: Git -> (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref
           -> ExceptT String IO CommitTreeDiff
maySubTree repository f name r = do
  sub <- liftIO $ accessObject repository r
  -- | Sometimes, there is nothing to see
  case sub of
    Nothing -> return $ f (T.pack name) r
    Just el  -> batchSubTree repository f name r el

toAdditions :: IgnoredSet -> FilePath -> [(SubKind, BC.ByteString)] -> [CommitTreeDiff]
toAdditions ignoreSet name lst =
  [AddElement (T.pack fullName) nullRef
        | (KindFile, fname) <- lst
        , let fullName = name </> BC.unpack fname 
              fullNameCheck = name <///> BC.unpack fname
        , not $ isPathIgnored ignoreSet fullNameCheck]

toDeletions :: Git -> FilePath -> [TreeEnt] -> ExceptT String IO [CommitTreeDiff]
toDeletions repository name lst = sequence
    [maySubTree repository DelElement fullName r
        | (_, item, r) <- lst
        , let fullName = name </> BC.unpack (toBytes item) ]

diffTree :: Git -> Int -> IgnoredSet -> UTCTime -> FilePath -> Object -> Ref
         -> ExceptT String IO CommitTreeDiff
diffTree repository contextSize ignoreSet maxTime upper = diffObject upper "" "" where
  go flatName name lefts rights = case (lefts, rights) of
    ([], []) -> return []
    ([], lst) -> return $ toAdditions ignoreSet name lst
    (lst, []) -> toDeletions repository name lst
    (ll, rr) -> compareDirectoryContent flatName name ll rr

  diffObject flatname name elemName obj r = case obj of
     ObjTree (Tree left) -> goTree flatname name elemName left r
     ObjBlob (Blob c1) -> goObj flatname elemName c1 r
     _ -> throwE "Wrong git object kind"

  goTree flatname name elemName lefts r = do
    rights <- liftIO $ fetchDirectoryInfo flatname
    let sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) lefts
        sortedFolder = sortBy (\(_,a) (_,b) -> compare a b) rights
        after = go flatname name sortedLeft sortedFolder
    TreeElement (T.pack elemName) r <$> after
            
  goObj flatname name c r = do
    file2 <- liftIO $ L.readFile flatname
    pure $! fileComparer contextSize name c r file2

  compareDirectoryContent _ _ [] _ = error "Impossible"
  compareDirectoryContent  _ _ ((_, _, _) : _) [] = error "Impossible"
  compareDirectoryContent flatname name lefts@((_, lName, lRef):ls) rights@((_, rName):rs)
    | rName `elem` [".", "..", ".git"] = go flatname name lefts rs
    | toBytes lName == rName = do
        -- We try to prune scanning if possible
        let thisElem = NeutralElement (decodeUtf8 $ toBytes lName) lRef
        !wasModified <- liftIO $ wasEdited repository maxTime thisName
        if not wasModified then (thisElem :) <$> go flatname name ls rs else do
        maySubL <- liftIO $ accessObject repository lRef
        case maySubL of
          -- This case should happen in presence of submodules.
          Nothing -> (thisElem :) <$> go flatname name ls rs
          Just subL ->
            let flat' = flatname </> BC.unpack (toBytes lName)
                subInfo = diffObject flat' thisName elemName subL lRef
                subInfoWrapped = ((:) <$> subInfo) `catchE` (\_ -> return id)
            in
            subInfoWrapped <*> go flatname name ls rs

    | isPathIgnored ignoreSet thisName = go flatname name lefts rs
    | toBytes lName < rName =
        (:) <$> maySubTree repository DelElement (BC.unpack $ toBytes lName) lRef
            <*> go flatname name ls rights

    | otherwise = (AddElement (decodeUtf8 rName) lRef:) <$> go flatname name lefts rs
    where
      elemName = BC.unpack (toBytes rName)
      thisName = name <///> elemName

diffWorkingDirectory :: Git -> IgnoredSet -> Int -> Ref
                     -> ExceptT String IO (Commit, CommitTreeDiff)
diffWorkingDirectory repository ignoreSet contextSize ref = do
    (ObjCommit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let authorStamp = timeOfAuthor $ commitAuthor thisCommit
        commitStamp = timeOfAuthor $ commitCommitter thisCommit
        maxStamp = min commitStamp authorStamp
        utcTime = posixSecondsToUTCTime $ realToFrac maxStamp
        upperPath = unpackPath repository ++ "/.."
        getObj reason = errorIO reason . accessObject repository
    thisTree <- getObj "Error can't access commit tree" $ commitTreeish thisCommit
    (thisCommit ,) <$> 
        diffTree repository contextSize ignoreSet utcTime upperPath thisTree (commitTreeish thisCommit) 



