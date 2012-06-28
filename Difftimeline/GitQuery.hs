{-# LANGUAGE OverloadedStrings #-}
module Difftimeline.GitQuery( CommitTreeDiff( .. )
                            , CommitDetail( .. )
                            , CommitPath( .. )
                            , ParentFile( .. )
                            , CommitOverview( .. )

                            , diffCommit
                            , diffCommitTree
                            , findFirstCommit
                            , findParentFile
                            , basePage
                            , decodeUtf8
                            , commitList


                            -- * Manipulation functions
                            , flattenTreeDiff
                            , filterCommitTreeDiff
                            ) where

import Prelude

import Data.List( sortBy, foldl' )
import Data.Monoid( mempty, mappend )
import System.FilePath( splitDirectories  )
import Control.Applicative
import Control.Monad.Error( ErrorT, throwError, runErrorT, catchError )
import Control.Monad.IO.Class( liftIO )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.List( find )
import qualified Data.Text as T
import Data.Text.Encoding( decodeUtf8With )
import Data.Text.Encoding.Error( lenientDecode )

import Data.Git( GitObject( .. )
               , CommitAuthor( .. )
               , CommitInfo( .. )
               , Git
               , findObject
               , CommitInfo( .. )
               , getHead
               , Ref
               , fromHexString
               , toHexString

               , TreeEntry
               )

import qualified Data.Vector as V

import Difftimeline.Diff

import Yesod.Logger

decodeUtf8 :: B.ByteString -> T.Text
decodeUtf8 = decodeUtf8With lenientDecode

data CommitTreeDiff = AddElement T.Text Ref
                    | DelElement T.Text Ref
                    | NeutralElement T.Text Ref
                    | TreeElement T.Text Ref [CommitTreeDiff]
                    | ModifyElement T.Text Ref [(DiffCommand, V.Vector T.Text)]
                    | ModifyBinaryElement T.Text Ref
                    deriving (Eq, Show)

joinBytePath :: [BC.ByteString] -> T.Text
joinBytePath = foldl' (<//>) mempty
    where (<//>) t n = t `mappend` T.pack "/" `mappend` (decodeUtf8 n)

flattenTreeDiff :: CommitTreeDiff -> [CommitTreeDiff]
flattenTreeDiff = starter
    where (<//>) t n = t `mappend` T.pack "/" `mappend` n
          inner n (AddElement t r) = [AddElement (n <//> t) r]
          inner n (DelElement t r) = [DelElement (n <//> t) r]
          inner n (NeutralElement t r) = [NeutralElement (n <//> t) r]
          inner n (ModifyElement t r diff) = [ModifyElement (n <//> t) r diff]
          inner n (ModifyBinaryElement t r) = [ModifyBinaryElement (n <//> t) r]
          inner n (TreeElement t _ sub) = concatMap (inner $ n <//> t) sub

          starter (AddElement t r) = [AddElement t r]
          starter (DelElement t r) = [DelElement t r]
          starter (NeutralElement t r) = [NeutralElement t r]
          starter (ModifyElement t r diff) = [ModifyElement t r diff]
          starter (ModifyBinaryElement t r) = [ModifyBinaryElement t r]
          starter (TreeElement t _ sub) = concatMap (inner t) sub

filterCommitTreeDiff :: (CommitTreeDiff -> Bool) -> CommitTreeDiff
                     -> [CommitTreeDiff]
filterCommitTreeDiff f = inner
    where inner a@(AddElement _ _) | f a = [a]
          inner a@(DelElement _ _) | f a = [a]
          inner a@(NeutralElement _ _) | f a = [a]
          inner a@(ModifyElement _ _ _) | f a = [a]
          inner a@(TreeElement n r sub) | f a =
                [TreeElement n r $ concatMap inner sub]
          inner _ = []

data CommitDetail = CommitDetail
    { commitDetailMessage :: T.Text
    , commitDetailParents :: [Ref]
    , commitDetailKey     :: Ref
    , commitDetailTimestamp :: Int
    , commitDetailTimezone  :: Int
    , commitDetailAuthor  :: T.Text
    , commitDetailChanges :: [CommitTreeDiff]
    }
    deriving Show

data CommitPath = CommitPath
    { pathCommitRef     :: Ref
    , pathParentRef     :: Ref
    , pathMessage       :: T.Text
    , pathTimestamp     :: Int
    , pathTimezone      :: Int
    , pathAuthor        :: T.Text
    }

-- | Try to detect binary element given a blob
detectBinary :: L.ByteString -> Bool
detectBinary = L.any (== 0) . L.take binaryDetectionSize
    where binaryDetectionSize = 8 * 1024


-- | Want same behaviour between windows & Unix
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ "/" ++ b

errorIO :: String -> IO (Maybe a) -> ErrorT String IO a
errorIO str a = do
    mayrez <- liftIO a
    case mayrez of
        Nothing -> throwError str
        Just j  -> return j

accessCommit :: String -> Git -> Ref -> ErrorT String IO GitObject
accessCommit s rep ref = do
    rez <- liftIO $ accessObject rep ref
    case rez of
        Nothing -> throwError s
        Just c@(Commit _) -> return c
        Just _ -> throwError s

accessTree :: String -> Git -> Ref -> ErrorT String IO GitObject
accessTree s rep ref = do
    rez <- liftIO $ accessObject rep ref
    case rez of
        Nothing -> throwError s
        Just c@(Tree _) -> return c
        Just _ -> throwError s

accessBlob :: String -> Git -> Ref -> ErrorT String IO GitObject
accessBlob s rep ref = do
    rez <- liftIO $ accessObject rep ref
    case rez of
        Nothing -> throwError s
        Just c@(Blob _) -> return c
        Just _ -> throwError s

nullRef :: Ref
nullRef = fromHexString $ replicate 40 '0'

batchSubTree :: Git -> (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref -> GitObject
             -> ErrorT String IO CommitTreeDiff
batchSubTree  repository f = aux
    where objAccess = errorIO "" . accessObject repository

          aux name r (Blob _) = return $ f (T.pack name) r
          aux name r (Tree t) = TreeElement (T.pack name) r <$>
            sequence [objAccess subRef >>= (aux (BC.unpack n) subRef)
                                        | (_, n, subRef) <- t]
          aux _ _ _ = throwError "Wrong git object kind"

diffCommitTree :: Git -> Ref -> IO (Either String CommitTreeDiff)
diffCommitTree repository ref = liftA snd <$> createCommitDiff repository 0 False ref

createCommitDiff :: Git -> Int -> Bool -> Ref
                 -> IO (Either String (CommitInfo, CommitTreeDiff))
createCommitDiff repository contextSize deep ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let prevRef = head $ commitParents thisCommit
    (Commit prevCommit) <- accessCommit "Error can't file parent commit" repository prevRef
    thisTree <- getObj "Error can't access commit tree" $ commitTree thisCommit
    prevTree <- getObj "Error can't access previous commit tree" $ commitTree prevCommit
    (,) thisCommit <$> inner "" prevTree (commitTree prevCommit) thisTree (commitTree thisCommit)
  where getObj reason = errorIO reason . accessObject repository
        inner name (Tree left) _r1 (Tree right) _r2 = 
          TreeElement (T.pack name) nullRef <$> diffTree name sortedLeft sortedRight
            where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
                  sortedRight = sortBy (\(_,a,_) (_,b,_) -> compare a b) right

        inner name (Blob c1) r1 (Blob c2) r2
            | r1 == r2  = return $ NeutralElement (T.pack name) r1
            | not deep  = return $ ModifyElement (T.pack name) r2 []
            | detectBinary c2  = return $ ModifyBinaryElement (T.pack name) r2
            | otherwise = return .
                ModifyElement (T.pack name) r2 $ computeTextScript contextSize txtLeft txtRight
                    where strictify = B.concat . L.toChunks
                          txtLeft = decodeUtf8 $ strictify c1
                          txtRight = decodeUtf8 $ strictify c2
        inner _ _ _ _ _ = throwError "Wrong git object kind"

        maySubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref
                   -> ErrorT String IO CommitTreeDiff
        maySubTree f name r = do
            sub <- liftIO $ accessObject repository r
            -- | Sometimes, there is nothing to see
            case sub of
               Nothing -> return $ f (T.pack name) r
               Just el  -> batchSubTree repository f name r el

        diffTree :: String -> [TreeEntry] -> [TreeEntry]
                 -> ErrorT String IO [CommitTreeDiff]
        diffTree _name   []     [] = return []
        diffTree name    [] rights = sequence
            [maySubTree AddElement fullName r | (_, item, r) <- rights
                                              , let fullName = name </> BC.unpack item ]
        diffTree name lefts     [] = sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = name </> BC.unpack item ]
        diffTree name lefts@((_, lName, lRef):ls) rights@((_, rName, rRef):rs)
            | lName == rName = do
                maySubL <- liftIO $ accessObject repository lRef
                maySubR <- liftIO $ accessObject repository rRef

                case (maySubL, maySubR) of
                  -- This case should happen in presence of submodules.
                  (Nothing, Nothing) -> (thisElem :) <$> diffTree name ls rs
                        where thisElem | rRef == rRef = NeutralElement (decodeUtf8 lName) lRef
                                       | otherwise = ModifyElement (decodeUtf8 lName) lRef []

                  (Just subL, Just subR) ->
                        (((:) <$> inner (BC.unpack lName) subL lRef subR rRef)
                                `catchError` (\_ -> return id)) <*> diffTree name ls rs
                  (Nothing, _) ->
                      throwError $ "Cannot fetch parent sub tree (" ++ toHexString lRef ++ ")"

                  (_, Nothing) ->
                      throwError $ "Cannot fetch this sub tree (" ++ toHexString rRef ++ ")"

            | lName < rName =
                (:) <$> maySubTree DelElement (BC.unpack lName) lRef
                    <*> diffTree name ls rights

            | otherwise = (:) <$> maySubTree AddElement (BC.unpack rName) rRef
                              <*> diffTree name lefts rs

filterCommitModifications :: [CommitTreeDiff] -> [CommitTreeDiff]
filterCommitModifications = filter isModification
    where isModification (AddElement _ _) = True
          isModification (DelElement _ _) = True
          isModification (NeutralElement _ _) = False
          isModification (ModifyElement _ _ _) = True
          isModification (ModifyBinaryElement _ _)= True
          isModification (TreeElement _ _ _) = False

diffCommit :: Git -> Int -> Bool -> Ref -> IO (Either String CommitDetail)
diffCommit repository contextSize deep ref =
   detailer <$> createCommitDiff  repository contextSize deep ref
    where detailer (Left err) = Left err
          detailer (Right (thisCommit, diff)) = 
            let author = commitAuthor thisCommit
            in Right $ CommitDetail {
                  commitDetailMessage = decodeUtf8 $ commitMessage thisCommit
                , commitDetailParents = commitParents thisCommit
                , commitDetailKey     = ref
                , commitDetailAuthor  = decodeUtf8 $ authorName author
                , commitDetailTimestamp = authorTimestamp author
                , commitDetailTimezone  = authorTimezone author
                , commitDetailChanges = filterCommitModifications $ flattenTreeDiff diff
                }

findFirstCommit :: Git              -- ^ Repository
                -> [B.ByteString]   -- ^ Path
                -> Ref              -- ^ Ref of the element in the path
                -> Ref              -- ^ First commit ref
                -> ErrorT String IO (CommitInfo, Ref, [CommitPath])
findFirstCommit repository path currentFileRef firstCommit = inner undefined undefined firstCommit
    where inner prevCommit prevRef currentCommit = do
            (Commit info) <- accessCommit "Can't file commit in commit path" repository currentCommit
            t@(Tree _)    <- accessTree "Can't find tree in commit path" repository $ commitTree info
            catchError (do
                commitFileRef <- errorIO "Can't find children in commit path"
                                        $ findInTree repository path t

                if commitFileRef /= currentFileRef
                    then return (prevCommit, prevRef, [])
                    else do
                        (obj, r, commitPathRest) <- inner info currentCommit $ commitParents info !! 0
                        let author = commitAuthor info
                        return (obj, r, CommitPath {
                                pathCommitRef = currentCommit,
                                pathParentRef = (commitParents info) !! 0,
                                pathMessage = decodeUtf8 $ commitMessage info,
                                pathAuthor = decodeUtf8 $ authorName author,
                                pathTimestamp = authorTimestamp author,
                                pathTimezone = authorTimezone author
                            } : commitPathRest))
                            (\_ -> return (prevCommit, prevRef, []))

accessObject :: Git -> Ref -> IO (Maybe GitObject)
accessObject g r = findObject g r

-- | Given a Tree object, try to find a path in it.
-- This function should not call error
findInTree :: Git -> [B.ByteString] -> GitObject -> IO (Maybe Ref)
findInTree git pathes = inner pathes . Just
    where inner _                Nothing = return Nothing
          inner []                     _ = return Nothing
          inner [lp]   (Just (Tree lst)) = return $ findVal lp lst
          inner (x:xs) (Just (Tree lst)) = case findVal x lst of
                    Nothing -> return Nothing
                    Just r  -> accessObject git r >>= inner xs
          inner _ _                      = return Nothing

          extractRef (_, _, ref) = ref
          findVal v lst = extractRef <$> find (\(_, n, _) -> v == n) lst

findParentFile :: Git -> String -> String -> FilePath -> IO (Either String ParentFile)
findParentFile repository lastFileStrSha commitStrSha path = runErrorT $ inner
  where prevFileSha = fromHexString lastFileStrSha
        prevCommit = fromHexString commitStrSha

        bytePath = map BC.pack $ splitDirectories path

        inner = do
            Commit commit <- accessCommit "Can't find parent commit" repository prevCommit
            t@(Tree _)    <- accessTree "Can't find tree commit" repository $ commitTree commit
            Blob prevFile <- accessBlob "Can't find file content" repository prevFileSha
            currentFileRef <- errorIO "Can't find current content" $ findInTree repository bytePath t
            Blob file <- accessBlob "Can't find file content" repository $ currentFileRef
            (firstNfo, firstRef, betweenCommits) <-
                    findFirstCommit repository bytePath currentFileRef prevCommit

            let toStrict = B.concat . L.toChunks
                prevData = decodeUtf8 $ toStrict prevFile
                thisData = decodeUtf8 $ toStrict file
                author = commitAuthor firstNfo

            return $ ParentFile
                { fileData = T.filter (/= '\r') thisData
                , fileName = T.pack path
                , fileRef = currentFileRef
                , parentRef = commitParents firstNfo
                , fileMessage = decodeUtf8 $ commitMessage firstNfo
                , commitRef = firstRef
                , commitPath = reverse betweenCommits
                , fileDiff = computeTextDiff thisData prevData
                , parentCommitAuthor = decodeUtf8 $ authorName author
                , parentCommitTimestamp = authorTimestamp author
                , parentCommmitTimezone = authorTimezone author
                }

data ParentFile = ParentFile
    { fileData    :: T.Text
    , fileName    :: T.Text
    , fileRef     :: Ref
    , parentRef   :: [Ref]
    , fileMessage :: T.Text
    , commitRef   :: Ref
    , commitPath  :: [CommitPath]
    , fileDiff    :: [DiffCommand]
    , parentCommitAuthor :: T.Text
    , parentCommitTimestamp :: Int
    , parentCommmitTimezone :: Int
    }

data CommitOverview = CommitOverview
    { commitOverviewParent    :: [Ref]
    , commitOverviewMessage   :: T.Text
    , commitOverviewRef       :: Ref
    , commitOverviewAuthor    :: T.Text
    , commitOverviewTimestamp :: Int
    }

commitList :: Git -> Int -> Ref -> IO (Either String [CommitOverview])
commitList repository count = runErrorT . inner count
    where inner 0 _ = return []
          inner n _ | n < 0 = return []
          inner n r = do
              Commit commit <- accessCommit "" repository r
              (prepare r commit :) <$> inner (n - 1) (head $ commitParents commit)

          prepare r c = CommitOverview
              { commitOverviewParent = commitParents c
              , commitOverviewMessage = decodeUtf8 $ commitMessage c
              , commitOverviewRef = r
              , commitOverviewAuthor = decodeUtf8 . authorName $ commitAuthor c
              , commitOverviewTimestamp = authorTimestamp $ commitAuthor c
              }

basePage :: Logger -> Git -> [B.ByteString] -> IO (Either String ParentFile)
basePage _logger repository path = runErrorT $ do
    let getObj errorReason = errorIO errorReason . accessObject repository
    headRef        <- errorIO "Can't read HEAD" $ getHead repository
    (Commit cInfo) <- accessCommit "Error can't access commit" repository headRef
    tree           <- getObj "Error can't access commit tree" $ commitTree cInfo
    foundFileRef   <- errorIO "Error can't find file in tree" $ findInTree repository path tree
    (Blob content) <- getObj "Error can't find file content" foundFileRef

    let toStrict = B.concat . L.toChunks
        author = commitAuthor cInfo

    return $ ParentFile {
    	commitRef = headRef,
    	fileRef = foundFileRef,
    	fileName = joinBytePath path,
    	commitPath = [CommitPath { pathCommitRef = headRef
                                 , pathParentRef = head $ commitParents cInfo
                                 , pathMessage = decodeUtf8 $ commitMessage cInfo
                                 , pathTimestamp = authorTimestamp author
                                 , pathTimezone = authorTimezone author
                                 , pathAuthor = decodeUtf8 $ authorName author
                                 }],
    	fileDiff = [],
    	fileData = decodeUtf8 $ toStrict content,
    	parentRef = commitParents cInfo,
    	fileMessage = decodeUtf8 $ commitMessage cInfo,
        parentCommitAuthor = decodeUtf8 $ authorName author,
        parentCommitTimestamp = authorTimestamp author,
        parentCommmitTimezone = authorTimezone author
    }

