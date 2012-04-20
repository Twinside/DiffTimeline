module GitQuery( CommitTreeDiff( .. )
               , CommitDetail( .. )
               , CommitPath( .. )
               , ParentFile( .. )
               , diffCommit
               , findFirstCommit
               , findParentFile 
               , basePage
               ) where

import Prelude

import Data.Monoid( mappend )
import System.FilePath( splitDirectories  )
import System.IO( stderr, hPutStrLn )
import Control.Applicative
import Control.Monad.Error( ErrorT, throwError, runErrorT )
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
               )

import qualified Data.Vector as V
import Data.Git.Ref( Ref, fromHexString )

import Diff

import Yesod.Logger

decodeUtf8 :: B.ByteString -> T.Text
decodeUtf8 = decodeUtf8With lenientDecode

data CommitTreeDiff = AddElement T.Text Ref
                    | DelElement T.Text Ref
                    | ModifyElement T.Text Ref [(DiffCommand, V.Vector T.Text)]
                    deriving (Eq, Show)

data CommitDetail = CommitDetail
    { commitDetailMessage :: T.Text
    , commitDetailParents :: [Ref]
    , commitDetailKey     :: Ref
    , commitDetailAuthor  :: T.Text
    , commitDetailChanges :: [CommitTreeDiff]
    }
    deriving (Eq, Show)

data CommitPath = CommitPath
    { pathCommitRef     :: Ref
    , pathParentRef     :: Ref
    , pathMessage       :: T.Text
    }
    deriving (Eq, Show)

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

diffCommit :: Git -> Bool -> Ref -> IO (Either String CommitDetail)
diffCommit repository deep ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let prevRef = head $ commitParents thisCommit
    (Commit prevCommit) <- accessCommit "Error can't file parent commit" repository prevRef
    thisTree <- getObj "Error can't access commit tree" $ commitTree thisCommit
    prevTree <- getObj "Error can't access previous commit tree" $ commitTree prevCommit
    diff <- inner "" thisTree (commitTree prevCommit) prevTree (commitTree thisCommit)
    return (CommitDetail {
          commitDetailMessage = decodeUtf8 $ commitMessage thisCommit
        , commitDetailParents = commitParents thisCommit
        , commitDetailKey     = ref
        , commitDetailAuthor  = decodeUtf8 . authorName $ commitAuthor thisCommit
        , commitDetailChanges = diff
        })
  where getObj reason = errorIO reason . accessObject repository
        inner name (Tree left) _r1 (Tree right) _r2 = diffTree name left right
        inner name (Blob c1) _r1 (Blob c2) r2
            | not deep  = return [ModifyElement (T.pack name) r2 []]
            | otherwise = return
                [ModifyElement (T.pack name) r2 $ computeTextScript txtLeft txtRight]
                    where strictify = B.concat . L.toChunks
                          txtLeft = decodeUtf8 $ strictify c1
                          txtRight = decodeUtf8 $ strictify c2
        inner _ _ _ _ _ = return []

        diffTree _name   []     [] = return []
        diffTree name    [] rights = return
            [AddElement fullName r | (_, item, r) <- rights
                                  , let fullName = T.pack $ name </> BC.unpack item ]
        diffTree name lefts     [] = return
            [DelElement fullName r | (_, item, r) <- lefts
                                  , let fullName = T.pack $ name </> BC.unpack item ]
        diffTree name ((_, lName, lRef):ls) ((_, rName, rRef):rs)
            | lName == rName && lRef == rRef = diffTree name ls rs
            | lName == rName = do
                subL <- errorIO "Error can't access parent commit subtree"
                                $ accessObject repository lRef
                subR <- errorIO "Error can't acces commit subtree"
                                $ accessObject repository rRef
                mappend <$> inner subname subL lRef subR rRef <*> diffTree name ls rs

            | lName < rName = return [DelElement delName lRef]
            | otherwise = return [AddElement addName rRef]
                where delName = T.pack $ name </> BC.unpack lName
                      addName =  T.pack $ name </> BC.unpack rName
                      subname = name </> BC.unpack lName

findFirstCommit :: Git              -- ^ Repository
                -> [B.ByteString]   -- ^ Path
                -> Ref              -- ^ Ref of the element in the path
                -> Ref              -- ^ First commit ref
                -> ErrorT String IO (CommitInfo, Ref, [CommitPath])
findFirstCommit repository path currentFileRef firstCommit = inner undefined undefined firstCommit
    where inner prevCommit prevRef currentCommit = do
            (Commit info) <- accessCommit "Can't file commit in commit path" repository currentCommit
            t@(Tree _)    <- accessTree "Can't find tree in commit path" repository $ commitTree info
            commitFileRef <- errorIO "Can't find children in commit path"
                                     $ findInTree repository path t
       
            if commitFileRef /= currentFileRef
               then return (prevCommit, prevRef, [])
               else do
               	(obj, r, commitPathRest) <- inner info currentCommit $ commitParents info !! 0
               	return (obj, r, CommitPath {
                        pathCommitRef = currentCommit,
                        pathParentRef = (commitParents info) !! 0,
                        pathMessage = decodeUtf8 $ commitMessage info
                    } : commitPathRest)
      
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

            return $ ParentFile
                { fileData = thisData
                , fileRef = currentFileRef
                , parentRef = commitParents firstNfo
                , fileMessage = decodeUtf8 $ commitMessage firstNfo
                , commitRef = firstRef
                , commitPath = betweenCommits
                , fileDiff = computeTextDiff thisData prevData
                }

data ParentFile = ParentFile
    { fileData    :: T.Text
    , fileRef     :: Ref
    , parentRef   :: [Ref]
    , fileMessage :: T.Text
    , commitRef   :: Ref
    , commitPath  :: [CommitPath]
    , fileDiff    :: [DiffCommand]
    }
    deriving (Eq, Show)

basePage :: Logger -> Git -> [B.ByteString] -> IO (Either String ParentFile)
basePage logger repository path = runErrorT $ do
    let getObj errorReason = errorIO errorReason . accessObject repository
    headRef        <- errorIO "Can't read HEAD" $ getHead repository
    liftIO . logString logger $ "init ref : " ++ show headRef
    (Commit cInfo) <- accessCommit "Error can't access commit" repository headRef
    tree           <- getObj "Error can't access commit tree" $ commitTree cInfo
    foundFileRef   <- errorIO "Error can't find file in tree" $ findInTree repository path tree
    (Blob content) <- getObj "Error can't find file content" foundFileRef

    let toStrict = B.concat . L.toChunks
    return $ ParentFile {
    	commitRef = headRef,
    	fileRef = foundFileRef,
    	commitPath = [],
    	fileDiff = [],
    	fileData = decodeUtf8 $ toStrict content,
    	parentRef = commitParents cInfo,
    	fileMessage = decodeUtf8 $ commitMessage cInfo
    
    }

