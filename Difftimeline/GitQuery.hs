module GitQuery where

import Prelude

import System.FilePath( splitDirectories )
import System.IO( stderr, hPutStrLn )
import Control.Applicative
import Control.Monad.IO.Class( liftIO )
import Control.Monad.Trans.Maybe( MaybeT, runMaybeT  )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.List( find )
import Data.Maybe( fromJust )
import qualified Data.Text as T
import Data.Text.Encoding( decodeUtf8 )

import Data.Git( GitObject( .. )
               , CommitInfo( .. )
               , Git
               , findObject
               , CommitInfo( .. )
               , readBranch
               , getBranchNames
               )

import Data.Git.Ref( Ref, fromHexString )

import Diff

import Yesod.Logger

data CommitPath = CommitPath
    { pathCommitRef     :: Ref
    , pathParentRef     :: Ref
    , pathMessage       :: T.Text
    }
    deriving (Eq, Show)

maybeIO :: IO (Maybe a) -> MaybeT IO a
maybeIO a = do
    mayrez <- liftIO a
    case mayrez of
        Nothing -> fail undefined
        Just j  -> return j


findFirstCommit :: Git              -- ^ Repository
                -> [B.ByteString]   -- ^ Path
                -> Ref              -- ^ Ref of the element in the path
                -> Ref              -- ^ First commit ref
                -> IO (CommitInfo, Ref, [CommitPath])
findFirstCommit repository path currentFileRef firstCommit =
  fromJust <$> (runMaybeT $ inner undefined undefined firstCommit)
    where getObj = maybeIO . accessObject repository
  
          inner prevCommit prevRef currentCommit = do
            (Commit info) <- getObj currentCommit
            t@(Tree _)    <- getObj $ commitTree info
            commitFileRef <- maybeIO $ findInTree repository path t
       
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

findParentFile :: Git -> String -> String -> FilePath -> IO (Maybe ParentFile)
findParentFile repository lastFileStrSha commitStrSha path = runMaybeT $ inner
  where prevFileSha = fromHexString lastFileStrSha
        prevCommit = fromHexString commitStrSha

        bytePath = map BC.pack $ splitDirectories path

        getObj = maybeIO . accessObject repository

        inner = do 
            Commit commit <- getObj prevCommit
            t@(Tree _)    <- getObj $ commitTree commit
            Blob prevFile <- getObj prevFileSha
            currentFileRef <- maybeIO $ findInTree repository bytePath t
            Blob file <- getObj currentFileRef 

            (firstNfo, firstRef, path) <-
                    liftIO $ findFirstCommit repository bytePath currentFileRef prevCommit

            let toStrict = B.concat . L.toChunks
                prevData = decodeUtf8 $ toStrict prevFile
                thisData = decodeUtf8 $ toStrict file

            return $ ParentFile
                { fileData = thisData
                , fileRef = currentFileRef
                , parentRef = commitParents firstNfo
                , fileMessage = decodeUtf8 $ commitMessage firstNfo
                , commitRef = firstRef
                , commitPath = path
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

basePage :: Logger -> Git -> [B.ByteString] -> IO (Maybe ParentFile)
basePage logger repository path = runMaybeT $ do
    let getObj = maybeIO . accessObject repository
    liftIO $ logString logger "OK before read branch"
    headLists <- liftIO $ getBranchNames repository
    liftIO . logString logger $ show headLists
    headRef        <- liftIO $ readBranch repository "master"
    liftIO . logString logger $ show headRef
    liftIO . hPutStrLn stderr $ "init ref : " ++ show headRef
    (Commit cInfo) <- getObj headRef
    tree           <- getObj $ commitTree cInfo
    foundFileRef   <- maybeIO $ findInTree repository path tree
    (Blob content) <- getObj foundFileRef

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

