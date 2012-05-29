module Difftimeline.GitQuery( CommitTreeDiff( .. )
                            , CommitDetail( .. )
                            , CommitPath( .. )
                            , ParentFile( .. )
                            , diffCommit
                            , diffCommitTree
                            , findFirstCommit
                            , findParentFile
                            , basePage
                            , decodeUtf8


                            -- * Manipulation functions
                            , flattenTreeDiff
                            , filterCommitTreeDiff
                            ) where

import Prelude

import Data.List( sortBy )
import Data.Monoid( mempty, mappend )
import System.FilePath( splitDirectories  )
import Control.Applicative
import Control.Monad.Error( ErrorT, throwError, runErrorT, catchError )
import Control.Monad.IO.Class( liftIO )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Aeson
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
               , toHex

               , TreeEntry
               )

import qualified Data.Vector as V

import Difftimeline.Diff

import Yesod.Logger

decodeUtf8 :: B.ByteString -> T.Text
decodeUtf8 = decodeUtf8With lenientDecode

refToText :: Ref -> T.Text
refToText = decodeUtf8 . toHex

data CommitTreeDiff = AddElement T.Text Ref
                    | DelElement T.Text Ref
                    | NeutralElement T.Text Ref
                    | TreeElement T.Text Ref [CommitTreeDiff]
                    | ModifyElement T.Text Ref [(DiffCommand, V.Vector T.Text)]
                    | ModifyBinaryElement T.Text Ref
                    deriving (Eq, Show)

flattenTreeDiff :: CommitTreeDiff -> [CommitTreeDiff]
flattenTreeDiff = inner mempty
    where (<//>) t n = t `mappend` T.pack "/" `mappend` n
          inner n (AddElement t r) = [AddElement (n <//> t) r]
          inner n (DelElement t r) = [DelElement (n <//> t) r]
          inner n (NeutralElement t r) = [NeutralElement (n <//> t) r]
          inner n (ModifyElement t r diff) = [ModifyElement (n <//> t) r diff]
          inner n (ModifyBinaryElement t r) = [ModifyBinaryElement (n <//> t) r]
          inner n (TreeElement t _ sub) = concatMap (inner $ n <//> t) sub

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

instance ToJSON CommitTreeDiff where
    toJSON (TreeElement name r children) =
        object [ "kind" .= ("neutral" :: T.Text)
               , "name" .= name
               , "hash" .= toHexString r
               , "children" .= children
               ]

    toJSON (NeutralElement name r) =
        object [ "kind" .= ("neutral" :: T.Text)
               , "name" .= name
               , "hash" .= toHexString r]

    toJSON (AddElement name r) =
        object [ "kind" .= ("addition" :: T.Text)
               , "name" .= name
               , "hash" .= toHexString r]

    toJSON (DelElement name r) =
        object [ "kind" .= ("deletion" :: T.Text)
               , "name" .= name
               , "hash" .= toHexString r]

    toJSON (ModifyBinaryElement name r) =
        object $ [ "kind" .= ("modification" :: T.Text)
                 , "name" .= name
                 , "hash" .= toHexString r
                 , "binary" .= True
                 ]

    toJSON (ModifyElement name r diffs) =
        object $ [ "kind"   .= ("modification" :: T.Text)
                 , "name"   .= name
                 , "hash"   .= toHexString r
                 , "binary" .= False
                 ] ++ diffElement
          where diffElement | null diffs = []
                            | otherwise = ["diff" .= map adder diffs]
                adder (d, content) = object $ diffToJson d ++ ["data" .= content]

data CommitDetail = CommitDetail
    { commitDetailMessage :: T.Text
    , commitDetailParents :: [Ref]
    , commitDetailKey     :: Ref
    , commitDetailTimestamp :: Int
    , commitDetailTimezone  :: Int
    , commitDetailAuthor  :: T.Text
    , commitDetailChanges :: [CommitTreeDiff]
    }
    deriving (Eq, Show)

instance ToJSON CommitDetail where
    toJSON detail = object $
      [ "message"      .= commitDetailMessage detail
      , "parents_sha"  .= (toJSON . map toHexString $ commitDetailParents detail)
      , "key"          .= (toHexString $ commitDetailKey detail)
      , "author"       .= commitDetailAuthor detail
      , "file_changes" .= toJSON (commitDetailChanges detail)
      , "timestamp"    .= commitDetailTimestamp detail
      , "timezone"     .= commitDetailTimezone detail
      ]


data CommitPath = CommitPath
    { pathCommitRef     :: Ref
    , pathParentRef     :: Ref
    , pathMessage       :: T.Text
    , pathTimestamp     :: Int
    , pathTimezone      :: Int
    , pathAuthor        :: T.Text
    }
    deriving (Eq, Show)

instance ToJSON CommitPath where
    toJSON cp =
      object [ "commit" .= refToText (pathCommitRef cp)
             , "parent_commit" .= refToText (pathParentRef cp)
             , "message" .= pathMessage cp
             , "author"    .= pathAuthor cp
             , "timestamp" .= pathTimestamp cp
             , "timezone"  .= pathTimezone cp
             ]

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

diffCommitTree :: Git -> Int -> Bool -> Ref -> IO (Either String CommitTreeDiff)
diffCommitTree repository contextSize deep ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let prevRef = head $ commitParents thisCommit
    (Commit prevCommit) <- accessCommit "Error can't file parent commit" repository prevRef
    thisTree <- getObj "Error can't access commit tree" $ commitTree thisCommit
    prevTree <- getObj "Error can't access previous commit tree" $ commitTree prevCommit
    inner "" prevTree (commitTree prevCommit) thisTree (commitTree thisCommit)
  where getObj reason = errorIO reason . accessObject repository
        inner name (Tree left) _r1 (Tree right) _r2 = 
          TreeElement (T.pack name) nullRef <$> diffTree name sortedLeft sortedRight
            where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
                  sortedRight = sortBy (\(_,a,_) (_,b,_) -> compare a b) right

        inner name (Blob c1) r1 (Blob c2) r2
            | r1 == r2  = return $ NeutralElement (T.pack name) r1
            | not deep  = return $ ModifyElement (T.pack name) r2 []
            | otherwise = return .
                ModifyElement (T.pack name) r2 $ computeTextScript contextSize txtLeft txtRight
                    where strictify = B.concat . L.toChunks
                          txtLeft = decodeUtf8 $ strictify c1
                          txtRight = decodeUtf8 $ strictify c2
        inner _ _ _ _ _ = throwError "Wrong git object kind"

        maySubTree f name r = do
            sub <- errorIO "Dip dup" $ accessObject repository r
            batchSubTree f name r sub

        batchSubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref -> GitObject
                     -> ErrorT String IO CommitTreeDiff
        batchSubTree f name r (Blob _) = return $ f (T.pack name) r
        batchSubTree f name r (Tree t) = TreeElement (T.pack name) r <$>
          sequence [objAccess subRef >>= (batchSubTree f (BC.unpack n) subRef)
                                            | (_, n, subRef) <- t]
            where objAccess = getObj "dipdup2"
        batchSubTree _ _ _ _ = throwError "Wrong git object kind"

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
            | lName == rName = ((do
                subL <- errorIO ("Error can't access parent commit subtree " ++ BC.unpack lName)
                                $ accessObject repository lRef
                subR <- errorIO ("Error can't acces commit subtree" ++ BC.unpack lName)
                                $ accessObject repository rRef
                (:) <$> inner (BC.unpack lName) subL lRef subR rRef)
                               `catchError` (\_ -> return id)) <*> diffTree name ls rs

            | lName < rName =
                (:) <$> maySubTree DelElement (BC.unpack lName) lRef
                    <*> diffTree name ls rights

            | otherwise = (:) <$> maySubTree AddElement (BC.unpack rName) rRef
                              <*> diffTree name lefts rs

diffCommit :: Git -> Int -> Bool -> Ref -> IO (Either String CommitDetail)
diffCommit repository contextSize deep ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let prevRef = head $ commitParents thisCommit
    (Commit prevCommit) <- accessCommit "Error can't file parent commit" repository prevRef
    thisTree <- getObj "Error can't access commit tree" $ commitTree thisCommit
    prevTree <- getObj "Error can't access previous commit tree" $ commitTree prevCommit
    diff <- inner "" prevTree (commitTree prevCommit) thisTree (commitTree thisCommit)
    let author = commitAuthor thisCommit
    return (CommitDetail {
          commitDetailMessage = decodeUtf8 $ commitMessage thisCommit
        , commitDetailParents = commitParents thisCommit
        , commitDetailKey     = ref
        , commitDetailAuthor  = decodeUtf8 $ authorName author
        , commitDetailTimestamp = authorTimestamp author
        , commitDetailTimezone  = authorTimezone author
        , commitDetailChanges = diff
        })
  where getObj reason = errorIO reason . accessObject repository
        inner name (Tree left) _r1 (Tree right) _r2 = diffTree name sortedLeft sortedRight
            where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
                  sortedRight = sortBy (\(_,a,_) (_,b,_) -> compare a b) right
        inner name (Blob c1) _r1 (Blob c2) r2
            | isBinary  = return [ModifyBinaryElement (T.pack name) r2]
            | not deep  = return [ModifyElement (T.pack name) r2 []]
            | otherwise = return
                [ModifyElement (T.pack name) r2 $ computeTextScript contextSize txtLeft txtRight]
                    where strictify = B.concat . L.toChunks
                          txtLeft = decodeUtf8 $ strictify c1
                          txtRight = decodeUtf8 $ strictify c2

                          isBinary = detectBinary c2

        inner _ _ _ _ _ = return []

        maySubTree f name r = do
            sub <- errorIO "Dip dup" $ accessObject repository r
            batchSubTree f name r sub

        batchSubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref -> GitObject
                     -> ErrorT String IO [CommitTreeDiff]
        batchSubTree f name r (Blob _) = return [f (T.pack name) r]
        batchSubTree f name _ (Tree t) =
          concat <$> sequence [objAccess r >>= (batchSubTree f (namer n) r) | (_, n, r) <- t]
            where objAccess = getObj "dipdup2"
                  namer n = name </> BC.unpack n
        batchSubTree _ _ _ _ = return []

        diffTree :: String -> [TreeEntry] -> [TreeEntry]
                 -> ErrorT String IO [CommitTreeDiff]
        diffTree _name   []     [] = return []
        diffTree name    [] rights = concat <$> sequence
            [maySubTree AddElement fullName r | (_, item, r) <- rights
                                              , let fullName = name </> BC.unpack item ]
        diffTree name lefts     [] = concat <$> sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = name </> BC.unpack item ]
        diffTree name lefts@((_, lName, lRef):ls) rights@((_, rName, rRef):rs)
            | lName == rName && lRef == rRef = diffTree name ls rs
            | lName == rName = do
                subL <- errorIO "Error can't access parent commit subtree"
                                $ accessObject repository lRef
                subR <- errorIO "Error can't acces commit subtree"
                                $ accessObject repository rRef
                mappend <$> inner subname subL lRef subR rRef <*> diffTree name ls rs

            | lName < rName = mappend <$> maySubTree DelElement addName lRef
                                      <*> diffTree name ls rights

            | otherwise = mappend <$> maySubTree AddElement delName rRef
                                  <*> diffTree name lefts rs
                where addName = name </> BC.unpack lName
                      delName = name </> BC.unpack rName
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
    deriving (Eq, Show)

instance ToJSON ParentFile where
    toJSON p =
      object ["data"          .= fileData p
             ,"filekey"       .= (refToText $ fileRef p)
             ,"parent_commit" .= (refToText . last $ parentRef p)
             ,"message"       .= fileMessage p
             ,"diff"          .= (toJSON . map (object . diffToJson) $ fileDiff p)
             ,"path"          .= toJSON (commitPath p)
             ,"key"           .= refToText (commitRef p)
             ,"author"        .= parentCommitAuthor p
             ,"timestamp"     .= parentCommitTimestamp p
             ,"timezone"      .= parentCommmitTimezone p
             ]

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
    	commitPath = [],
    	fileDiff = [],
    	fileData = decodeUtf8 $ toStrict content,
    	parentRef = commitParents cInfo,
    	fileMessage = decodeUtf8 $ commitMessage cInfo,
        parentCommitAuthor = decodeUtf8 $ authorName author,
        parentCommitTimestamp = authorTimestamp author,
        parentCommmitTimezone = authorTimezone author
    }

