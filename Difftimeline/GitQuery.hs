{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Difftimeline.GitQuery( CommitTreeDiff( .. )
                            , CommitDetail( .. )
                            , CommitPath( .. )
                            , ParentFile( .. )
                            , CommitOverview( .. )
                            , BranchInfo( .. )
                            , RemoteBranches( .. )

                            , brancheslist
                            , diffCommit
                            , diffBranches
                            , compareBranches
                            , diffCommitTree
                            , workingDirectoryChanges 
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
import Control.Monad( forM )
import Control.Monad.Error( ErrorT, throwError, runErrorT, catchError )
import Control.Monad.IO.Class( liftIO )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.List( find )
import qualified Data.Text as T
import Data.Text.Encoding( decodeUtf8With )
import Data.Text.Encoding.Error( lenientDecode )

import Data.Time.Clock.POSIX( getPOSIXTime )

import System.Directory( getDirectoryContents, doesFileExist )
import Data.Git( GitObject( .. )
               , CommitAuthor( .. )
               , CommitInfo( .. )
               , Git
               , findObject
               , gitRepoPath 
               , CommitInfo( .. )
               , getHead
               , Ref
               , fromHexString
               , toHexString
               , getBranchNames
               , getRemoteNames
               , getRemoteBranchNames
               , getTagNames
               , readRemoteBranch
               , readBranch
               , readTag

               , TreeEntry
               , revFromString
               , resolveRevision
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

compareBranches :: Git -> Int -> String -> String
                -> IO (Either String CommitDetail)
compareBranches repo contextSize b1 b2 = runErrorT $ do
    detailer <$> diffBranches repo contextSize b1 b2
  where detailer diff = CommitDetail
            { commitDetailMessage = T.pack $ "Comparing " ++ b1 ++ " and " ++ b2
            , commitDetailParents = [nullRef]
            , commitDetailKey     = nullRef
            , commitDetailAuthor  = ""
            , commitDetailTimestamp = 0
            , commitDetailTimezone  = 0
            , commitDetailChanges = filterCommitModifications
                                  $ flattenTreeDiff diff
            }

data BranchInfo = BranchInfo
    { branchName :: T.Text
    , branchRef  :: Ref
    }
    deriving Show

data RemoteBranches = RemoteBranches
    { remoteName     :: T.Text
    , remoteBranches :: [BranchInfo]
    }
    deriving Show

brancheslist :: Git -> IO [RemoteBranches]
brancheslist repo = do
    let fetchBranch b = BranchInfo (T.pack b) <$> readBranch repo b
        fetchRemoteBranch _remote "HEAD" = pure[]
        fetchRemoteBranch remote b =
            sequence [BranchInfo (T.pack $ remote </> b) <$> readRemoteBranch repo remote b]

        fetchTag b = BranchInfo (T.pack b) <$> readTag repo b
    branchInfo <- getBranchNames repo >>= mapM fetchBranch 
    remoteList <- getRemoteNames repo
    remotes <-  forM remoteList (\remote -> do
          branchesName <- getRemoteBranchNames repo remote 
          branches <- concat <$> mapM (fetchRemoteBranch remote) branchesName
          pure $ RemoteBranches { remoteName = T.pack remote
                                , remoteBranches = branches 
                                })

    tagInfo <- getTagNames repo >>= mapM fetchTag
    let localBranch = RemoteBranches {
        remoteName = "local",
        remoteBranches = branchInfo ++ tagInfo
    }
    pure $ localBranch : remotes

diffBranches :: Git -> Int -> String -> String -> ErrorT String IO CommitTreeDiff
diffBranches repo contextSize branch1 branch2 = do
    let fetchRef = liftIO . resolveRevision repo . revFromString
    ref1 <- fetchRef branch1
    ref2 <- fetchRef branch2
    case (ref1, ref2) of
      (Nothing,       _) -> throwError ("Error branch " ++ branch1 ++ " doesnt exist")
      (      _, Nothing) -> throwError ("Error branch " ++ branch2 ++ " doesnt exist")
      (Just b1Ref, Just b2Ref) ->
            snd <$> createCommitDiff  repo contextSize True b1Ref b2Ref

diffCommitTree :: Git -> Ref -> IO (Either String CommitTreeDiff)
diffCommitTree repository ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let prevRef = head $ commitParents thisCommit
    snd <$> createCommitDiff repository 0 False ref prevRef

data SubKind = KindFile | KindDirectory

fetchDirectoryInfo :: FilePath -> IO [(SubKind, BC.ByteString)]
fetchDirectoryInfo name = do
    files <- getDirectoryContents name
    sequence [(, BC.pack sub) <$> (kindOfExist <$> doesFileExist sub) | sub <- files ]
        where kindOfExist True = KindFile
              kindOfExist False = KindDirectory

workingDirectoryChanges  :: Git -> Int -> Ref -> IO (Either String CommitDetail)
workingDirectoryChanges repository contextSize ref = do
   time <- truncate <$> getPOSIXTime 
   detailer time <$> diffWorkingDirectory repository contextSize ref
    where detailer _ (Left err) = Left err
          detailer time (Right (_, diff)) =
            Right $ CommitDetail {
                  commitDetailMessage = "Working directory"
                , commitDetailParents = [ref]
                , commitDetailKey     = nullRef
                , commitDetailAuthor  = ""
                , commitDetailTimestamp = time
                , commitDetailTimezone  = 0
                , commitDetailChanges = filterCommitModifications
                                      $ flattenTreeDiff diff
                }


diffWorkingDirectory :: Git -> Int -> Ref -> IO (Either String (CommitInfo, CommitTreeDiff))
diffWorkingDirectory repository contextSize ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    thisTree <- getObj "Error can't access commit tree" $ commitTree thisCommit
    (,) thisCommit <$> inner (gitRepoPath repository ++ "/..") "" thisTree (commitTree thisCommit) 
  where getObj reason = errorIO reason . accessObject repository

        fileComparer name bin1 r1 bin2
            | bin1 == bin2 = NeutralElement (T.pack name) r1
            | detectBinary bin2 = ModifyBinaryElement (T.pack name) r1
            | otherwise = case computeTextScript contextSize txtLeft txtRight of
                    [] -> NeutralElement (T.pack name) r1
                    lst -> ModifyElement (T.pack name) r1 lst
                    where strictify = B.concat . L.toChunks
                          txtLeft = decodeUtf8 $ strictify bin1
                          txtRight = decodeUtf8 $ strictify bin2

        inner flatname name (Tree left) r = do
          right <- liftIO $ fetchDirectoryInfo flatname
          TreeElement (T.pack name) r <$> diffTree flatname name sortedLeft (sortFolder right)
            where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
                  sortFolder = sortBy (\(_,a) (_,b) -> compare a b)

        inner flatname name (Blob c1) r1 = do
            file2 <- liftIO $ L.readFile flatname
            pure $ fileComparer name c1 r1 file2
        inner _ _ _ _ = throwError "Wrong git object kind"

        maySubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref
                   -> ErrorT String IO CommitTreeDiff
        maySubTree f name r = do
            sub <- liftIO $ accessObject repository r
            -- | Sometimes, there is nothing to see
            case sub of
               Nothing -> return $ f (T.pack name) r
               Just el  -> batchSubTree repository f name r el

        diffTree :: String -> String -> [TreeEntry] -> [(SubKind, BC.ByteString)]
                 -> ErrorT String IO [CommitTreeDiff]
        diffTree _ _name   []     [] = return []
        diffTree _ name    [] rights = pure
            [AddElement (T.pack fullName) nullRef | (KindFile, fname) <- rights
                                                  , let fullName = name </> BC.unpack fname ]

        diffTree _flatname name lefts     [] = sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = name </> BC.unpack item ]
        diffTree flatname name lefts@((_, lName, lRef):ls) rights@((_, rName):rs)
            | rName `elem` [".", "..", ".git"] = diffTree flatname name lefts rs
            | lName == rName = do
                maySubL <- liftIO $ accessObject repository lRef

                case maySubL of
                  -- This case should happen in presence of submodules.
                  Nothing -> (thisElem :) <$> diffTree flatname name ls rs
                        where thisElem = NeutralElement (decodeUtf8 lName) lRef

                  Just subL ->
                        (((:) <$> inner (flatname </> BC.unpack lName) (BC.unpack lName) subL lRef)
                                `catchError` (\_ -> return id)) <*> diffTree flatname name ls rs

            | lName < rName =
                (:) <$> maySubTree DelElement (BC.unpack lName) lRef
                    <*> diffTree flatname name ls rights

            | otherwise = (AddElement (decodeUtf8 rName) lRef:) <$> 
                              diffTree flatname name lefts rs

createCommitDiff :: Git -> Int -> Bool -> Ref -> Ref
                 -> ErrorT String IO (CommitInfo, CommitTreeDiff)
createCommitDiff repository contextSize deep ref prevRef = do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
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
        diffTree _name    [] rights = sequence
            [maySubTree AddElement fullName r | (_, item, r) <- rights
                                              , let fullName = BC.unpack item]
        diffTree _name lefts     [] = sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = BC.unpack item ]
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
diffCommit repository contextSize deep ref = runErrorT $ do
   (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
   let prevRef = head $ commitParents thisCommit
   detailer <$> createCommitDiff  repository contextSize deep ref prevRef
    where detailer (thisCommit, diff) = 
            let author = commitAuthor thisCommit
            in CommitDetail {
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

