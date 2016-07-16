{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Difftimeline.GitQuery( CommitTreeDiff( .. )
                            , CommitDetail( .. )
                            , CommitPath( .. )
                            , ParentFile( .. )
                            , CommitOverview( .. )
                            , BranchInfo( .. )
                            , RemoteBranches( .. )
                            , FileComparison( .. )
                            , FileReference( .. )
                            , brancheslist
                            , diffCommit
                            , diffBranches
                            , compareBranches
                            , compareFiles
                            , diffCommitTree
                            , workingDirectoryChanges
                            , workingDirectoryChanges'
                            , findFirstCommit
                            , findParentFile
                            , basePage
                            , decodeUtf8
                            , commitList
                            , nullRef

                            -- * Blame Management
                            , blameFile
                            , BlameRangeSource( .. )
                            , BlameInfo( .. )


                            , diffWorkingDirectory 

                            -- * Manipulation functions
                            , flattenTreeDiff
                            , filterCommitTreeDiff

                            , errorIO
                            , getHead
                            ) where

import Prelude

import Data.List( foldl', find  )
import Data.Maybe( catMaybes )
import Data.Byteable( toBytes )
import System.FilePath( splitDirectories  )
import Control.Monad( forM, when, void )
import Control.Monad.Trans.Except( ExceptT, throwE, runExceptT, catchE )
import Control.Monad.IO.Class( liftIO )
import qualified Data.Set as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Writer.Strict( WriterT, runWriterT )

import Data.Time.Clock.POSIX( getPOSIXTime )

import Data.Git 
    ( Commit( .. )
    , Person( .. )
    , Tree( .. )
    , Blob( .. )
    , RefName( .. )
    , Git
    , Ref
    , resolveRevision
    , headGet
    )
import Data.Git.Ref
    ( fromHexString
    , isHexString
    )
import Data.Hourglass
    ( TimezoneOffset( TimezoneOffset, timezoneOffsetToMinutes ))
import Data.Git.Repository( branchList, tagList )
import qualified Data.Git.Revision as Rev
import Data.Git.Storage.Object( Object( .. ) )
import Data.Git.Types ( GitTime( .. ))

import Control.Monad.Trans.Class( lift )

import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro( sort )

import Difftimeline.CommitDiff
import Difftimeline.Diff
import Difftimeline.BaseLayer
import Difftimeline.Contract
import Difftimeline.DiffWorkingDirectory
import Difftimeline.GitIgnore

joinBytePath :: [BC.ByteString] -> T.Text
joinBytePath = foldl' (<//>) mempty
    where (<//>) t n = t `mappend` T.pack "/" `mappend` decodeUtf8 n

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
          inner a@(ModifyElement {}) | f a = [a]
          inner a@(TreeElement n r sub) | f a =
                [TreeElement n r $ concatMap inner sub]
          inner _ = []

data CommitDetail = CommitDetail
    { commitDetailMessage :: !T.Text
    , commitDetailParents :: ![CommitOverview]
    , commitDetailKey     :: !Ref
    , commitDetailTimestamp :: !Int
    , commitDetailTimezone  :: !Int
    , commitDetailAuthor  :: !T.Text
    , commitDetailChanges :: ![CommitTreeDiff]
    }
    deriving Show

data CommitPath = CommitPath
    { pathCommitRef     :: !Ref
    , pathParentRef     :: ![Ref]
    , pathMessage       :: !T.Text
    , pathTimestamp     :: !Int
    , pathTimezone      :: !Int
    , pathAuthor        :: !T.Text
    }

timezoneOfAuthor :: Person -> Int
timezoneOfAuthor = timezoneOffsetToMinutes . gitTimeTimezone . personTime

compareBranches :: Git -> Int -> String -> String
                -> IO (Either String CommitDetail)
compareBranches repo contextSize b1 b2 = runExceptT $
    detailer <$> diffBranches repo contextSize b1 b2
  where detailer diff = CommitDetail
            { commitDetailMessage = T.pack $ "Comparing " ++ b1 ++ " and " ++ b2
            , commitDetailParents = []
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

resolve :: Git -> RefName -> IO (Maybe Ref)
resolve repo = resolveRevision repo . Rev.fromString . refNameRaw

brancheslist :: Git -> IO [RemoteBranches]
brancheslist repo = do
  branchSet <- branchList repo
  tagSet <- tagList repo
  let fetchInfos refSet =
        forM (S.toList refSet) $ \r -> do
            resolved <- resolve repo r
            return $ BranchInfo (T.pack $ refNameRaw r) <$> resolved
  branches <- catMaybes <$> fetchInfos branchSet
  tags <- catMaybes <$> fetchInfos tagSet
  return [RemoteBranches "Branches" branches, RemoteBranches "Tags" tags]


diffBranches :: Git -> Int -> String -> String -> ExceptT String IO CommitTreeDiff
diffBranches repo contextSize branch1 branch2 = do
    ref1 <- revisionToRef repo branch1
    ref2 <- revisionToRef repo branch2
    snd <$> createCommitDiff  repo contextSize True ref1 ref2

diffCommitTree :: Git -> Ref -> IO (Either String CommitTreeDiff)
diffCommitTree repository ref = runExceptT $ do
  (ObjCommit thisCommit) <- accessCommit "Error can't file commit" repository ref
  let prevRef = firstParentRef thisCommit
  snd <$> createCommitDiff repository 0 False prevRef ref

revisionToRef :: Git -> String -> ExceptT String IO Ref
revisionToRef _repo r | isHexString r && length r == 40 = return $ fromHexString r
revisionToRef repo r = (do
    rr <- liftIO $ resolveRevision repo $ Rev.fromString r
    case rr of
      Nothing -> throwE $ "Can't resolve revision" ++ r
      Just f -> return f) `catchE` (\err ->
            throwE $ "Can't parse revision " ++ r ++ " " ++ show err)

workingDirectoryChanges  :: Git -> IgnoredSet -> Int -> String
                         -> IO (Either String CommitDetail)
workingDirectoryChanges repository ignoreSet contextSize ref = runExceptT $ do
    resolved <- revisionToRef repository ref
    workingDirectoryChanges' repository ignoreSet contextSize resolved

workingDirectoryChanges'  :: Git -> IgnoredSet -> Int -> Ref -> ExceptT String IO CommitDetail
workingDirectoryChanges' repository ignoreSet contextSize ref = do
   time <- lift $ truncate <$> getPOSIXTime 
   parentDetail <- fetchCommitOverview repository ref
   detailer parentDetail time
            <$> diffWorkingDirectory repository ignoreSet contextSize ref
    where detailer parentInfo time (_, diff) =
            CommitDetail {
                  commitDetailMessage = "Working directory"
                , commitDetailParents = [parentInfo]
                , commitDetailKey     = nullRef
                , commitDetailAuthor  = ""
                , commitDetailTimestamp = time
                , commitDetailTimezone  = 0
                , commitDetailChanges = filterCommitModifications
                                      $ flattenTreeDiff diff
                }

-- | Given a modification list, keep only the meaningful ones (the
-- one which contain real information about the diff).
filterCommitModifications :: [CommitTreeDiff] -> [CommitTreeDiff]
filterCommitModifications = filter isModification
    where isModification (AddElement _ _) = True
          isModification (DelElement _ _) = True
          isModification (NeutralElement _ _) = False
          isModification (ModifyElement {}) = True
          isModification (ModifyBinaryElement _ _)= True
          isModification (TreeElement {}) = False

-- | For a given commit, compute the diff with it's first parent.
diffCommit :: Git -> Int -> Bool -> Ref -> IO (Either String CommitDetail)
diffCommit repository contextSize deep ref = runExceptT $ do
   (ObjCommit thisCommit) <- accessCommit "Error can't find commit" repository ref
   let prevRef = firstParentRef thisCommit
   parentsInfo <- mapM (fetchCommitOverview repository) $ commitParents thisCommit
   detailer parentsInfo <$> createCommitDiff  repository contextSize deep prevRef ref 
    where detailer parents (thisCommit, diff) = 
            let author = commitAuthor thisCommit
            in CommitDetail {
                  commitDetailMessage = decodeUtf8 $ commitMessage thisCommit
                , commitDetailParents = parents
                , commitDetailKey     = ref
                , commitDetailAuthor  = decodeUtf8 $ personName author
                , commitDetailTimestamp = timeOfAuthor author
                , commitDetailTimezone  = timezoneOfAuthor author
                , commitDetailChanges = filterCommitModifications $ flattenTreeDiff diff
                }


-- | Given a file and a reference commit, compute the difference with
-- the previous file occurence for the file.
findFirstCommit :: Git              -- ^ Repository
                -> [B.ByteString]   -- ^ Path
                -> Ref              -- ^ Ref of the element in the path
                -> Ref              -- ^ First commit ref
                -> ExceptT String IO (Commit, Ref, [CommitPath])
findFirstCommit repository path currentFileRef ref = inner undefined undefined [ref]
    where inner prevCommit prevRef [] = return (prevCommit, prevRef, [])
          inner prevCommit prevRef (currentCommit:_) =
            catchE (do
                (info, commitFileRef) <- fetchFileRefInCommit repository currentCommit path
                if commitFileRef /= currentFileRef
                    then return (prevCommit, prevRef, [])
                    else do
                        (obj, r, commitPathRest) <- inner info currentCommit $ commitParents info
                        let author = commitAuthor info
                        return (obj, r, CommitPath {
                                pathCommitRef = currentCommit,
                                pathParentRef = commitParents info,
                                pathMessage = decodeUtf8 $ commitMessage info,
                                pathAuthor = decodeUtf8 $ personName author,
                                pathTimestamp = timeOfAuthor author,
                                pathTimezone = timezoneOfAuthor author
                            } : commitPathRest))
                            (\_ -> return (prevCommit, prevRef, []))

-- | Given a Tree object, try to find a path in it.
-- This function should not call error
findInTree :: Git -> [B.ByteString] -> Object -> IO (Maybe Ref)
findInTree git pathes = go  pathes . Just where
  go _                Nothing = return Nothing
  go []                     _ = return Nothing
  go [lp]   (Just (ObjTree (Tree lst))) = return $ findVal lp lst
  go (x:xs) (Just (ObjTree (Tree lst))) = case findVal x lst of
    Nothing -> return Nothing
    Just r  -> accessObject git r >>= go xs
  go _ _                      = return Nothing

  extractRef (_, _, ref) = ref
  findVal v lst = extractRef <$> find (\(_, n, _) -> v == toBytes n) lst

bytePathToString :: [B.ByteString] -> FilePath
bytePathToString = BC.unpack . BC.concat . map (BC.append (BC.pack "/"))

fetchFileRefInCommit :: Git -> Ref -> [B.ByteString] -> ExceptT String IO (Commit, Ref)
fetchFileRefInCommit repo ref bytePath = do
    let path = bytePathToString bytePath
    ObjCommit commit <- accessCommit ("Can't find commit (" ++ show ref ++ ")") repo ref
    t@(ObjTree _) <- accessTree ("Can't find tree for commit (" ++ show ref ++ ")") repo
                $ commitTreeish commit

    thisFileRef <- errorIO ("Can't find file (" ++ path ++ ") in commit(" ++ show ref ++ ")")
                 $ findInTree repo bytePath t

    return (commit, thisFileRef)

fetchBlobInCommit :: Git -> Ref -> [B.ByteString]
                  -> ExceptT String IO (Commit, L.ByteString)
fetchBlobInCommit repo ref bytePath = do
  let path = bytePathToString bytePath
  (commit, thisFileRef) <- fetchFileRefInCommit repo ref bytePath
  let msg = "Can't read file (" ++ path ++ ") content in commit (" ++ show ref ++ ")" 
  ObjBlob (Blob blobContent) <- accessBlob msg repo thisFileRef
  return (commit, blobContent)

data FileComparison = FileComparison
    { comparisonFile1    :: !T.Text
    , comparisonRef1     :: !Ref
    , comparisonFile2    :: !T.Text
    , comparisonRef2     :: !Ref
    , comparisonDiff     :: ![DiffCommand]
    }

data FileReference = LocalRef FilePath
                   | RepoRef String FilePath

pathOfRef :: FileReference -> FilePath
pathOfRef (LocalRef p) = p
pathOfRef (RepoRef _ p) = p

fetchFileReference :: Git -> FileReference
                   -> ExceptT String IO (T.Text, Ref)
fetchFileReference _ (LocalRef path) =
    liftIO $ (,nullRef) <$> TIO.readFile path

fetchFileReference repo (RepoRef commitKey path) = do
    ref <- revisionToRef repo commitKey
    let bytePath = map BC.pack $ splitDirectories path
        byteToText (_, b) = decodeUtf8 . B.concat $ L.toChunks b
    (,ref) . byteToText <$> fetchBlobInCommit repo ref bytePath

compareFiles :: Git -> FileReference -> FileReference
             -> IO (Either String FileComparison)
compareFiles repo ref1 ref2 = runExceptT $ do
    (blobContent1, f1) <- fetchFileReference repo ref1
    (blobContent2, f2) <- fetchFileReference repo ref2

    return FileComparison { 
          comparisonFile1 = blobContent1
        , comparisonFile2 = blobContent2
        , comparisonRef1  = f1
        , comparisonRef2  = f2
        , comparisonDiff = computeTextDiff blobContent1 blobContent2
        }

getHead :: Git -> IO (Maybe Ref)
getHead repo = do
  mr <- headGet repo
  case mr of
    Left r -> return $ Just r
    Right rn -> resolve repo rn

findParentFile :: Git -> FileReference -> IO (Either String ParentFile)
findParentFile repository originalRef = runExceptT inner where
  path = pathOfRef originalRef
  bytePath = map BC.pack $ splitDirectories path

  fetchCurrentRef (RepoRef commitStrSha _) = do
      let currentCommit = fromHexString commitStrSha
      (_, currentFileRef) <- fetchFileRefInCommit repository currentCommit bytePath
      ObjBlob nextFile <- accessBlob "can't find file content" repository currentFileRef
      (firstNfo, firstRef, betweenCommits) <-
              findFirstCommit repository bytePath currentFileRef currentCommit

      let previousRef = case commitParents firstNfo of
              [] -> currentCommit
              (r:_) -> r

      return (nextFile, firstNfo, firstRef, betweenCommits,
              previousRef, currentFileRef)

  fetchCurrentRef (LocalRef _) = do
      let fullPath = unpackPath repository ++ "/../" ++ path
      file <- 
          catchE (liftIO $ L.readFile path)
                     (\_ -> fail $ "Impossible to read file " ++ fullPath)
      {-time <- lift $ truncate <$> getPOSIXTime -}
      headRef  <- errorIO "Can't read HEAD" $ getHead repository
      let dummyAuthor = Person "" "" (GitTime 0 (TimezoneOffset 0))
          commit = Commit
            { commitTreeish = nullRef
            , commitEncoding = mempty
            , commitExtras = mempty
            , commitParents = [headRef]
            , commitAuthor = dummyAuthor
            , commitCommitter = dummyAuthor
            , commitMessage = "" }
      return (Blob file, commit, nullRef, [], headRef, nullRef)

  inner = do
      (Blob nextFile, firstNfo, firstRef, betweenCommits,
          previousRef, currentFileRef) <- fetchCurrentRef originalRef

      thisFile <-
          catchE (snd <$> fetchBlobInCommit repository previousRef bytePath)
                     (\_ -> return L.empty)

      let toStrict = B.concat . L.toChunks
          nextData = decodeUtf8 $ toStrict nextFile
          thisData = decodeUtf8 $ toStrict thisFile
          author = commitAuthor firstNfo
          isBinary = detectBinary nextFile

      parentsInfo <- mapM (fetchCommitOverview repository)
                   $ commitParents firstNfo

      return ParentFile
          { fileData = if isBinary then "" else T.filter (/= '\r') nextData
          , fileDataIsBinary = isBinary
          , fileName = T.pack path
          , fileRef = currentFileRef
          , parentRef = parentsInfo
          , fileMessage = decodeUtf8 $ commitMessage firstNfo
          , commitRef = firstRef
          , commitPath = reverse betweenCommits
          , fileDiff = computeTextDiff thisData nextData
          , parentCommitAuthor = decodeUtf8 $ personName author
          , parentCommitTimestamp = timeOfAuthor author
          , parentCommmitTimezone = timezoneOfAuthor author
          }

data ParentFile = ParentFile
    { fileData         :: T.Text
    , fileDataIsBinary :: Bool
    , fileName    :: T.Text
    , fileRef     :: Ref
    , parentRef   :: [CommitOverview]
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
    deriving (Eq, Show)

simplifyRange :: (Eq a) => [BlameRangeSource a] -> [BlameRangeSource a]
simplifyRange [] = []
simplifyRange [a] = [a]
simplifyRange ( BlameRangeSource { sourceLineIndex = b1, sourceSize = s1
                                 , sourceOriginalIndex = o1, sourceTag = t1 }
              : BlameRangeSource { sourceLineIndex = b2, sourceSize = s2
                                 , sourceTag = t2 }
              : xs )
    | b1 + s1 == b2 && t1 == t2 = simplifyRange $ BlameRangeSource b1 (s1 + s2) o1 t1 : xs
simplifyRange (x : xs) = x : simplifyRange xs

data BlameInfo = BlameInfo
    { blameData     :: T.Text
    , blameFilename :: T.Text
    , blameRanges   :: V.Vector (BlameRangeSource CommitOverview)
    , blameEarlyStamp  :: !Int
    , blameLatestStamp :: !Int
    }

-- | Perform a "blame", find the origin of every line of a commited file.
blameFile :: Git -> String -> FilePath -> IO (Either String BlameInfo)
blameFile repository commitStrSha path = runExceptT finalData where
  currentCommit = fromHexString commitStrSha

  bytePath = map BC.pack $ splitDirectories path
  toStrict = decodeUtf8 . B.concat . L.toChunks

  dateRange = V.foldl minMax (maxBound, minBound)
            . V.map (commitOverviewTimestamp . sourceTag)
      where minMax (mini, maxi) v = (min mini v, max maxi v)

  finalData = do
    (file, ranges) <- runWriterT initiator
    unfrozen <- liftIO $ V.unsafeThaw ranges
    liftIO $ sort unfrozen
    immutableSorted <- liftIO $ V.unsafeFreeze unfrozen
    let blameRange = V.fromList . simplifyRange $ V.toList immutableSorted
        (earliest, latest) = dateRange blameRange

    return BlameInfo
      { blameData = T.unlines $ V.toList file
      , blameRanges = blameRange
      , blameFilename = T.pack path
      , blameEarlyStamp = earliest
      , blameLatestStamp = latest
      }

  fetchCommit = accessCommit "Can't find current commit" repository
  fetchTree = accessTree "Can't find tree commit" repository
  fetchBlob = accessBlob "Can't find file content" repository
  fetchFileRef =
      errorIO "Can't find current content" . findInTree repository bytePath

  initiator :: WriterT (V.Vector (BlameRangeSource CommitOverview)) (ExceptT String IO) (V.Vector T.Text)
  initiator = do
    (_, currentFileRef) <- lift $
        fetchFileRefInCommit repository currentCommit bytePath

    (firstNfo, firstRef, _betweenCommits) <- lift $
        findFirstCommit repository bytePath currentFileRef currentCommit
    ObjBlob (Blob initialFile) <- lift $ fetchBlob currentFileRef

    when (detectBinary initialFile) .
       lift $ throwE "Can't perform blame on a binary file"

    let backLines = V.fromList . T.lines $ toStrict initialFile
        initialRange = createBlameRangeForSize $ V.length backLines
        overview = makeOverview firstRef firstNfo
    aux initialRange backLines overview $ commitParents firstNfo
    return backLines

  {-aux :: [BlameRange] -> V.Vector T.Text -> tag -> [Ref]-}
      {--> WriterT (V.Vector (BlameRangeSource tag)) (ExceptT String IO) ()-}
  aux []            _       _ _ = return ()
  aux ranges backData backRef [] =
      void $ blameInitialStep backRef (V.length backData) ranges

  aux ranges backData backRef (firstParent:_) = do
      ObjCommit commit <- lift $ fetchCommit firstParent
      t@(ObjTree _)    <- lift . fetchTree  $ commitTreeish commit
      (parents, overview, thisCommitData) <- lift $ catchE (do
                      currentFileRef <- fetchFileRef t
                      (firstNfo, firstRef, _betweenCommits) <-
                              findFirstCommit repository bytePath currentFileRef firstParent
                      ObjBlob (Blob nextFile) <- fetchBlob currentFileRef
                      let overview = makeOverview firstRef firstNfo
                      return (commitParents firstNfo, overview, nextFile))

                      (\_ -> return ([], undefined, L.empty))

      let nextData = V.fromList . T.lines $ toStrict thisCommitData
      leftRanges <- blameStep backRef nextData backData ranges
      aux leftRanges nextData overview parents

fetchCommitOverview :: Git -> Ref -> ExceptT String IO CommitOverview
fetchCommitOverview git ref = do
  ObjCommit info <- accessCommit ("Invalid parent " ++ show ref) git ref
  return $ makeOverview ref info

makeOverview :: Ref -> Commit -> CommitOverview
makeOverview r c = CommitOverview
   { commitOverviewParent = commitParents c
   , commitOverviewMessage = decodeUtf8 $ commitMessage c
   , commitOverviewRef = r
   , commitOverviewAuthor = decodeUtf8 . personName $ commitAuthor c
   , commitOverviewTimestamp = timeOfAuthor $ commitAuthor c
   }

commitList :: Git -> Int -> Ref -> IO (Either String [CommitOverview])
commitList repository count = runExceptT . go count where 
  go 0 _ = return []
  go n _ | n < 0 = return []
  go n r = do
    ObjCommit commit <- accessCommit "" repository r
    (makeOverview r commit :) <$> go (n - 1) (firstParentRef commit)

basePage :: Git -> [B.ByteString] -> IO (Either String ParentFile)
basePage repository path = runExceptT $ do
    let getObj errorReason = errorIO errorReason . accessObject repository
    headRef        <- errorIO "Can't read HEAD" $ getHead repository
    (cInfo, foundFileRef) <- fetchFileRefInCommit repository headRef path
    (ObjBlob (Blob content)) <-
        getObj ("Error can't find file (" ++ bytePathToString path ++ ") content in HEAD") foundFileRef
    (_, prevContent) <- fetchBlobInCommit repository (firstParentRef cInfo) path

    let toStrict = B.concat . L.toChunks
        author = commitAuthor cInfo
        prevData = decodeUtf8 $ toStrict prevContent
        thisData = decodeUtf8 $ toStrict content
        isBinary = detectBinary content

    parentsInfo <- mapM (fetchCommitOverview repository)
                 $ commitParents cInfo
    let cpath = CommitPath
          { pathCommitRef = headRef
          , pathParentRef = commitParents cInfo
          , pathMessage = decodeUtf8 $ commitMessage cInfo
          , pathTimestamp = timeOfAuthor author
          , pathTimezone = timezoneOfAuthor author
          , pathAuthor = decodeUtf8 $ personName author
          }

    return ParentFile {
        commitRef = headRef,
        fileRef = foundFileRef,
        fileName = joinBytePath path,
        commitPath = [cpath],
        fileDiff = computeTextDiff prevData thisData,
        fileData = if isBinary then "" else thisData,
        fileDataIsBinary = isBinary,
        parentRef = parentsInfo,
        fileMessage = decodeUtf8 $ commitMessage cInfo,
        parentCommitAuthor = decodeUtf8 $ personName author,
        parentCommitTimestamp = timeOfAuthor author,
        parentCommmitTimezone = timezoneOfAuthor author
    }

