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

import Data.List( sortBy, foldl', find  )
import Data.Maybe( catMaybes )
import Data.Byteable( toBytes )
import Data.Monoid( (<>) )
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
import Data.Text.Encoding( decodeUtf8With )
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding.Error( lenientDecode )
import Control.Monad.Trans.Writer.Strict( WriterT, runWriterT )

import Data.Time.Clock
    ( UTCTime
    , addUTCTime
    , picosecondsToDiffTime 
    )

import Data.Time.Clock.POSIX
    ( getPOSIXTime
    , posixSecondsToUTCTime
    )

import qualified Filesystem.Path.Rules as FP
import System.Directory( getDirectoryContents, doesFileExist, getModificationTime )
import Data.Git 
    ( Commit( .. )
    , Person( .. )
    , Tree( .. )
    , Blob( .. )
    , RefName( .. )
    , Git
    , Ref
    , getObject
    , resolveRevision
    , headGet
    )
import Data.Git.Ref
    ( fromHexString
    , isHexString
    , toHexString
    )
import Data.Hourglass
    ( TimezoneOffset( TimezoneOffset, timezoneOffsetToMinutes )
    , timeFromElapsed )
import Foreign.C.Types( CTime( CTime ) )
import Data.Git.Repository( branchList, tagList )
import qualified Data.Git.Revision as Rev
import Data.Git.Storage
    ( gitRepoPath
    , findFileInIndex
    , IndexEntry( _mtime, _mtimeNano )
    )
import Data.Git.Storage.Object( Object( .. ) )
import Data.Git.Types
    ( TreeEnt
    , GitTime( .. )
    )

import Control.Monad.Trans.Class( lift )

import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro( sort )

import Difftimeline.Diff
import Difftimeline.GitIgnore

import Debug.Trace
import Text.Printf

decodeUtf8 :: B.ByteString -> T.Text
decodeUtf8 = decodeUtf8With lenientDecode

decodeUtf8Lazy :: L.ByteString -> T.Text
decodeUtf8Lazy = TL.toStrict . LE.decodeUtf8With lenientDecode

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

timeOfAuthor :: Person -> Int
timeOfAuthor = unCTime . timeFromElapsed . gitTimeUTC . personTime
  where unCTime (CTime t) = fromIntegral t

unpackPath :: Git -> FilePath
unpackPath = BC.unpack . FP.encode FP.posix . gitRepoPath 

-- | Try to detect binary element given a blob
detectBinary :: L.ByteString -> Bool
detectBinary = L.any (== 0) . L.take binaryDetectionSize
    where binaryDetectionSize = 8 * 1024

firstParentRef :: Commit -> Ref
firstParentRef info = case commitParents info of
    [] -> nullRef
    (r:_) -> r

-- | Want same behaviour between windows & Unix
(</>) :: FilePath -> FilePath -> FilePath
(</>) a b = a ++ "/" ++ b

errorIO :: String -> IO (Maybe a) -> ExceptT String IO a
errorIO str a = do
    mayrez <- liftIO a
    case mayrez of
        Nothing -> throwE str
        Just j  -> return j

accessCommit :: String -> Git -> Ref -> ExceptT String IO Object
accessCommit s rep ref = do
  rez <- liftIO $ getObject rep ref True
  case rez of
      Nothing -> throwE s
      Just c@(ObjCommit _) -> return c
      Just _ -> throwE s

accessTree :: String -> Git -> Ref -> ExceptT String IO Object
accessTree s rep ref = do
    rez <- liftIO $ getObject rep ref True
    case rez of
        Nothing -> throwE s
        Just c@(ObjTree _) -> return c
        Just _ -> throwE s

accessBlob :: String -> Git -> Ref -> ExceptT String IO Object
accessBlob s rep ref = do
    rez <- liftIO $ getObject rep ref True
    case rez of
        Nothing -> throwE s
        Just c@(ObjBlob _) -> return c
        Just _ -> throwE s

nullRef :: Ref
nullRef = fromHexString $ replicate 40 '0'

batchSubTree :: Git -> (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref -> Object
             -> ExceptT String IO CommitTreeDiff
batchSubTree  repository f = aux where
  objAccess = errorIO "" . accessObject repository

  aux name r (ObjBlob _) = return $ f (T.pack name) r
  aux name r (ObjTree (Tree t)) = TreeElement (T.pack name) r <$>
    sequence [objAccess subRef >>= aux (BC.unpack $ toBytes n) subRef
                                | (_, n, subRef) <- t]
  aux _ _ _ = throwE "Wrong git object kind"

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

data SubKind = KindFile | KindDirectory

fetchDirectoryInfo :: FilePath -> IO [(SubKind, BC.ByteString)]
fetchDirectoryInfo name = do
    files <- getDirectoryContents name
    sequence [(, BC.pack sub) <$> (kindOfExist <$> doesFileExist sub) | sub <- files ]
        where kindOfExist True = KindFile
              kindOfExist False = KindDirectory

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

(<///>) :: FilePath -> FilePath -> FilePath
"" <///> a = a
a  <///> b = a </> b

diffWorkingDirectory :: Git -> IgnoredSet -> Int -> Ref
                     -> ExceptT String IO (Commit, CommitTreeDiff)
diffWorkingDirectory repository ignoreSet contextSize ref = do
    (ObjCommit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let authorStamp = timeOfAuthor $ commitAuthor thisCommit
        commitStamp = timeOfAuthor $ commitCommitter thisCommit
        maxStamp = min commitStamp authorStamp
        utcTime = posixSecondsToUTCTime $ realToFrac maxStamp
        upperPath = unpackPath repository ++ "/.."
    thisTree <- getObj "Error can't access commit tree" $ commitTreeish thisCommit
    (,) thisCommit <$> inner utcTime upperPath "" thisTree (commitTreeish thisCommit) 
  where getObj reason = errorIO reason . accessObject repository

        fileComparer name bin1 r1 bin2
            | bin1 == bin2 = NeutralElement (T.pack name) r1
            | detectBinary bin2 = ModifyBinaryElement (T.pack name) r1
            | otherwise = case computeTextScript contextSize bin1 bin2 of
                [] -> NeutralElement (T.pack name) r1
                lst -> ModifyElement (T.pack name) r1 $ unpackDiff lst
                   where unpackDiff = fmap (\(a, t) -> (a, fmap decodeUtf8Lazy t))

        wasEdited path = do
           indexEntry <- findFileInIndex repository $ BC.pack path
           case indexEntry of
             Nothing -> return $ trace ("NO_INDEX for: " ++ show path) True
             Just ix -> do
                modTime <- getModificationTime path
                let time = addUTCTime (fromRational . toRational $ picosecondsToDiffTime (fromIntegral (_mtimeNano ix) * 1000))
                                      (posixSecondsToUTCTime (realToFrac $ _mtime ix))
                return $ (\v-> trace (printf "MOD: %s (index:%s) (file:%s) => %s" path (show time) (show modTime) (show v)) v) $ time < modTime

        inner maxTime flatname name (ObjTree (Tree left)) r = do
          right <- liftIO $ fetchDirectoryInfo flatname
          TreeElement (T.pack name) r <$> diffTree maxTime flatname name sortedLeft (sortFolder right)
            where sortedLeft = sortBy (\(_,a,_) (_,b,_) -> compare a b) left
                  sortFolder = sortBy (\(_,a) (_,b) -> compare a b)

        inner _ flatname name (ObjBlob (Blob c1)) r1 = do
            file2 <- liftIO $ L.readFile flatname
            pure $! fileComparer name c1 r1 file2
        inner _ _ _ _ _ = throwE "Wrong git object kind"

        maySubTree :: (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref
                   -> ExceptT String IO CommitTreeDiff
        maySubTree f name r = do
            sub <- liftIO $ accessObject repository r
            -- | Sometimes, there is nothing to see
            case sub of
               Nothing -> return $ f (T.pack name) r
               Just el  -> batchSubTree repository f name r el

        diffTree :: UTCTime -> String -> String -> [TreeEnt] -> [(SubKind, BC.ByteString)]
                 -> ExceptT String IO [CommitTreeDiff]
        diffTree _ _ _name   []     [] = return []
        diffTree _ _ name    [] rights = pure
            [AddElement (T.pack fullName) nullRef | (KindFile, fname) <- rights
                                                  , let fullName = name </> BC.unpack fname 
                                                        fullNameCheck = name <///> BC.unpack fname
                                                  , not $ isPathIgnored ignoreSet fullNameCheck]

        diffTree _ _flatname name lefts     [] = sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = name </> BC.unpack (toBytes item) ]
        diffTree maxTime flatname name lefts@((_, lName, lRef):ls) rights@((_, rName):rs)
            | rName `elem` [".", "..", ".git"]
                || isPathIgnored ignoreSet (name <///> BC.unpack rName) = diffTree maxTime flatname name lefts rs
            | toBytes lName == rName = do
                -- We try to prune scanning if possible
                wasModified <- liftIO $ wasEdited (name <///> BC.unpack (toBytes lName))
                let thisElem = NeutralElement (decodeUtf8 $ toBytes lName) lRef
                if not wasModified then (thisElem :) <$> diffTree maxTime flatname name ls rs else do
                maySubL <- liftIO $ accessObject repository lRef
                case maySubL of
                  -- This case should happen in presence of submodules.
                  Nothing -> (thisElem :) <$> diffTree maxTime flatname name ls rs
                  Just subL ->
                        (((:) <$> inner maxTime (flatname </> BC.unpack (toBytes lName)) (BC.unpack $ toBytes lName) subL lRef)
                                `catchE` (\_ -> return id)) <*> diffTree maxTime flatname name ls rs

            | toBytes lName < rName =
                (:) <$> maySubTree DelElement (BC.unpack $ toBytes lName) lRef
                    <*> diffTree maxTime flatname name ls rights

            | otherwise = (AddElement (decodeUtf8 rName) lRef:) <$> 
                              diffTree maxTime flatname name lefts rs

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

accessObject :: Git -> Ref -> IO (Maybe Object)
accessObject repo ref = getObject repo ref True

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
    deriving (Eq)

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

