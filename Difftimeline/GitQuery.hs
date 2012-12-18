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


                            -- * Manipulation functions
                            , flattenTreeDiff
                            , filterCommitTreeDiff
                            ) where

import Prelude

import Data.List( sortBy, foldl', find  )
import Data.Monoid( mempty, mappend )
import System.FilePath( splitDirectories  )
import Control.Applicative
import qualified Control.Exception as E
import Control.Monad( forM, when, void )
import Control.Monad.Error( ErrorT, throwError, runErrorT, catchError )
import Control.Monad.IO.Class( liftIO )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding( decodeUtf8With )
import Data.Text.Encoding.Error( lenientDecode )

import Data.Time.Clock.POSIX( getPOSIXTime )

import System.Directory( getDirectoryContents, doesFileExist  )
import Data.Git( GitObject( .. )
               , CommitAuthor( .. )
               , CommitInfo( .. )
               , Git
               , findObject
               , gitRepoPath 
               , CommitInfo( .. )
               , getHead
               , Ref
               , RefSpec( .. )
               , fromHexString
               , toHexString
               , getBranchNames
               , getRemoteNames
               , getRemoteBranchNames
               , getTagNames
               , readRemoteBranch
               , readAllRemoteBranches
               , readBranch
               , readTag

               , TreeEntry
               , revFromString
               , resolveRevision
               )

import Control.Monad.Trans.Class( lift )
import Control.Monad.Trans.Writer.Strict( WriterT
                                        , runWriterT
                                        )

import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro( sort )

import Difftimeline.Diff
import Difftimeline.GitIgnore

import Debug.Trace

decodeUtf8 :: B.ByteString -> T.Text
decodeUtf8 = decodeUtf8With lenientDecode

data CommitTreeDiff = AddElement !T.Text !Ref
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
    { commitDetailMessage :: T.Text
    , commitDetailParents :: [CommitOverview]
    , commitDetailKey     :: Ref
    , commitDetailTimestamp :: Int
    , commitDetailTimezone  :: Int
    , commitDetailAuthor  :: T.Text
    , commitDetailChanges :: [CommitTreeDiff]
    }

data CommitPath = CommitPath
    { pathCommitRef     :: Ref
    , pathParentRef     :: [Ref]
    , pathMessage       :: T.Text
    , pathTimestamp     :: Int
    , pathTimezone      :: Int
    , pathAuthor        :: T.Text
    }

-- | Try to detect binary element given a blob
detectBinary :: L.ByteString -> Bool
detectBinary = L.any (== 0) . L.take binaryDetectionSize
    where binaryDetectionSize = 8 * 1024

firstParentRef :: CommitInfo -> Ref
firstParentRef info = case commitParents info of
    [] -> nullRef
    (r:_) -> r

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
            sequence [objAccess subRef >>= aux (BC.unpack n) subRef
                                        | (_, n, subRef) <- t]
          aux _ _ _ = throwError "Wrong git object kind"

compareBranches :: Git -> Int -> String -> String
                -> IO (Either String CommitDetail)
compareBranches repo contextSize b1 b2 = runErrorT $
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

brancheslist :: Git -> IO [RemoteBranches]
brancheslist repo = do
    let fetchBranch b =
          E.catch ((:[]) . BranchInfo (T.pack b) <$> readBranch repo b)
                  (\(_ :: IOError) -> pure [])

        fetchRemoteBranch _remote "HEAD" = pure[]
        fetchRemoteBranch remote b =
            sequence [BranchInfo (T.pack $ remote </> b) <$> readRemoteBranch repo remote b]

        fetchTag b = trace ("[9 ] fetching tag " ++ b) $ BranchInfo (T.pack b) <$> readTag repo b
    branchNames <- (trace "[1 ] Fetching local branch names") $ getBranchNames repo
    oldBranchInfo <- (trace "[2 ] Fetching local branch values") $ concat <$> mapM fetchBranch branchNames

    allBranches <- (\a -> trace ("[3 ] Gather all branches (packed-ref)" {-++ show a -}) a) <$> readAllRemoteBranches repo
    remoteList <- (trace "[4 ] Gather all remotes (old way)") $ getRemoteNames repo
    remotesOldStyle <- forM remoteList (\remote -> do
        branchesName <- trace ("[5 ] branches for remote" ++ remote) $ E.catch (getRemoteBranchNames repo remote)
                                (\(_ :: IOError) -> pure [])
        branches <- concat <$> E.catch (mapM (\b -> trace ("[6 ] fetching branch " ++ b) $ fetchRemoteBranch remote b) branchesName)
                                       (\(_ :: IOError) -> trace ("/!\\ ERROR") $ pure [])
        trace ("[7 ] Got branches for remote " ++ remote) $ pure RemoteBranches { remoteName = T.pack remote
                            , remoteBranches = branches })
    oldTagInfo <- trace "[8 ] Gathering tag (if possible)" $ getTagNames repo >>= mapM fetchTag 

    let remoteNotEmpty (RemoteBranches _ lst) = not $ null lst

        remotes =
                  [RemoteBranches (T.pack n)
                      [BranchInfo (T.pack s) r | (r, s) <- lst]
                        | RefRemote n lst <- allBranches ]
                  ++
                    [RemoteBranches (T.pack "/others/")
                        [BranchInfo (T.pack s) r | RefOther r s <- allBranches ]
                    ]
        tagInfo = [BranchInfo (T.pack s) r | RefTag r s <- allBranches]
        branchInfo = [BranchInfo (T.pack s) r | RefLocal r s <- allBranches]

    let localBranch = RemoteBranches {
        remoteName = "local",
        remoteBranches = branchInfo ++ oldBranchInfo ++ tagInfo ++ oldTagInfo
    }
    pure . filter remoteNotEmpty . trace ("[10] OK.") $ localBranch : remotesOldStyle ++ remotes

diffBranches :: Git -> Int -> String -> String -> ErrorT String IO CommitTreeDiff
diffBranches repo contextSize branch1 branch2 = do
    ref1 <- revisionToRef repo branch1
    ref2 <- revisionToRef repo branch2
    snd <$> createCommitDiff  repo contextSize True ref1 ref2

diffCommitTree :: Git -> Ref -> IO (Either String CommitTreeDiff)
diffCommitTree repository ref = runErrorT $ do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    let prevRef = firstParentRef thisCommit
    snd <$> createCommitDiff repository 0 False prevRef ref

data SubKind = KindFile | KindDirectory

fetchDirectoryInfo :: FilePath -> IO [(SubKind, BC.ByteString)]
fetchDirectoryInfo name = do
    files <- getDirectoryContents name
    sequence [(, BC.pack sub) <$> (kindOfExist <$> doesFileExist sub) | sub <- files ]
        where kindOfExist True = KindFile
              kindOfExist False = KindDirectory

revisionToRef :: Git -> String -> ErrorT String IO Ref
revisionToRef repo r =
    case revFromString r of
      Left err -> throwError $ "Can't parse revision " ++ r ++ " " ++ err
      Right rev -> do
          rr <- liftIO $ resolveRevision repo rev
          case rr of
               Nothing -> throwError $ "Can't resolve revision" ++ r
               Just f -> return f


workingDirectoryChanges  :: Git -> IgnoredSet -> Int -> String
                         -> IO (Either String CommitDetail)
workingDirectoryChanges repository ignoreSet contextSize ref = runErrorT $ do
    resolved <- revisionToRef repository ref
    workingDirectoryChanges' repository ignoreSet contextSize resolved

workingDirectoryChanges'  :: Git -> IgnoredSet -> Int -> Ref -> ErrorT String IO CommitDetail
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
                     -> ErrorT String IO (CommitInfo, CommitTreeDiff)
diffWorkingDirectory repository ignoreSet contextSize ref = do
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
            pure $! fileComparer name c1 r1 file2
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
                                                  , let fullName = name </> BC.unpack fname 
                                                        fullNameCheck = name <///> BC.unpack fname
                                                  , not $ isPathIgnored ignoreSet fullNameCheck]

        diffTree _flatname name lefts     [] = sequence
            [maySubTree DelElement fullName r | (_, item, r) <- lefts
                                              , let fullName = name </> BC.unpack item ]
        diffTree flatname name lefts@((_, lName, lRef):ls) rights@((_, rName):rs)
            | rName `elem` [".", "..", ".git"]
                || isPathIgnored ignoreSet (name <///> BC.unpack rName) = diffTree flatname name lefts rs
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

-- | Compare two commits
createCommitDiff :: Git
                 -> Int  -- ^ Context size to embed
                 -> Bool -- ^ If we compute file diff
                 -> Ref  -- ^ Base commit
                 -> Ref  -- ^ Final commit (destination)
                 -> ErrorT String IO (CommitInfo, CommitTreeDiff)
createCommitDiff repository contextSize deep prevRef ref = do
    (Commit thisCommit) <- accessCommit "Error can't file commit" repository ref
    thisTree <- getObj "Error can't access commit tree" $ commitTree thisCommit

    (prevTreeRef, prevTree) <- if prevRef /= nullRef
            then do Commit prevCommit <- accessCommit "Error can't file parent commit" repository prevRef
                    let prevTreeRef = commitTree prevCommit
                    (prevTreeRef,) <$> getObj "Error can't access previous commit tree" prevTreeRef
            else return (nullRef, Tree [])

    (,) thisCommit <$> inner "" prevTree prevTreeRef thisTree (commitTree thisCommit)
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
                ModifyElement (T.pack name) r2 $! computeTextScript contextSize txtLeft txtRight
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
diffCommit repository contextSize deep ref = runErrorT $ do
   (Commit thisCommit) <- accessCommit "Error can't find commit" repository ref
   let prevRef = firstParentRef thisCommit
   parentsInfo <- mapM (fetchCommitOverview repository) $ commitParents thisCommit
   detailer parentsInfo <$> createCommitDiff  repository contextSize deep prevRef ref 
    where detailer parents (thisCommit, diff) = 
            let author = commitAuthor thisCommit
            in CommitDetail {
                  commitDetailMessage = decodeUtf8 $ commitMessage thisCommit
                , commitDetailParents = parents
                , commitDetailKey     = ref
                , commitDetailAuthor  = decodeUtf8 $ authorName author
                , commitDetailTimestamp = authorTimestamp author
                , commitDetailTimezone  = authorTimezone author
                , commitDetailChanges = filterCommitModifications $ flattenTreeDiff diff
                }


-- | Given a file and a reference commit, compute the difference with
-- the previous file occurence for the file.
findFirstCommit :: Git              -- ^ Repository
                -> [B.ByteString]   -- ^ Path
                -> Ref              -- ^ Ref of the element in the path
                -> Ref              -- ^ First commit ref
                -> ErrorT String IO (CommitInfo, Ref, [CommitPath])
findFirstCommit repository path currentFileRef ref = inner undefined undefined [ref]
    where inner prevCommit prevRef [] = return (prevCommit, prevRef, [])
          inner prevCommit prevRef (currentCommit:_) =
            catchError (do
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
                                pathAuthor = decodeUtf8 $ authorName author,
                                pathTimestamp = authorTimestamp author,
                                pathTimezone = authorTimezone author
                            } : commitPathRest))
                            (\_ -> return (prevCommit, prevRef, []))

accessObject :: Git -> Ref -> IO (Maybe GitObject)
accessObject = findObject

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

bytePathToString :: [B.ByteString] -> FilePath
bytePathToString = BC.unpack . BC.concat . map (BC.append (BC.pack "/"))

fetchFileRefInCommit :: Git -> Ref -> [B.ByteString] -> ErrorT String IO (CommitInfo, Ref)
fetchFileRefInCommit repo ref bytePath = do
    let path = bytePathToString bytePath
    Commit commit <- accessCommit ("Can't find commit (" ++ show ref ++ ")") repo ref
    t@(Tree _) <- accessTree ("Can't find tree for commit (" ++ show ref ++ ")") repo
                $ commitTree commit

    thisFileRef <- errorIO ("Can't find file (" ++ path ++ ") in commit(" ++ show ref ++ ")")
                 $ findInTree repo bytePath t

    return (commit, thisFileRef)

fetchBlobInCommit :: Git -> Ref -> [B.ByteString] -> ErrorT String IO (CommitInfo, L.ByteString)
fetchBlobInCommit repo ref bytePath = do
    let path = bytePathToString bytePath
    (commit, thisFileRef) <- fetchFileRefInCommit repo ref bytePath
    Blob blobContent <- accessBlob ("Can't read file (" ++ path ++ ") content in commit (" ++ show ref ++ ")" )
                                repo thisFileRef
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

fetchFileReference :: Git -> FileReference
                   -> ErrorT String IO (T.Text, Ref)
fetchFileReference _ (LocalRef path) =
    liftIO $ (,nullRef) <$> TIO.readFile path

fetchFileReference repo (RepoRef commitKey path) = do
    ref <- revisionToRef repo commitKey
    let bytePath = map BC.pack $ splitDirectories path
        byteToText (_, b) = decodeUtf8 . B.concat $ L.toChunks b
    (,ref) . byteToText <$> fetchBlobInCommit repo ref bytePath

compareFiles :: Git -> FileReference -> FileReference
             -> IO (Either String FileComparison)
compareFiles repo ref1 ref2 = runErrorT $ do
    (blobContent1, f1) <- fetchFileReference repo ref1
    (blobContent2, f2) <- fetchFileReference repo ref2

    return FileComparison { 
          comparisonFile1 = blobContent1
        , comparisonFile2 = blobContent2
        , comparisonRef1  = f1
        , comparisonRef2  = f2
        , comparisonDiff = computeTextDiff blobContent1 blobContent2
        }

findParentFile :: Git -> String -> FilePath -> IO (Either String ParentFile)
findParentFile repository commitStrSha path = runErrorT inner
  where currentCommit = fromHexString commitStrSha
        bytePath = map BC.pack $ splitDirectories path

        inner = do
            (_, currentFileRef) <- fetchFileRefInCommit repository currentCommit bytePath
            (firstNfo, firstRef, betweenCommits) <-
                    findFirstCommit repository bytePath currentFileRef currentCommit
            let previousRef = case commitParents firstNfo of
                    [] -> currentCommit
                    (r:_) -> r

            Blob nextFile <- accessBlob "can't find file content" repository currentFileRef
            thisFile <-
                catchError (snd <$> fetchBlobInCommit repository previousRef bytePath)
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
                , parentCommitAuthor = decodeUtf8 $ authorName author
                , parentCommitTimestamp = authorTimestamp author
                , parentCommmitTimezone = authorTimezone author
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
blameFile repository commitStrSha path = runErrorT finalData
  where currentCommit = fromHexString commitStrSha
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

            return BlameInfo { blameData = T.unlines $ V.toList file
                             , blameRanges = blameRange
                             , blameFilename = T.pack path
                             , blameEarlyStamp = earliest
                             , blameLatestStamp = latest
                             }

        fetchCommit = lift . accessCommit "Can't find current commit" repository
        fetchTree = lift . accessTree "Can't find tree commit" repository
        fetchBlob = lift . accessBlob "Can't find file content" repository
        fetchFileRef =
            lift . errorIO "Can't find current content" . findInTree repository bytePath

        initiator = do
            (_, currentFileRef) <- lift $
                fetchFileRefInCommit repository currentCommit bytePath

            (firstNfo, firstRef, _betweenCommits) <- lift $
                    findFirstCommit repository bytePath currentFileRef currentCommit
            Blob initialFile <- fetchBlob currentFileRef

            when (detectBinary initialFile)
                 (throwError "Can't perform blame on a binary file")

            let backLines = V.fromList . T.lines $ toStrict initialFile
                initialRange = createBlameRangeForSize $ V.length backLines
                overview = makeOverview firstRef firstNfo
            aux initialRange backLines overview $ commitParents firstNfo
            return backLines

        aux []            _       _ _ = return ()
        aux ranges backData backRef [] =
            void $ blameInitialStep backRef (V.length backData) ranges

        aux ranges backData backRef (firstParent:_) = do
            Commit commit <- fetchCommit firstParent
            t@(Tree _)    <- fetchTree  $ commitTree commit
            (parents, overview, thisCommitData) <- catchError (do
                            currentFileRef <- fetchFileRef t
                            (firstNfo, firstRef, _betweenCommits) <- lift $
                                    findFirstCommit repository bytePath currentFileRef firstParent
                            Blob nextFile <- fetchBlob currentFileRef
                            let overview = makeOverview firstRef firstNfo
                            return (commitParents firstNfo, overview, nextFile))

                            (\_ -> return ([], undefined, L.empty))

            let nextData = V.fromList . T.lines $ toStrict thisCommitData
            leftRanges <- blameStep backRef nextData backData ranges
            aux leftRanges nextData overview parents

fetchCommitOverview :: Git -> Ref -> ErrorT String IO CommitOverview
fetchCommitOverview git ref = do
    Commit info <- accessCommit ("Invalid parent " ++ show ref) git ref
    return $ makeOverview ref info

makeOverview :: Ref -> CommitInfo -> CommitOverview
makeOverview r c = CommitOverview
   { commitOverviewParent = commitParents c
   , commitOverviewMessage = decodeUtf8 $ commitMessage c
   , commitOverviewRef = r
   , commitOverviewAuthor = decodeUtf8 . authorName $ commitAuthor c
   , commitOverviewTimestamp = authorTimestamp $ commitAuthor c
   }

commitList :: Git -> Int -> Ref -> IO (Either String [CommitOverview])
commitList repository count = runErrorT . inner count
    where inner 0 _ = return []
          inner n _ | n < 0 = return []
          inner n r = do
              Commit commit <- accessCommit "" repository r
              (makeOverview r commit :) <$> inner (n - 1) (firstParentRef commit)

basePage :: Git -> [B.ByteString] -> IO (Either String ParentFile)
basePage repository path = runErrorT $ do
    let getObj errorReason = errorIO errorReason . accessObject repository
    headRef        <- errorIO "Can't read HEAD" $ getHead repository
    (cInfo, foundFileRef) <- fetchFileRefInCommit repository headRef path
    (Blob content) <-
        getObj ("Error can't find file (" ++ bytePathToString path ++ ") content in HEAD") foundFileRef
    (_, prevContent) <- fetchBlobInCommit repository (firstParentRef cInfo) path

    let toStrict = B.concat . L.toChunks
        author = commitAuthor cInfo
        prevData = decodeUtf8 $ toStrict prevContent
        thisData = decodeUtf8 $ toStrict content
        isBinary = detectBinary content

    parentsInfo <- mapM (fetchCommitOverview repository)
                 $ commitParents cInfo

    return ParentFile {
        commitRef = headRef,
        fileRef = foundFileRef,
        fileName = joinBytePath path,
        commitPath = [CommitPath { pathCommitRef = headRef
                                 , pathParentRef = commitParents cInfo
                                 , pathMessage = decodeUtf8 $ commitMessage cInfo
                                 , pathTimestamp = authorTimestamp author
                                 , pathTimezone = authorTimezone author
                                 , pathAuthor = decodeUtf8 $ authorName author
                                 }],
        fileDiff = computeTextDiff prevData thisData,
        fileData = if isBinary then "" else thisData,
        fileDataIsBinary = isBinary,
        parentRef = parentsInfo,
        fileMessage = decodeUtf8 $ commitMessage cInfo,
        parentCommitAuthor = decodeUtf8 $ authorName author,
        parentCommitTimestamp = authorTimestamp author,
        parentCommmitTimezone = authorTimezone author
    }

