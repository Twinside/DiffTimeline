{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Difftimeline.RequestHandler( DifftimelineAPI, serveDifftimeline ) where

import Data.Monoid( (<>) )
import Data.Aeson( ToJSON( toJSON ), object, (.=) )
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad.IO.Class( MonadIO, liftIO )
import Control.Monad.Trans.Except( runExceptT )
import Control.Monad.Reader( MonadReader, ReaderT, ask, asks )
import Control.Monad.Except( MonadError, throwError )

import System.FilePath( splitDirectories  )
import Data.Git( Git )
import Data.Git.Ref( fromHexString )
import Data.Aeson( encode )

import Data.Text( Text )
import Difftimeline.Types
import Difftimeline.Externs()
import Difftimeline.GitQuery
import Difftimeline.StaticFiles
import Difftimeline.GitIgnore( IgnoredSet )
import Difftimeline.Diff( invertWay )

import Servant.API.ContentTypes( Accept( .. ), MimeRender( mimeRender ) )
import qualified Servant.Server as SS
import Servant.Server( ServerT, ServantErr( .. ) )
import Servant.API
    ( (:>)
    , (:<|>)( .. )
    , Get, Capture
    , PlainText, JSON, OctetStream )

import Network.HTTP.Media ((//), (/:))

data RawHTML
data RawCSS

instance Accept RawHTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance Accept RawCSS where
  contentType _ = "text" // "css" /: ("charset", "utf-8")

instance MimeRender RawCSS BC.ByteString where
  mimeRender _ b = LC.fromChunks [b]

instance MimeRender RawHTML BC.ByteString where
  mimeRender _ b = LC.fromChunks [b]

instance MimeRender PlainText LC.ByteString where
  mimeRender _ = id
instance MimeRender PlainText BC.ByteString where
  mimeRender _ b = LC.fromChunks [b]

type DifftimelineAPI 
    = "ask_parent"        :> Capture "ref"  Text 
                          :> Capture "path" Text
                          :> Get '[JSON] ParentFile
  :<|> "ask_commit"       :> Capture "ref"  Text
                          :> Get '[JSON] CommitDetail
  :<|> "ask_commit_tree"  :> Capture "ref"  Text
                          :> Get '[JSON] CommitTreeDiff
  :<|> "compare_branches" :> Capture "ref1" Text
                          :> Capture "ref2" Text
                          :> Get '[JSON] CommitDetail
  :<|> "commit"           :> Capture "ref"  Text
                          :> Get '[JSON] CommitDetail
  :<|> "blame"            :> Capture "ref"  Text
                          :> Capture "filename" Text
                          :> Get '[JSON] BlameInfo
  :<|> "commit_list"      :> Capture "count" Int
                          :> Capture "ref" Text 
                          :> Get '[JSON] [CommitOverview]
  :<|> "compare_files"    :> Capture "key1"  Text
                          :> Capture "file1" Text
                          :> Capture "key2"  Text
                          :> Capture "file2" Text
                          :> Get '[JSON] FileComparison
  :<|> "branches_list"    :> Get '[JSON] [RemoteBranches]
  :<|> "initial_info.js"  :> Get '[PlainText] LC.ByteString
  :<|> StaticAPI
  :<|> Get '[RawHTML] B.ByteString
  :<|> "favicon.ico"      :> Get '[OctetStream] B.ByteString

type StaticAPI = "static-content" :>
    ( "composed.js" :> Get '[PlainText] T.Text
    :<|> "difftimeline.css" :> Get '[RawCSS] B.ByteString
    :<|> "syntax-highlight.css" :> Get '[RawCSS] B.ByteString
    :<|> "difftimeline.js"  :> Get '[PlainText] B.ByteString
    :<|> "third_party.js"  :> Get '[PlainText] T.Text
    )

staticServe :: Monad m => ServerT StaticAPI m
staticServe = return (snd composedEmbedded)
          :<|> return (snd diffTimelineCssEmbedded)
          :<|> return (snd syntaxhighlihgtCss)
          :<|> return (snd diffTimlineJsEmbedded)
          :<|> return (snd thirdParty)


serveDifftimeline :: ServerT DifftimelineAPI (ReaderT DiffTimeline SS.Handler)
serveDifftimeline = askParent
                :<|> askCommit
                :<|> askCommitTree
                :<|> compareBranch
                :<|> commit
                :<|> blame
                :<|> commitLister
                :<|> fileComparer
                :<|> branchList
                :<|> initial
                :<|> staticServe
                :<|> base
                :<|> return (snd faviconEmbed)
  where
    base = return $ snd basePageEmbedded 

    branchList = do
      repo <- asks getRepository
      liftIO $ brancheslist repo

    blame blameStartRef path =
      withRepo $ \repo ->
        blameFile repo (T.unpack blameStartRef) (T.unpack path)

    commitLister count ref =
      withRepo $ \repo ->
        commitList repo count . fromHexString $ T.unpack ref

    initial = do
      app <- asks id
      case initialCommand app of
        DiffWorking -> initialCommit app
        DiffFile fname -> initialFile app fname
        DiffBlame fname -> initialBlame app fname
        DiffCompare b1 b2 -> initialBranch app b1 b2

    askCommitTree r =
      let commitSha = fromHexString $ T.unpack r in
      withRepo $ \repo ->
        diffCommitTree repo commitSha

    commit r = 
      let commitSha = fromHexString $ T.unpack r in
      withRepo $ \repo ->
        diffCommit repo 3 True commitSha

    askCommit r =
      let commitSha = fromHexString $ T.unpack r in
      withRepo $ \repo ->
        diffCommit repo 0 False commitSha

    fileComparer key1 file1 key2 file2 =
       withRepo $ \repo ->
          compareFiles repo (toRef key1 file1) (toRef key2 file2)
      where
        toRef k f | k == workingDirRequestTokenT = LocalRef (T.unpack f)
                  | otherwise = RepoRef (T.unpack k) (T.unpack f)

type ServerContext m =
    ( MonadIO m
    , MonadReader DiffTimeline m
    , MonadError ServantErr m )

workingDirRequestTokenT :: Text
workingDirRequestTokenT = "__WORKING_DIR__"

serverError :: (MonadError ServantErr m) => String -> m a
serverError msg = throwError ServantErr
  { errHTTPCode = 200
  , errReasonPhrase = msg
  , errBody = encode $ object ["error" .= msg]
  , errHeaders = []
  }

withRepoAndIgnore :: ServerContext m
                  => (Git -> IgnoredSet -> IO (Either String a)) -> m a
withRepoAndIgnore act = do
  app <- ask
  rez <- liftIO $ act (getRepository app) (getIgnoreSet app)
  case rez of
    Left err -> serverError err
    Right info -> pure info

withRepo :: ServerContext m => (Git -> IO (Either String a)) -> m a
withRepo act = do
    repo <- asks getRepository
    rez <- liftIO $ act repo
    case rez of
      Left err -> serverError err
      Right info -> pure info

compareBranch :: ServerContext m
              => Text -> Text -> m CommitDetail
compareBranch b1 b2 
  | b1 == b2 = serverError "Source is identical to destination"
  | b2 == workingDirRequestTokenT =
      withRepoAndIgnore $ \repo ignored ->
        workingDirectoryChanges repo ignored 3 (T.unpack b1)
  | b1 == workingDirRequestTokenT =
      withRepoAndIgnore $ \repo ignored -> do
        changes <- workingDirectoryChanges repo ignored 3 (T.unpack b2)
        let inverter c =
                c { commitDetailChanges = invertWay <$> commitDetailChanges c }
        return $ inverter <$> changes
  | otherwise =
      withRepo $ \repo ->
          compareBranches repo 3 (T.unpack b1) (T.unpack b2)

askParent :: ServerContext m => Text -> Text -> m ParentFile
askParent initialCommitT filePathes = withRepo $ \repo -> findParentFile repo ref
  where
    file = T.unpack filePathes
    initialCommitRef = T.unpack initialCommitT
    ref | initialCommitT == workingDirRequestTokenT = LocalRef file
        | otherwise = RepoRef initialCommitRef file

renderJson :: (ToJSON a) => a -> LC.ByteString
renderJson = encode . toJSON

initialBranch :: MonadIO m => DiffTimeline -> String -> String -> m LC.ByteString
initialBranch app b1 b2 = do
  let repository = getRepository app
  diffRez <- liftIO $ compareBranches repository 3 b1 b2
  pure $ case diffRez of
    Left err -> "alert(\"Error " <> LC.pack err <> "\");"
    Right rez ->
        "var first_state = " <> renderJson rez <> ";\n" <>
        "Project.state.start_commit( first_state );\n"

initialBlame :: MonadIO m => DiffTimeline -> String -> m LC.ByteString
initialBlame app file = do
  let repository = getRepository app
  blameRez <- liftIO . runExceptT $ do
    headRef <- errorIO "can't read HEAD ref" $ getHead repository
    liftIO $ blameFile repository (show headRef) file

  pure $ case blameRez of
    Left err -> "alert(\"Error " <> LC.pack err <> "\");"
    Right rez ->
        "var first_state = \"" <> renderJson rez <> "\";\n" <>
        "Project.state.start_blame( first_state );"

initialFile :: MonadIO m => DiffTimeline -> FilePath -> m LC.ByteString
initialFile app filename = do
  let repository = getRepository app
      splitedFilename = map BC.pack $ splitDirectories filename
  answer <- liftIO $ basePage repository splitedFilename
  pure $ case answer of
    Left err -> "alert(\"Error " <> LC.pack err <> "\");"
    Right initialAnswer ->
        "var first_state = " <> renderJson initialAnswer <> ";\n" <>
        "Project.state.start_file( first_state );"

initialCommit :: MonadIO m => DiffTimeline -> m LC.ByteString
initialCommit app = do
  let repository = getRepository app
      ignoreSet = getIgnoreSet app

  diffRez <- liftIO . runExceptT $ do
    headRef <- errorIO "Problem reading HEAD ref" $ getHead repository
    workingDirectoryChanges' repository ignoreSet 3 headRef 

  pure $ case diffRez of
    Left err ->  "alert(\"Error " <> LC.pack err <> "\");"
    Right rez ->
       "var first_state = " <> renderJson rez <> ";\n" <>
       "Project.state.start_commit( first_state );\n"

