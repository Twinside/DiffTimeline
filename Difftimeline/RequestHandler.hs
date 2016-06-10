{-# LANGUAGE OverloadedStrings #-}
module Difftimeline.RequestHandler( getRootR
                                  , getQuitR

                                  -- * Dynamic JSON
                                  , getInitialInfoR
                                  , getFileComparisonR
                                  , getBranchComparisonR
                                  , getBranchesR
                                  , getCommitListR
                                  , getJSONExternR
                                  , getCommitTreeR
                                  , getCommitR
                                  , getCommitOverviewR
                                  , getFileParentR
                                  , getBlameR
                                  , getBlameFromRoot

                                  -- * Static files

                                  -- ** Scripts
                                  , getDifftimelineJs
                                  , getFavicon
                                  , getTinysyntaxhighlighter
                                  , getComposed
                                  , getThirdParty

                                  -- ** CSS
                                  , getDifftimelineCss
                                  , getSyntax_highlight
                                  ) where

import Difftimeline.Import
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad.Error( runErrorT )

import System.Directory( doesFileExist )
import System.Exit( exitSuccess )
import System.FilePath( splitDirectories  )
import Data.Git( Git )
import Data.Git.Ref( fromHexString )
import Data.Aeson( encode )

import Text.Language.Closure( renderClosureEnvironment )
import Difftimeline.Externs
import Difftimeline.GitQuery
import Difftimeline.StaticFiles
import Difftimeline.GitIgnore( IgnoredSet )
import Difftimeline.Diff( invertWay )

-- import Yesod.Logger

newtype RepCss = RepCss Content

instance ToContent RepCss where
    toContent (RepCss c) = c

instance ToTypedContent RepCss where
    toTypedContent (RepCss c) = toTypedContent (typeCss, c)

newtype RepRawHtml = RepRawHtml Content

instance ToContent RepRawHtml where
    toContent (RepRawHtml c) = c

instance ToTypedContent RepRawHtml where
    toTypedContent (RepRawHtml c) = toTypedContent (typeHtml, c)

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepRawHtml
getRootR = fetchStatic RepRawHtml basePageEmbedded 

fetchStatic :: (ToContent txt)
            => (Content -> a) -> (FilePath, txt)
            -> Handler a
fetchStatic outputType (path, str) = do
  app <- getYesod
  case getDevMode app of
    Nothing -> return . outputType $ toContent str
    Just devPath -> do
        let devName = devPath ++ "/" ++ path
        fetchable <- liftIO $ doesFileExist devName 
        if fetchable
           then return . outputType . toContent =<< liftIO (B.readFile devName)
           else return . outputType $ toContent str

getDifftimelineCss, getSyntax_highlight :: Handler RepCss
getDifftimelineCss = fetchStatic RepCss diffTimelineCssEmbedded
getSyntax_highlight = fetchStatic RepCss syntaxhighlihgtCss

getDifftimelineJs, getFavicon, getTinysyntaxhighlighter, 
    getThirdParty, getComposed :: Handler RepPlain
getDifftimelineJs = fetchStatic RepPlain diffTimlineJsEmbedded
getFavicon = fetchStatic RepPlain faviconEmbed
getTinysyntaxhighlighter = fetchStatic RepPlain tinySyntaxHighlightJs
getComposed = fetchStatic RepPlain composedEmbedded
getThirdParty = fetchStatic RepPlain thirdParty

getBlameFromRoot :: String -> [Text] -> Handler RepJson
getBlameFromRoot rootCommit filePathes = withRepository extractor
    where file = T.unpack $ T.intercalate (T.pack "/") filePathes
          extractor repository = blameFile repository rootCommit file

getBlameR :: String -> [Text] -> Handler RepJson
getBlameR rootCommit filePathes = withRepository extractor
    where file = T.unpack $ T.intercalate (T.pack "/") filePathes
          extractor repository =
              blameFile repository rootCommit file

getFileParentR :: String -> [Text] -> Handler RepJson
getFileParentR initialCommit filePathes = do
    app <- getYesod
    let repository = getRepository app
        file = T.unpack $ T.intercalate (T.pack "/") filePathes
        ref | initialCommit == workingDirRequestToken = LocalRef file
            | otherwise = RepoRef initialCommit file
    rez <- liftIO $ findParentFile repository ref
    case rez of
       Left err -> pure . repJson $ object ["error" .= err]
       Right info -> pure . repJson $ toJSON info

withRepository :: (ToJSON a)
               => (Git -> IO (Either String a)) -> Handler RepJson
withRepository act = do
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO $ act repository
    case rez of
        Left err -> pure . repJson $ object ["error" .= err]
        Right info -> pure . repJson $ toJSON info

withRepositoryAndIgnore :: (ToJSON a)
                        => (Git -> IgnoredSet -> IO (Either String a)) -> Handler RepJson
withRepositoryAndIgnore act = do
    app <- getYesod
    let repository = getRepository app
        ignoreSet = getIgnoreSet app
    rez <- liftIO $ act repository ignoreSet
    case rez of
        Left err -> pure . repJson $ object ["error" .= err]
        Right info -> pure . repJson $ toJSON info


getCommitOverviewR :: String -> Handler RepJson
getCommitOverviewR commitSha = withRepository extractor
    where extractor repository = do
              rez <- diffCommit repository 0 False $ fromHexString commitSha
              return $ commitDetailChanges <$> rez

getCommitR :: String -> Handler RepJson
getCommitR commitSha = withRepository extractor
    where extractor repository =
              diffCommit repository 3 True $ fromHexString commitSha

getCommitTreeR :: String -> Handler RepJson
getCommitTreeR commitSha = withRepository extractor
    where extractor repository =
              diffCommitTree repository $ fromHexString commitSha

getJSONExternR :: Handler RepPlain
getJSONExternR = return . RepPlain . toContent
			   $ renderClosureEnvironment difftimelineEnv 

getCommitListR :: Int -> String -> Handler RepJson
getCommitListR count commitSha = withRepository extractor
    where extractor repository =
              commitList repository count $ fromHexString commitSha 

getInitialCommit :: Handler RepPlain
getInitialCommit = do
  app <- getYesod
  let repository = getRepository app
      ignoreSet = getIgnoreSet app

  diffRez <- liftIO . runErrorT $ do
      headRef <- errorIO "Problem reading HEAD ref" $ getHead repository
      workingDirectoryChanges' repository ignoreSet 3 headRef 

  return . RepPlain . toContent $ case diffRez of
    Left err ->  "alert(\"Error " <> LC.pack err <> "\");"
    Right rez ->
            "var first_state = " <> renderJson rez <> ";\n" <>
            "Project.state.start_commit( first_state );\n"

renderJson :: (ToJSON a) => a -> LC.ByteString
renderJson = encode . toJSON

getInitialFile :: FilePath -> Handler RepPlain
getInitialFile filename = do
    repository <- getRepository <$> getYesod
    let splitedFilename = map BC.pack $ splitDirectories filename
    answer <- liftIO $ basePage repository splitedFilename
    let rendered = case answer of
            Left err -> "alert(\"Error " <> LC.pack err <> "\");"
            Right initialAnswer ->
                "var first_state = " <> renderJson initialAnswer <> ";\n" <>
                "Project.state.start_file( first_state );"

    return . RepPlain $ toContent rendered


getBranchesR :: Handler RepJson
getBranchesR = withRepository extractor
  where extractor repo = Right <$> brancheslist repo

workingDirRequestToken :: String
workingDirRequestToken = "__WORKING_DIR__"

getBranchComparisonR :: String -> String -> Handler RepJson
getBranchComparisonR b1 b2 
    | b1 == b2 = withRepository (\_ -> return val)
            where val :: Either String Int
                  val = Left "Source is identical to destination"
getBranchComparisonR b1 b2 
    | b2 == workingDirRequestToken = withRepositoryAndIgnore extractor
        where extractor repo ignored = workingDirectoryChanges repo ignored 3 b1
getBranchComparisonR b1 b2 
    | b1 == workingDirRequestToken = withRepositoryAndIgnore extractor
        where inverter c =
                c { commitDetailChanges = invertWay <$> commitDetailChanges c }
              extractor repo ignored = do
                changes <- workingDirectoryChanges repo ignored 3 b2
                return $ inverter <$> changes
getBranchComparisonR b1 b2 = withRepository extractor
  where extractor repo = compareBranches repo 3 b1 b2

getFileComparisonR :: String -> String -> String -> [Text] -> Handler RepJson
getFileComparisonR key1 file1 key2 file2 = withRepository extractor
  where extractor repo = compareFiles repo (toRef key1 file1) (toRef key2 file)

        toRef k f | k == workingDirRequestToken = LocalRef f
                  | otherwise = RepoRef k f

        file = T.unpack $ T.intercalate (T.pack "/") file2

getInitialBranch :: String -> String -> Handler RepPlain
getInitialBranch b1 b2 = do
  repository <- getRepository <$> getYesod
  diffRez <- liftIO $ compareBranches repository 3 b1 b2
  return . RepPlain . toContent $ case diffRez of
    Left err -> "alert(\"Error " <> LC.pack err <> "\");"
    Right rez ->
        "var first_state = " <> renderJson rez <> ";\n" <>
        "Project.state.start_commit( first_state );\n"

getInitialBlame :: String -> Handler RepPlain
getInitialBlame file = do
  repository <- getRepository <$> getYesod
  blameRez <- liftIO . runErrorT $ do
      headRef <- errorIO "can't read HEAD ref" $ getHead repository
      liftIO $ blameFile repository (show headRef) file

  return . RepPlain . toContent $ case blameRez of
    Left err -> "alert(\"Error " <> LC.pack err <> "\");"
    Right rez ->
        "var first_state = \"" <> renderJson rez <> "\";\n" <>
        "Project.state.start_blame( first_state );"

getInitialInfoR :: Handler RepPlain
getInitialInfoR = do
    app <- getYesod
    case initialCommand app of
        DiffWorking -> getInitialCommit
        DiffFile fname -> getInitialFile fname
        DiffBlame fname -> getInitialBlame fname
        DiffCompare b1 b2 -> getInitialBranch b1 b2

getQuitR :: Handler RepJson
getQuitR = do
    _ <- returnJson $ object ["ok" .= True]
    liftIO exitSuccess

