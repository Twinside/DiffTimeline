{-# LANGUAGE OverlappingInstances #-}
module Difftimeline.RequestHandler where

import Difftimeline.Import
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.Julius( julius, renderJavascript )

import Yesod.Json( jsonToRepJson )
import System.Directory( doesFileExist )
import System.Exit( exitSuccess )
import System.FilePath( splitDirectories  )
import Data.Git( Git, getHead, fromHexString )
import Data.Aeson( ToJSON, toJSON, object, (.=), encode )

import Text.Language.Closure( renderClosureEnvironment )

import Difftimeline.Externs
import Difftimeline.GitQuery
import Difftimeline.StaticFiles

-- import Yesod.Logger

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = fetchStatic RepHtml basePageEmbedded 

newtype RepCss = RepCss Content
instance HasReps RepCss where
    chooseRep (RepCss c) _ = return (typeCss, c)

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

getDifftimelineCss, getSyntax_highlight, getScreen :: Handler RepCss
getDifftimelineCss = fetchStatic RepCss diffTimelineCssEmbedded
getScreen =  fetchStatic RepCss screenCssEmbedded
getSyntax_highlight = fetchStatic RepCss syntaxhighlihgtCss

getICanHaz_min,  getDifftimelineJs, getFavicon, getJquery, getJqueryUI,
    getTinysyntaxhighlighter, getJqueryHotKeys, getJqueryScrollTo :: Handler RepPlain
getICanHaz_min = fetchStatic RepPlain icanHazEmbedded 
getDifftimelineJs = fetchStatic RepPlain diffTimlineJsEmbedded 
getFavicon = fetchStatic RepPlain faviconEmbed
getJquery = fetchStatic RepPlain jqueryEmbedded 
getJqueryUI = fetchStatic RepPlain jqueryUiEmbedded 
getTinysyntaxhighlighter = fetchStatic RepPlain tinySyntaxHighlightJs 
getJqueryHotKeys = fetchStatic RepPlain jqueryHotkeysEmbedded
getJqueryScrollTo = fetchStatic RepPlain jqueryScrollToEmbedded

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
    let file = T.unpack $ T.intercalate (T.pack "/") filePathes
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO $ findParentFile repository initialCommit file
    case rez of
       Left err -> jsonToRepJson $ object ["error" .= err]
       Right info -> jsonToRepJson $ toJSON info

withRepository :: (ToJSON a)
               => (Git -> IO (Either String a)) -> Handler RepJson
withRepository act = do
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO $ act repository
    case rez of
        Left err -> jsonToRepJson $ object ["error" .= err]
        Right info -> jsonToRepJson $ toJSON info

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

javascriptize :: T.Text -> T.Text
javascriptize = T.replace quo quoRep
              . T.replace nl nlRep
              . T.replace bs bsRep
              . T.replace cr crRep
    where (cr, crRep) = (T.pack "\r",  T.pack "\\r")
          (nl, nlRep) = (T.pack "\n",  T.pack "\\n")
          (bs, bsRep) = (T.pack "\\",  T.pack "\\\\")
          (quo, quoRep) = (T.pack "\"",  T.pack "\\\"")

getInitialCommit :: Handler RepPlain
getInitialCommit = do
  app <- getYesod
  let repository = getRepository app
  Just headRef <- liftIO $ getHead repository
  diffRez <- liftIO $ workingDirectoryChanges repository 3 headRef 
  return . RepPlain . toContent $ case diffRez of
    Left err ->
        renderJavascript $ [julius| alert("Error #{err}"); |] ("" :: Text)

    Right rez ->
        renderJavascript $ [julius|
            var first_state = #{renderJson rez};
            Project.state.start_commit( first_state ); |] ("" :: Text)

renderJson :: (ToJSON a) => a -> T.Text
renderJson = decodeUtf8 . BC.concat . LC.toChunks . encode . toJSON

getInitialFile :: FilePath -> Handler RepPlain
getInitialFile filename = do
    app <- getYesod
    let repository = getRepository app
        splitedFilename = map BC.pack $ splitDirectories filename
    answer <- liftIO $ basePage repository splitedFilename
    let rendered = case answer of
            Left err ->
                renderJavascript $ [julius| alert("Error #{err}"); |] ("" :: Text)

            Right initialAnswer ->
            	let renderedAnswer = renderJson initialAnswer
				in renderJavascript $ [julius|
                        var first_state = #{renderedAnswer};
                        Project.state.start_file( first_state ); |] ("" :: Text)

    return . RepPlain $ toContent rendered


getBranchesR :: Handler RepJson
getBranchesR = withRepository extractor
  where extractor repo = Right <$> brancheslist repo

getBranchComparer :: Handler RepPlain
getBranchComparer = return . RepPlain . toContent . renderJavascript $
    [julius| Project.state.start_branch_comp(); |] ("" :: Text)

getBranchComparisonR :: String -> String -> Handler RepJson
getBranchComparisonR b1 b2 = withRepository extractor
  where extractor repo = compareBranches repo 3 b1 b2

getFileComparisonR :: String -> String -> String -> [Text] -> Handler RepJson
getFileComparisonR key1 file1 key2 file2 = withRepository extractor
  where extractor repo = compareFiles repo key1 file1 key2 file
        file = T.unpack $ T.intercalate (T.pack "/") file2

getInitialBranch :: String -> String -> Handler RepPlain
getInitialBranch b1 b2 = do
  app <- getYesod
  let repository = getRepository app
  diffRez <- liftIO $ compareBranches repository 3 b1 b2
  return . RepPlain . toContent $ case diffRez of
    Left err ->
        renderJavascript $ [julius| alert("Error #{err}"); |] ("" :: Text)

    Right rez ->
        renderJavascript $ [julius|
            var first_state = #{renderJson rez};
            Project.state.start_commit( first_state ); |] ("" :: Text)

getInitialBlame :: String -> Handler RepPlain
getInitialBlame file = do
  app <- getYesod
  let repository = getRepository app
  Just headRef <- liftIO $ getHead repository
  blameRez <- liftIO $ blameFile repository (show headRef) file
  return . RepPlain . toContent $ case blameRez of
    Left err ->
        renderJavascript $ [julius| alert("Error #{err}"); |] ("" :: Text)

    Right rez ->
        renderJavascript $ [julius|
            var first_state = #{renderJson rez};
            Project.state.start_blame( first_state ); |] ("" :: Text)

getInitialInfoR :: Handler RepPlain
getInitialInfoR = do
    app <- getYesod
    case initialCommand app of
        DiffWorking -> getInitialCommit
        DiffFile fname -> getInitialFile fname
        DiffBlame fname -> getInitialBlame fname
        DiffCompare "" "" -> getBranchComparer
        DiffCompare b1 b2 -> getInitialBranch b1 b2

getQuitR :: Handler RepJson
getQuitR = do
    _ <- jsonToRepJson $ object ["ok" .= True]
    liftIO $ exitSuccess

