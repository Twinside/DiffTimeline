{-# LANGUAGE OverlappingInstances #-}
module Difftimeline.RequestHandler where

import Difftimeline.Import
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.Julius( julius, renderJavascript )

import Yesod.Json( jsonToRepJson )
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
getRootR = return $ RepHtml $ toContent basePageEmbedded 

newtype RepCss = RepCss Content
instance HasReps RepCss where
    chooseRep (RepCss c) _ = return (typeCss, c)

getDifftimelineCss, getSyntax_highlight, getScreen :: Handler RepCss
getDifftimelineCss = return . RepCss $ toContent diffTimelineCssEmbedded
getScreen =  return . RepCss $ toContent screenCssEmbedded
getSyntax_highlight = return . RepCss $ toContent  syntaxhighlihgtCss

getICanHaz_min,  getDifftimelineJs, getFavicon, getJquery, getJqueryUI,
    getTinysyntaxhighlighter :: Handler RepPlain
getICanHaz_min = return . RepPlain $ toContent icanHazEmbedded 
getDifftimelineJs = return . RepPlain $ toContent diffTimlineJsEmbedded 
getFavicon = return . RepPlain $ toContent faviconEmbed
getJquery = return . RepPlain $ toContent jqueryEmbedded 
getJqueryUI = return . RepPlain $ toContent jqueryUiEmbedded 
getTinysyntaxhighlighter = return . RepPlain $ toContent tinySyntaxHighlightJs 

getFileParentR :: [Text] -> Handler RepJson
getFileParentR filePathes = do
    let file = T.unpack $ T.intercalate (T.pack "/") filePathes
    app <- getYesod
    params <- reqGetParams <$> getRequest
    let commitRefs = map snd $ filter (\(n, _) -> n == T.pack "commit") params
        Just fRef = lookup (T.pack "last_file") params
        repository = getRepository app

    rez <- liftIO $ findParentFile repository
                                    (T.unpack fRef)
                                    (T.unpack $ head commitRefs) file
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
    let logger = getLogger app
        repository = getRepository app
        splitedFilename = map BC.pack $ splitDirectories filename
    answer <- liftIO $ basePage logger repository splitedFilename
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


getInitialInfoR :: Handler RepPlain
getInitialInfoR = do
    app <- getYesod
    case initialCommand app of
        DiffWorking -> getInitialCommit
        DiffFile fname -> getInitialFile fname
        DiffCompare "" "" -> getBranchComparer
        DiffCompare b1 b2 -> getInitialBranch b1 b2

getQuitR :: Handler RepJson
getQuitR = do
    _ <- jsonToRepJson $ object ["ok" .= True]
    liftIO $ exitSuccess

