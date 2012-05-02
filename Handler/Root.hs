{-# LANGUAGE OverlappingInstances #-}
module Handler.Root where

import Import
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LC
import Text.Julius( julius, renderJavascript )

import Yesod.Json( jsonToRepJson )
import System.Exit( exitSuccess )
import System.FilePath( splitDirectories  )
import Data.Git( getHead, toHexString, fromHexString )
import GitQuery
import StaticFiles
import Data.Aeson

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

getICanHaz_min,  getDifftimelineJs, getFavicon, getJquery, 
    getTinysyntaxhighlighter, getUnderscore_min :: Handler RepPlain
getICanHaz_min = return . RepPlain $ toContent icanHazEmbedded 
getDifftimelineJs = return . RepPlain $ toContent diffTimlineJsEmbedded 
getFavicon = return . RepPlain $ toContent faviconEmbed
getJquery = return . RepPlain $ toContent jqueryEmbedded 
getTinysyntaxhighlighter = return . RepPlain $ toContent tinySyntaxHighlightJs 
getUnderscore_min = return . RepPlain $ toContent underscoreJs

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

getCommitOverviewR :: String -> Handler RepJson
getCommitOverviewR commitSha =  do
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO . diffCommit repository False $ fromHexString commitSha
    case rez of
        Left err -> jsonToRepJson $ object ["error" .= err]
        Right info ->
            jsonToRepJson . toJSON $ commitDetailChanges info



getCommitR :: String -> Handler RepJson
getCommitR commitSha = do
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO . diffCommit repository True $ fromHexString commitSha
    case rez of
        Left err -> jsonToRepJson $ object ["error" .= err]
        Right nfo -> jsonToRepJson $ toJSON nfo

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
      strictify = BC.concat . LC.toChunks
  Just headRef <- liftIO $ getHead repository
  diffRez <- liftIO $ diffCommit repository True headRef 
  return . RepPlain . toContent $ case diffRez of
    Left err ->
        renderJavascript $ [julius| alert("Error #{err}"); |] ("" :: Text)

    Right rez ->
        renderJavascript $ [julius|
            var first_state = #{decodeUtf8 $ strictify $ encode $ toJSON rez};
            application_state.start_commit( first_state ); |] ("" :: Text)

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
                    renderJavascript $ [julius|
                        var first_state = { file: "#{javascriptize $ T.pack filename}",
                                             key: "#{toHexString $ commitRef initialAnswer}",
                                         filekey: "#{toHexString $ fileRef initialAnswer}",
                                   parent_commit: "#{toHexString $ head $ parentRef initialAnswer}", 
                                            data: "#{javascriptize $ fileData initialAnswer}",
                                         message: "#{javascriptize $ fileMessage initialAnswer}",
                                            diff: [],
                                            path: [] };

                        application_state.start_file( first_state ); |] ("" :: Text)
    return . RepPlain $ toContent rendered

getInitialInfoR :: Handler RepPlain
getInitialInfoR = do
    app <- getYesod
    case initialPath app of
        Nothing -> getInitialCommit
        Just fname -> getInitialFile fname

getQuitR :: Handler RepJson
getQuitR = do
    _ <- jsonToRepJson $ object ["ok" .= True]
    liftIO $ exitSuccess

