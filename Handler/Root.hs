{-# LANGUAGE OverlappingInstances #-}
module Handler.Root where

import Import
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BC
import Yesod.Json
import Yesod.Logger
import Text.Julius( julius, renderJavascript )

import System.Exit( exitSuccess )
import System.FilePath( splitDirectories  )
import Data.Text.Encoding( decodeUtf8 )
import Data.Git.Ref( Ref, toHex, toHexString, fromHexString )
import Diff
import GitQuery

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = sendFile "text/html" "static-content/base_page.html"

refToText :: Ref -> T.Text
refToText = decodeUtf8 . toHex

diffToJson :: DiffCommand -> [(T.Text, Value)]
diffToJson (DiffCommand way bego begdest s) =
         [ "way" .= wayText way
         , "orig_idx" .= bego
         , "dest_idx" .= begdest
         , "size"     .= s]
    where wayText DiffAddition = "+" :: T.Text
          wayText DiffDeletion = "-"

commitTreeDiffToJson :: CommitTreeDiff -> Value
commitTreeDiffToJson (AddElement name r) =
  object ["kind" .= ("addition" :: T.Text), "name" .= name
         ,"hash" .= toHexString r]
commitTreeDiffToJson (DelElement name r) =
  object ["kind" .= ("deletion" :: T.Text), "name" .= name
         ,"hash" .= toHexString r]
commitTreeDiffToJson (ModifyElement name r diffs) =
  object $ ["kind" .= ("modification" :: T.Text), "name" .= name
           ,"hash" .= toHexString r] ++ diffElement 
        where diffElement | null diffs = []
                          | otherwise = ["diff" .= map adder diffs]
              adder (d, content) = object $ diffToJson d ++ ["data" .= content]

commitDetailToJson :: CommitDetail -> Value
commitDetailToJson detail = object $ 
    [ "message"      .= commitDetailMessage detail
    , "parents_sha"  .= (array . map toHexString $ commitDetailParents detail)
    , "key"          .= (toHexString $ commitDetailKey detail)
    , "author"       .= commitDetailAuthor detail
    , "file_changes" .= array (map commitTreeDiffToJson
                                        $ commitDetailChanges detail)
    ] 
    
commitPathToJson :: CommitPath -> Value
commitPathToJson cp =
    object [ "commit" .= refToText (pathCommitRef cp)
           , "parent_commit" .= refToText (pathParentRef cp)
           , "message" .= pathMessage cp
           ]

parentFileToJson :: ParentFile -> Value
parentFileToJson p =
    object ["data"          .= fileData p
           ,"filekey"       .= (refToText $ fileRef p)
           ,"parent_commit" .= (refToText . head $ parentRef p)
           ,"message"       .= fileMessage p
           ,"diff"          .= (array . map (object . diffToJson) $ fileDiff p)
           ,"path"          .= map commitPathToJson (commitPath p)
           ,"key"           .= refToText (commitRef p)
           ]

getFileParentR :: [Text] -> Handler RepJson
getFileParentR filePathes = do
    let file = T.unpack $ T.intercalate (T.pack "/") filePathes
    app <- getYesod
    params <- reqGetParams <$> getRequest
    let logger = getLogger app
    liftIO . logString logger $ "file : " ++ file
    liftIO . logString logger $ "param : " ++ show params
    let commitRefs = map snd $ filter (\(n, _) -> n == T.pack "commit") params
        Just fRef = lookup (T.pack "last_file") params
        repository = getRepository app

    rez <- liftIO $ findParentFile repository
                                    (T.unpack fRef)
                                    (T.unpack $ head commitRefs) file

    -- liftIO $ logLazyText logger rendered
    case rez of
       Left err -> jsonToRepJson $ object ["error" .= err]
       Right info -> jsonToRepJson $ parentFileToJson info

getCommitOverviewR :: String -> Handler RepJson
getCommitOverviewR commitSha =  do
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO . diffCommit repository False $ fromHexString commitSha
    case rez of
        Left err -> jsonToRepJson $ object ["error" .= err]
        Right info ->
            jsonToRepJson . map commitTreeDiffToJson  $ commitDetailChanges info



getCommitR :: String -> Handler RepJson
getCommitR commitSha = do
    app <- getYesod
    let repository = getRepository app
    rez <- liftIO . diffCommit repository True $ fromHexString commitSha
    case rez of
        Left err -> jsonToRepJson $ object ["error" .= err]
        Right nfo ->
            jsonToRepJson $ commitDetailToJson nfo

javascriptize :: T.Text -> T.Text
javascriptize = T.replace quo quoRep
              . T.replace nl nlRep
              . T.replace bs bsRep
              . T.replace cr crRep
    where (cr, crRep) = (T.pack "\r",  T.pack "\\r")
          (nl, nlRep) = (T.pack "\n",  T.pack "\\n")
          (bs, bsRep) = (T.pack "\\",  T.pack "\\\\")
          (quo, quoRep) = (T.pack "\"",  T.pack "\\\"")

getInitialInfoR :: Handler RepPlain
getInitialInfoR = do
    app <- getYesod
    let logger = getLogger app
        repository = getRepository app
        filename = initialPath app
        splitedFilename = map BC.pack $ splitDirectories filename
    liftIO . logString logger $ "Loading " ++ filename
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
    -- liftIO $ logLazyText logger rendered
    return . RepPlain $ toContent rendered

getQuitR :: Handler RepJson
getQuitR = do
    _ <- jsonToRepJson $ object [("ok", True)]
    liftIO $ exitSuccess

