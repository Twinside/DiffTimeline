{-# LANGUAGE OverlappingInstances #-}
module Handler.Root where

import Import
import Control.Monad( when )
import qualified Data.ByteString.Char8 as BC
import Yesod.Json
import Text.Julius( julius, renderJavascript, ToJavascript( .. ) )
import GitQuery
import Data.Git.Ref( Ref, toHexString )
import qualified Data.Text as T

import Yesod.Logger

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = sendFile "text/html" "../static-content/base_page.html"

getFileParentR :: String -> Handler RepJson
getFileParentR file = jsonToRepJson $ object [("file_parent", file)]

getCommitOverviewR :: String -> Handler RepJson
getCommitOverviewR commitSha = 
    jsonToRepJson $ object [("commit_overview", commitSha)]

getCommitR :: String -> Handler RepJson
getCommitR commitSha =
    jsonToRepJson $ object [("commit", commitSha)]

instance ToJavascript Ref where
    toJavascript v = toJavascript ("\"" :: String)
           `mappend` toJavascript (toHexString v)
           `mappend` toJavascript ("\"" :: String)

instance (ToJavascript a) => ToJavascript [a] where
    toJavascript [] = toJavascript ("[]" :: String)
    toJavascript [x] = toJavascript ("[" :: String)
             `mappend` toJavascript x
             `mappend` toJavascript ("]" :: String)
    toJavascript (f:fs) =  toJavascript ("[" :: String)
                `mappend` concater (toJavascript f) fs
                `mappend` toJavascript ("]" :: String)
        where sep = toJavascript (", " :: String)

              concater acc     [] = acc
              concater acc (x:xs) = concater sub xs
                    where sub = acc `mappend` sep `mappend`  toJavascript x


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
        filename = "difftimeline.rb" :: String
    answer <- liftIO $ basePage logger repository [BC.pack filename]
    let rendered = case answer of
            Nothing ->
                renderJavascript $ [julius| alert("Uknown git error"); |] ("" :: Text)

            Just initialAnswer ->
                    renderJavascript $ [julius|
                        var first_state = { file: "#{javascriptize $ T.pack filename}",
                                             key: #{commitRef initialAnswer},
                                         filekey: #{fileRef initialAnswer},
                                   parent_commit: #{parentRef initialAnswer}, 
                                            data: "#{javascriptize $ fileData initialAnswer}",
                                         message: "#{javascriptize $ fileMessage initialAnswer}",
                                            diff: [],
                                            path: [] };

                        application_state.start_file( first_state ); |] ("" :: Text)
    -- liftIO $ logLazyText logger rendered
    return . RepPlain $ toContent rendered

postQuitR :: Handler RepJson
postQuitR = jsonToRepJson $ object [("ok", True)]

