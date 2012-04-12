module Handler.Root where

import Import
import qualified Data.ByteString as B
import Yesod.Json
import Text.Julius( julius, renderJavascript )
import GitQuery

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

getInitialInfoR :: Handler RepPlain
getInitialInfoR = do
    app <- getYesod
    let repository = getRepository app
        filename = "difftimeline.rb"
    Just initialAnswer <- liftIO $ basePage repository [B.pack "difftimeline.rb"]
    return . RepPlain . toContent . renderJavascript $ [julius|
        var first_state = { file: #{filename},
                             key: #{commitRef initialAnswer},
                         filekey: #{fileRef initialAnswer},
                   parent_commit: #{head $ parentRef initialAnswer},
                            data: #{fileData initialAnswer},
                         message: #{fileMessage initialAnswer},
                            diff: [],
                            path: [] };

        application_state.start_file( first_state ); |] ("" :: Text)

postQuitR :: Handler RepJson
postQuitR = jsonToRepJson $ object [("ok", True)]

