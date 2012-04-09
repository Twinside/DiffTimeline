module Handler.Root where

import Import
import Yesod.Json
import Text.Julius( julius, renderJavascript )

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "Difftimeline homepage"
        $(widgetFile "homepage")

getFileParentR :: String -> Handler RepJson
getFileParentR file = jsonToRepJson $ object [("file_parent", file)]

getCommitOverviewR :: String -> Handler RepJson
getCommitOverviewR commitSha = 
    jsonToRepJson $ object [("commit_overview", commitSha)]

getCommitR :: String -> Handler RepJson
getCommitR commitSha =
    jsonToRepJson $ object [("commit", commitSha)]

getInitialInfoR :: Handler RepPlain
getInitialInfoR = return . RepPlain . toContent . renderJavascript $ [julius|
        var first_state = { file: "test.c", // "@tracked_path"
            key: "0000000000000000000", // "current_head"
           filekey: "1111111111111111111", // file.sha
           parent_commit: "2222222222222222222", // commit.parents_sha[0]
           data: "// content\n// line1\n// line2\nint main(int argc, char argv[]) { return 0; }", // clean_data.to_json
           message: "Test message", // clean_message.to_json
           diff: [],
           path: [] };

        application_state.start_file( first_state ); |] ("" :: Text)

postQuitR :: Handler RepJson
postQuitR = jsonToRepJson $ object [("ok", True)]

