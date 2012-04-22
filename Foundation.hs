module Foundation
    ( DiffTimeline (..)
    , Route (..)
    , DiffTimelineMessage (..)
    , resourcesDiffTimeline
    , Handler
    , Widget
    , module Yesod.Core
    , module Settings
    , liftIO
    ) where

import Prelude
import Yesod.Core -- hiding (Route)
import Yesod.Default.Config
-- import Yesod.Default.Util (addStaticContentExternal)
-- import Yesod.Static
-- import Settings.StaticFiles
import Yesod.Logger (Logger, logMsg, formatLogText)
import qualified Settings
import Settings (Extra (..), widgetFile)
import Control.Monad.IO.Class (liftIO)
-- import Web.ClientSession (getKey)
-- import Text.Hamlet (hamletFile)
import Data.Git.Repository( Git )

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data DiffTimeline = DiffTimeline
    { settings  :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getRepository :: Git
    , initialPath   :: FilePath
    }

-- Set up i18n messages. See the message folder.
mkMessage "DiffTimeline" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype DiffTimelineRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route DiffTimeline = DiffTimelineRoute
-- * Creates the value resourcesDiffTimeline which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- DiffTimeline. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the DiffTimelineRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "DiffTimeline" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod DiffTimeline where
    approot = ApprootMaster $ appRoot . settings

    defaultLayout _ = return . RepHtml $ toContent ("" :: String)

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride _ _ = Nothing

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    {-addStaticContent = addStaticContentExternal (const $ Left ()) base64md5 Settings.staticDir (StaticR . flip StaticRoute [])-}

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody
