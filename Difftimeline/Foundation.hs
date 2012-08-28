module Difftimeline.Foundation
    ( DiffTimeline (..)
    , Route (..)
    , Command (..)
    , DiffTimelineMessage (..)
    , resourcesDiffTimeline
    , Handler
    , Widget
    , module Yesod.Core
    , liftIO
    ) where

import Prelude
import Yesod.Core -- hiding (Route)
import Yesod.Default.Config
import Yesod.Logger (Logger, logMsg, formatLogText)
import Control.Monad.IO.Class (liftIO)
import Data.Git( Git )

data Command = DiffCompare String String
             | DiffBlame String
             | DiffFile String
             | DiffWorking

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data DiffTimeline = DiffTimeline
    { settings  :: AppConfig DefaultEnv ()
    , getLogger :: Logger
    , getDevMode :: Maybe FilePath
    , getRepository :: Git
    , initialCommand   :: Command
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

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    makeSessionBackend _ = return Nothing

