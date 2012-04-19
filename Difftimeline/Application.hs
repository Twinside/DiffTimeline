{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings (parseExtra)
import Settings.StaticFiles (staticSite)
import System.Directory( getCurrentDirectory )
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)
import Yesod.Default.Handlers (getFaviconR, getRobotsR)
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS, logString)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction, logString)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import Network.Wai (Application)

import System.FilePath( (</>), makeRelative )
import Data.Git.Repository( Git, openRepo, findRepository, gitRepoPath )
-- Import all relevant handler modules here.
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "DiffTimeline" resourcesDiffTimeline

initRepository :: Logger -> IO Git
initRepository logger = do
    Just dir <- getCurrentDirectory >>= findRepository
    let repoPath = dir </> ".git"
    logString logger $ "Trying to open git: " ++ repoPath
    openRepo repoPath

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: FilePath -> AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication fname conf logger = do
    s <- staticSite
    initRepo <- initRepository logger
    cwd <- getCurrentDirectory
    let initPath = makeRelative (cwd </> fname) $ gitRepoPath initRepo
        foundation = DiffTimeline conf setLogger s initRepo initPath
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader $ getApplication ""
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

