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

import System.FilePath( (</>), makeRelative, takeDirectory,
                        normalise, splitPath )
import qualified System.FilePath as FP
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

simplifyPath :: FilePath -> FilePath
simplifyPath = map subst . FP.joinPath . inner . splitPath . normalise 
    where subst '\\' = '/'
          subst a = a

          inner :: [FilePath] -> [FilePath]
          inner []          = []
          inner ("./":xs)    = inner xs
          inner (".\\":xs)    = inner xs
          inner (_:"../":xs) = inner xs
          inner (_:"..\\":xs) = inner xs
          inner (x:xs)      = x : inner xs

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: FilePath -> AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication fname conf logger = do
    s <- staticSite
    initRepo <- initRepository logger
    cwd <- getCurrentDirectory
    let initPath = simplifyPath $ makeRelative (takeDirectory $ gitRepoPath initRepo) (cwd </> fname)
        foundation = DiffTimeline conf setLogger s initRepo initPath
    liftIO . logString logger $ "Initial file : " ++ initPath
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

