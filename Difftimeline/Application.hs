{-# OPTIONS_GHC -fno-warn-orphans #-}
module Difftimeline.Application( getApplication, Command( .. ) ) where

import Difftimeline.Import
import System.Directory( getCurrentDirectory, doesDirectoryExist )
import Network.Wai( Application )
{-import System.Log.FastLogger( Logger, toLogStr, loggerPutStr )-}
import Yesod.Default.Config
import Yesod.Default.Handlers (getFaviconR)

import System.FilePath( (</>), makeRelative, takeDirectory,
                        normalise, splitPath, isRelative )
import qualified System.FilePath as FP
import Data.Git( Git, openRepo, findRepository, gitRepoPath )
-- Import all relevant handler modules here.
import Difftimeline.RequestHandler
import System.Exit( exitFailure )

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "DiffTimeline" resourcesDiffTimeline

initRepository :: FilePath -> IO Git
initRepository startDir = do
    maybeRepo <- findRepository startDir
    case maybeRepo of
       Just dir -> openRepo $ dir </> ".git"
       Nothing -> do
           putStrLn "Error : no git repository found"
           exitFailure

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
getApplication :: Maybe FilePath -> Command -> AppConfig DefaultEnv ()
               -> IO Application
getApplication devModePath (DiffBlame fname) conf = do
    cwd <- getCurrentDirectory 
    isDir <- doesDirectoryExist fname
    let absName
          | isRelative fname = simplifyPath $ cwd </> fname
          | otherwise = simplifyPath fname

        name | isDir = absName ++ "/"
             | otherwise = absName

    initRepo <- initRepository $ takeDirectory name
    initPath <- if isDir 
        then pure DiffWorking
        else do
          let absPath = takeDirectory $ gitRepoPath initRepo
              relPath = simplifyPath $ makeRelative absPath name
          pure $ DiffBlame relPath

    toWaiAppPlain $ DiffTimeline conf devModePath initRepo initPath

getApplication devModePath (DiffFile fname) conf = do
    cwd <- getCurrentDirectory 
    isDir <- doesDirectoryExist fname
    let absName
          | isRelative fname = simplifyPath $ cwd </> fname
          | otherwise = simplifyPath fname

        name | isDir = absName ++ "/"
             | otherwise = absName

    initRepo <- initRepository $ takeDirectory name
    initPath <- if isDir 
        then pure DiffWorking
        else do
          let absPath = takeDirectory $ gitRepoPath initRepo
              relPath = simplifyPath $ makeRelative absPath name
          pure $ DiffFile relPath

    toWaiAppPlain $ DiffTimeline conf devModePath initRepo initPath

getApplication devModePath cmd conf = do
    initRepo <- getCurrentDirectory >>= initRepository
    toWaiAppPlain $ DiffTimeline conf devModePath initRepo cmd

