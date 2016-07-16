{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Difftimeline.Application
    ( getApplication
    , getDebugStart
    , Command( .. )
    ) where

import Difftimeline.Import
import System.Directory( getCurrentDirectory, doesDirectoryExist, doesFileExist )
{-import Network.Wai( Application )-}
import Yesod.Default.Config( AppConfig, DefaultEnv )
import Yesod.Default.Handlers (getFaviconR)
import qualified Filesystem.Path as FPR
import qualified Filesystem.Path.Rules as FPR
import qualified Data.ByteString.Char8 as BC

import System.FilePath( (</>), makeRelative, takeDirectory,
                        normalise, splitPath, isRelative )
import qualified System.FilePath as FP
import Data.Git.Storage( Git, isRepo, openRepo, gitRepoPath )
import Control.Monad.Trans.Except( runExceptT )
-- Import all relevant handler modules here.
import Difftimeline.RequestHandler
import Difftimeline.GitIgnore( IgnoredSet, loadIgnoreFile, emptyIgnoreSet )
import Difftimeline.GitQuery( getHead, workingDirectoryChanges' )
import System.Exit( exitFailure )

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "DiffTimeline" resourcesDiffTimeline

unpackPath :: Git -> FilePath
unpackPath = BC.unpack . FPR.encode FPR.posix . gitRepoPath 

packPath :: String -> FPR.FilePath
packPath = FPR.decode FPR.posix . BC.pack

-- | Find the nearest repository in the file tree starting
-- at the given directory
findRepository :: FilePath -> IO (Maybe FilePath)
findRepository = inner ""
  where inner prevDir dir | prevDir == dir || dir == "." = return Nothing
        inner _ dir = do
            isGitRepo <- isRepo . packPath $ dir </> ".git"
            if isGitRepo then return $ Just dir
                         else inner dir $ takeDirectory dir

initRepository :: FilePath -> IO (FilePath, Git)
initRepository startDir = do
    maybeRepo <- findRepository startDir
    case maybeRepo of
       Just dir -> (dir,) <$> (openRepo . packPath $ dir </> ".git")
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

loadIgnoreSet :: FilePath -> IO IgnoredSet
loadIgnoreSet path = do
    let ignoreFile = path </> ".gitignore"
    isExisting <- doesFileExist ignoreFile
    if isExisting
        then loadIgnoreFile ignoreFile
        else pure emptyIgnoreSet

getDebugStart :: IO ()
getDebugStart = do
  initDir <- getCurrentDirectory 
  (repoDir, initRepo) <- initRepository initDir 
  ignoreSet <- loadIgnoreSet repoDir
  Just headRef <- getHead initRepo
  rez <- runExceptT $ workingDirectoryChanges' initRepo ignoreSet 2 headRef
  print rez

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: Maybe FilePath -> Command -> AppConfig DefaultEnv ()
               -> IO DiffTimeline
getApplication devModePath (DiffBlame fname) conf = do
    cwd <- getCurrentDirectory 
    isDir <- doesDirectoryExist fname
    let absName
          | isRelative fname = simplifyPath $ cwd </> fname
          | otherwise = simplifyPath fname

        name | isDir = absName ++ "/"
             | otherwise = absName

    (repoDir, initRepo) <- initRepository $ takeDirectory name
    initPath <- if isDir 
        then pure DiffWorking
        else do
          let absPath = takeDirectory $ unpackPath initRepo
              relPath = simplifyPath $ makeRelative absPath name
          pure $ DiffBlame relPath

    ignoreSet <- loadIgnoreSet repoDir
    return $ DiffTimeline conf devModePath initRepo initPath ignoreSet

getApplication devModePath (DiffFile fname) conf = do
    cwd <- getCurrentDirectory 
    isDir <- doesDirectoryExist fname
    let absName
          | isRelative fname = simplifyPath $ cwd </> fname
          | otherwise = simplifyPath fname

        name | isDir = absName ++ "/"
             | otherwise = absName

    (repoDir, initRepo) <- initRepository $ takeDirectory name
    initPath <- if isDir 
        then pure DiffWorking
        else do
          let absPath = takeDirectory $ unpackPath initRepo
              relPath = simplifyPath $ makeRelative absPath name
          pure $ DiffFile relPath

    ignoreSet <- loadIgnoreSet repoDir
    return $ DiffTimeline conf devModePath initRepo initPath ignoreSet

getApplication devModePath cmd conf = do
    initDir <- getCurrentDirectory 
    (repoDir, initRepo) <- initRepository initDir 
    ignoreSet <- loadIgnoreSet repoDir
    return $ DiffTimeline conf devModePath initRepo cmd ignoreSet

