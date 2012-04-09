module Difftimeline.GitQuery where

import Difftimeline.Diff
import Data.Git.Repository( Git, findObject )
import Data.Git.Object
import Data.Git.Ref( Ref, fromHexString )

import qualified Data.Text as T

data CommitPath = CommitPath
    { commitRef     :: Ref
    , parentRef     :: Ref
    , pathMessage   :: T.Text
    }

findFirstCommit :: Git -> FilePath -> Ref -> GitObject -> Ref
                -> (GitObject, [CommitPath])
findFirstCommit repository path fileRef lastCommit commitRef = do
    commit <- findObject repository commitRef True
    file <- 
    
    

findParentFile :: Git -> String -> String -> FilePath -> IO [Int]
findParentFile repository lastFileStrSha commitStrSha path = inner
  where prevFileSha = fromHexString lastFileStrSha
        prevCommit = fromHexString commitStrSha

        inner = do 
            commit <- findObject repository prevCommit True


data ParentFile = ParentFile
    { fileData    :: T.Text
    , fileRef     :: Ref
    , parentRef   :: Ref
    , fileMessage :: T.Text
    , commitRef   :: Ref
    , commitPath  :: [CommitPath]
    , fileDiff    :: [DiffCommand]
    }
