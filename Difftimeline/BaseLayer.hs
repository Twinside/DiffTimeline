{-# LANGUAGE TupleSections #-}
module Difftimeline.BaseLayer
    ( SubKind( .. )
    , accessBlob
    , accessCommit
    , accessObject
    , accessTree
    , timeOfAuthor
    , unpackPath
    , detectBinary
    , firstParentRef
    , fetchDirectoryInfo
    , errorIO
    , nullRef
    , decodeUtf8
    , decodeUtf8Lazy
    , batchSubTree
    ) where


import Prelude

import Data.Byteable( toBytes )
import Control.Monad.Trans.Except( ExceptT, throwE )
import Control.Monad.IO.Class( liftIO )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LE
import Data.Text.Encoding( decodeUtf8With )
import Data.Text.Encoding.Error( lenientDecode )
import System.FilePath( (</>) )
import System.Directory( getDirectoryContents, doesFileExist )

import qualified Filesystem.Path.Rules as FP
import Data.Git 
    ( Commit( .. )
    , Person( .. )
    , Tree( .. )
    , Git
    , Ref
    , getObject
    )
import Data.Git.Ref
    ( fromHexString
    )
import Data.Hourglass
    ( timeFromElapsed )
import Foreign.C.Types( CTime( CTime ) )
import Data.Git.Storage
    ( gitRepoPath
    )
import Data.Git.Storage.Object( Object( .. ) )
import Data.Git.Types
    ( GitTime( .. )
    )

import Difftimeline.Contract

data SubKind = KindFile | KindDirectory deriving Show

accessObject :: Git -> Ref -> IO (Maybe Object)
accessObject repo ref = getObject repo ref True

accessCommit :: String -> Git -> Ref -> ExceptT String IO Object
accessCommit s rep ref = do
  rez <- liftIO $ getObject rep ref True
  case rez of
      Nothing -> throwE s
      Just c@(ObjCommit _) -> return c
      Just _ -> throwE s

accessTree :: String -> Git -> Ref -> ExceptT String IO Object
accessTree s rep ref = do
    rez <- liftIO $ getObject rep ref True
    case rez of
        Nothing -> throwE s
        Just c@(ObjTree _) -> return c
        Just _ -> throwE s

accessBlob :: String -> Git -> Ref -> ExceptT String IO Object
accessBlob s rep ref = do
    rez <- liftIO $ getObject rep ref True
    case rez of
        Nothing -> throwE s
        Just c@(ObjBlob _) -> return c
        Just _ -> throwE s

timeOfAuthor :: Person -> Int
timeOfAuthor = unCTime . timeFromElapsed . gitTimeUTC . personTime
  where unCTime (CTime t) = fromIntegral t

unpackPath :: Git -> FilePath
unpackPath = BC.unpack . FP.encode FP.posix . gitRepoPath 

-- | Try to detect binary element given a blob
detectBinary :: L.ByteString -> Bool
detectBinary = L.any (== 0) . L.take binaryDetectionSize
    where binaryDetectionSize = 8 * 1024

nullRef :: Ref
nullRef = fromHexString $ replicate 40 '0'

firstParentRef :: Commit -> Ref
firstParentRef info = case commitParents info of
    [] -> nullRef
    (r:_) -> r

errorIO :: String -> IO (Maybe a) -> ExceptT String IO a
errorIO str a = do
    mayrez <- liftIO a
    case mayrez of
        Nothing -> throwE str
        Just j  -> return j

decodeUtf8 :: B.ByteString -> T.Text
decodeUtf8 = decodeUtf8With lenientDecode

decodeUtf8Lazy :: L.ByteString -> T.Text
decodeUtf8Lazy = TL.toStrict . LE.decodeUtf8With lenientDecode

fetchDirectoryInfo :: FilePath -> IO [(SubKind, BC.ByteString)]
fetchDirectoryInfo name = do
    files <- getDirectoryContents name
    sequence [(, BC.pack sub) <$> (kindOfExist <$> doesFileExist (name </> sub)) | sub <- files ]
        where kindOfExist True = KindFile
              kindOfExist False = KindDirectory


batchSubTree :: Git -> (T.Text -> Ref -> CommitTreeDiff) -> String -> Ref -> Object
             -> ExceptT String IO CommitTreeDiff
batchSubTree  repository f = aux where
  objAccess = errorIO "" . accessObject repository

  aux name r (ObjBlob _) = return $ f (T.pack name) r
  aux name r (ObjTree (Tree t)) = TreeElement (T.pack name) r <$>
    sequence [objAccess subRef >>= aux (BC.unpack $ toBytes n) subRef
                                | (_, n, subRef) <- t]
  aux _ _ _ = throwE "Wrong git object kind"

