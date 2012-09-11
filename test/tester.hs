{-# LANGUAGE OverloadedStrings #-}
import Difftimeline.Externs
import Difftimeline.GitQuery

import qualified Data.Vector as V
import Control.Monad( forM_ )
import Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson( ToJSON(..), encode )
import Data.Git

import Text.Printf
import Difftimeline.Diff

serializeTest :: ToJSON a => a -> IO ()
serializeTest =
    putStrLn . T.unpack . E.decodeUtf8 . B.concat . BL.toChunks . encode . toJSON

nullRef = fromHexString "f97e22103a354dffc1fe3275e27312a0f6e47008"
commitTreeDiff = AddElement "bah" nullRef

commitTreeDiffs =
    [commitTreeDiff ]

main :: IO ()
main = do
    {-putStrLn $ show nullRef-}
    {-serializeTest $ ErrorReturn "anError"-}
    {-serializeTest commitTreeDiff-}
    {-serializeTest commitTreeDiffs-}
    {-putStrLn . show $-}
               {-computeTextScript 3 (T.pack "Hello: Hello\nab")-}
                                   {-(T.pack "Hello: Hello\nab")-}

    {-putStrLn . show $-}
               {-computeTextScript 3 (T.pack "Hello: Hello\n")-}
                                   {-(T.pack "Hello: Hello\r\n")-}

    repo <- openRepo "./.git"
    {-branches <- brancheslist repo-}
    {-Just headRef <- getHead repo-}
    {-diffRez <- workingDirectoryChanges repo 3 headRef -}
    {-putStrLn $ show branches-}
    nfo <- compareBranches repo 3 "1896b4e2601c0c857ac19b07673029bbb6c69f93" "f467ceb8b3a0bf85427636e63903ac81e3dc3de5"
    print nfo

    {-Right rez <- blameFile repo (show headRef) "test/blame_test.hs"-}

    {-let fileLines = V.fromList . T.lines $ blameData rez-}
    {-forM_ (V.toList $ blameRanges rez) -}
        {-$ \(BlameRangeSource beg size oline ref) -> do-}
        {-forM_ [beg .. beg + size - 1] $ \idx ->-}
            {-putStrLn $ printf "%s) %3d %3d %s" (take 6 $ show ref) (oline + idx - beg + 1)-}
                                        {-(idx + 1) (show $ fileLines V.! idx)-}

