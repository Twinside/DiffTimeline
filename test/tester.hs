{-# LANGUAGE OverloadedStrings #-}
import Difftimeline.Externs
import Difftimeline.GitQuery

import Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson( ToJSON(..), encode )
import Data.Git

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
    Just headRef <- getHead repo
    diffRez <- workingDirectoryChanges repo 3 headRef 
    putStrLn $ show diffRez
