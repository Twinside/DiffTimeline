{-# LANGUAGE OverloadedStrings #-}
import Difftimeline.Externs
import Difftimeline.GitQuery

import Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson( ToJSON(..), encode )
import Data.Git( fromHexString )

serializeTest :: ToJSON a => a -> IO ()
serializeTest =
    putStrLn . T.unpack . E.decodeUtf8 . B.concat . BL.toChunks . encode . toJSON

nullRef = fromHexString "f97e22103a354dffc1fe3275e27312a0f6e47008"
commitTreeDiff = AddElement "bah" nullRef

commitTreeDiffs =
	[commitTreeDiff ]

main :: IO ()
main = do
	putStrLn $ show nullRef
	serializeTest $ ErrorReturn "anError"
	serializeTest commitTreeDiff
	serializeTest commitTreeDiffs
