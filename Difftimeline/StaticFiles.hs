module Difftimeline.StaticFiles where

import Prelude
import qualified Data.ByteString as B
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding( decodeUtf8 )

thirdParty :: (FilePath, T.Text)
thirdParty = ("third_party.js", decodeUtf8 $(embedFile "static-content/third_party.js"))

composedEmbedded :: (FilePath, T.Text)
composedEmbedded = ("composed.js", decodeUtf8 $(embedFile "composed.js"))

composedMapEmbedded :: (FilePath, T.Text)
composedMapEmbedded = ("composed.js.map", decodeUtf8 $(embedFile "composed.js.map"))

basePageEmbedded :: (FilePath, T.Text)
basePageEmbedded = ("base_page.html", decodeUtf8 $(embedFile "static-content/base_page.html"))

diffTimelineCssEmbedded :: (FilePath, B.ByteString)
diffTimelineCssEmbedded = ("difftimeline.css", $(embedFile "static-content/difftimeline.css"))

diffTimlineJsEmbedded :: (FilePath, B.ByteString)
diffTimlineJsEmbedded = ("difftimeline.js", $(embedFile "static-content/difftimeline.js"))

faviconEmbed :: (FilePath, B.ByteString)
faviconEmbed = ("favicon.ico", $(embedFile "static-content/favicon.ico"))

syntaxhighlihgtCss :: (FilePath, B.ByteString)
syntaxhighlihgtCss = ("syntax-highlight.css", $(embedFile "static-content/syntax-highlight.css"))

