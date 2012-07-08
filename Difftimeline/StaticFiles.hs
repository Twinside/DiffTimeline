module Difftimeline.StaticFiles where

import qualified Data.ByteString as B
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding( decodeUtf8 )

basePageEmbedded :: T.Text
basePageEmbedded = decodeUtf8 $(embedFile "static-content/base_page.html")

icanHazEmbedded :: B.ByteString
icanHazEmbedded = $(embedFile "static-content/ICanHaz.min.js")

diffTimelineCssEmbedded :: B.ByteString
diffTimelineCssEmbedded = $(embedFile "static-content/difftimeline.css")

diffTimlineJsEmbedded :: B.ByteString
diffTimlineJsEmbedded = $(embedFile "static-content/difftimeline.js")

faviconEmbed :: B.ByteString
faviconEmbed = $(embedFile "static-content/favicon.ico")

jqueryEmbedded :: B.ByteString
jqueryEmbedded = $(embedFile "static-content/jquery-1.7.2.min.js")

jqueryUiEmbedded :: B.ByteString
jqueryUiEmbedded = $(embedFile "static-content/jquery-ui-1.8.21.custom.min.js")

screenCssEmbedded :: B.ByteString
screenCssEmbedded = $(embedFile "static-content/screen.css")

syntaxhighlihgtCss :: B.ByteString
syntaxhighlihgtCss = $(embedFile "static-content/syntax-highlight.css")

tinySyntaxHighlightJs :: B.ByteString
tinySyntaxHighlightJs = $(embedFile "static-content/tinysyntaxhighlighter.js")

