module Difftimeline.StaticFiles where

import Prelude
import qualified Data.ByteString as B
import Data.FileEmbed
import qualified Data.Text as T
import Data.Text.Encoding( decodeUtf8 )

basePageEmbedded :: (FilePath, T.Text)
basePageEmbedded = ("base_page.html", decodeUtf8 $(embedFile "static-content/base_page.html"))

icanHazEmbedded :: (FilePath, B.ByteString)
icanHazEmbedded = ("ICanHaz.min.js", $(embedFile "static-content/ICanHaz.min.js"))

diffTimelineCssEmbedded :: (FilePath, B.ByteString)
diffTimelineCssEmbedded = ("difftimeline.css", $(embedFile "static-content/difftimeline.css"))

diffTimlineJsEmbedded :: (FilePath, B.ByteString)
diffTimlineJsEmbedded = ("difftimeline.js", $(embedFile "static-content/difftimeline.js"))

faviconEmbed :: (FilePath, B.ByteString)
faviconEmbed = ("favicon.ico", $(embedFile "static-content/favicon.ico"))

jqueryEmbedded :: (FilePath, B.ByteString)
jqueryEmbedded = ("jquery-1.7.2.min.js", $(embedFile "static-content/jquery-1.7.2.min.js"))

jqueryUiEmbedded :: (FilePath, B.ByteString)
jqueryUiEmbedded = ("jquery-ui-1.8.21.custom.min.js", $(embedFile "static-content/jquery-ui-1.8.21.custom.min.js"))

screenCssEmbedded :: (FilePath, B.ByteString)
screenCssEmbedded = ("screen.css", $(embedFile "static-content/screen.css"))

syntaxhighlihgtCss :: (FilePath, B.ByteString)
syntaxhighlihgtCss = ("syntax-highlight.css", $(embedFile "static-content/syntax-highlight.css"))

tinySyntaxHighlightJs :: (FilePath, B.ByteString)
tinySyntaxHighlightJs = ("tinysyntaxhighlighter.js", $(embedFile "static-content/tinysyntaxhighlighter.js"))

