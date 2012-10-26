module Difftimeline.GitIgnore ( IgnoredSet
                              , loadIgnoreFile
                              , isPathIgnored
                              , emptyIgnoreSet
                              ) where
import Control.Applicative( (<$>) )
import Data.Either( Rights )
import Data.Maybe( isJust )
import qualified Data.ByteString as B
import qualified Text.Regex.PCRE.Light as R

newtype IgnoredSet = IgnoredSet [R.Regex]

convertToRegexp :: Char -> String
convertToRegexp '.' = "\\."
convertToRegexp '*' = ".*"
convertToRegexp '^' = "\\^"
convertToRegexp '(' = "\\("
convertToRegexp ')' = "\\)"
convertToRegexp '{' = "\\{"
convertToRegexp '}' = "\\}"
convertToRegexp '?' = "."
convertToRegexp a = [a]

compileGlob :: String -> Either String R.Regex
compileGlob = R.compileM . B.pack . globToRegexp
    where globToRegexp = concatMap convertToRegexp

loadIgnoreFile :: FilePath -> IO IgnoredSet
loadIgnoreFile path =
     IgnoredSet . rights . compileGlob . lines <$> readFile path

isPathIgnored :: IgnoredSet -> FilePath -> Bool
isPathIgnored (IgnoredSet lst) path = any isMatching lst
    where isMatching r = isJust $ match r path []

emptyIgnoreSet :: IgnoredSet
emptyIgnoreSet = IgnoredSet []

