
import Data.Monoid
import Difftimeline.Blame
import Test.HUnit
import Control.Monad.Trans.Writer.Strict

testVal :: [BlameRange] -> [(Int, Int)] -> ([BlameRange], [(Int, Int, ())])
testVal range split = runWriter $ cutAddedLines () range split

parseRanges :: String -> [(Int, Int)]
parseRanges = aux 0 0
  where aux _ 0 [] = []
        aux i n [] = [(i - n, n)]
        aux i 0 (' ':xs) = aux (i + 1) 0 xs
        aux i n (' ':xs) = (i - n, n) : aux (i + 1) 0 xs
        aux i n ('!':xs) = (i - n, n) : aux (i + 1) 1 xs
        aux i n ( _ :xs) = aux (i + 1) (n + 1) xs
            
rangeTest :: String -> String -> ([BlameRange], [(Int, Int, ())])
rangeTest blames adds = testVal parsedRanges parsedAdds
  where parsedRanges = [BlameRange b s 0 | (b, s) <- parseRanges blames]
        parsedAdds = parseRanges adds

resultTest :: String -> String -> ([BlameRange], [(Int, Int, ())])
resultTest blames adds = (parsedRanges, parsedAdds)
    where parsedRanges = [BlameRange b s 0 | (b, s) <- parseRanges blames] 
          parsedAdds = [(b, s, ()) | (b, s) <- parseRanges adds]

splitTest :: [Test]
splitTest =
    [   resultTest "###  #####"
                   "   ##     "
     ~=? rangeTest "##########"
                   "   ##     "

    ,   resultTest "####      "
                   "          "
     ~=? rangeTest "####      "
                   "      ####"

    ,   resultTest "      ####"
                   "          "
     ~=? rangeTest "      ####"
                   "####      "
    ]

parseTest :: [Test]
parseTest = 
    [ [] ~=? parseRanges "              "
    , [] ~=? parseRanges ""
    , [(0,1)] ~=? parseRanges "#"
    , [(0,1)] ~=? parseRanges "#        "
    , [(5,1)] ~=? parseRanges "     #"
    , [(0,3)] ~=? parseRanges "###      "
    , [(2,3)] ~=? parseRanges "  ###      "
    , [(0,1), (2,3)] ~=? parseRanges "# ###      "
    , [(1,1), (3,3)] ~=? parseRanges " # ###      "
    , [(0,1), (1,4)] ~=? parseRanges "#!###"
    , [(0,1), (1,1)] ~=? parseRanges "#!"
    ]

main :: IO ()
main =  runTestTT (test $ parseTest ++ splitTest) >>= (putStrLn . show)

