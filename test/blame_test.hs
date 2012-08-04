
import Data.Monoid
import Difftimeline.Blame
import Test.HUnit
import Control.Monad.Trans.Writer.Strict

testVal :: [BlameRange] -> [(Int, Int)] -> ([BlameRange], [(Int, Int, ())])
testVal range split = runWriter $ cutAddedLines () range split

parseRanges :: String -> [(Int, Int, Int)]
parseRanges = aux 0 0
  where aux _ 0 [] = []
        aux i n [] = [(i - n, n, 0)]
        aux i 0 (' ':xs) = aux (i + 1) 0 xs
        aux i n (' ':xs) = (i - n, n, 0) : aux (i + 1) 0 xs
        aux i n ('!':xs) = (i - n, n + 1, 0) : aux (i + 1) 0 xs
        aux i n ( d :xs) 
            | '0' <= d && d <= '9' = (i - n, n + 1, read [d]) : aux (i + 1) 0 xs
        aux i n ( _ :xs) = aux (i + 1) (n + 1) xs
            
rangeTest :: String -> String -> ([BlameRange], [(Int, Int, ())])
rangeTest blames adds = testVal parsedRanges parsedAdds
  where parsedRanges = [BlameRange b s offset | (b, s, offset) <- parseRanges blames]
        parsedAdds = [(a,b) | (a, b, _) <- parseRanges adds]

resultTest :: String -> String -> ([BlameRange], [(Int, Int, ())])
resultTest blames adds = (parsedRanges, parsedAdds)
    where parsedRanges = [BlameRange b s offset | (b, s, offset) <- parseRanges blames] 
          parsedAdds = [(b, s, ()) | (b, s, _) <- parseRanges adds]

splitTest :: [Test]
splitTest =
    [ "Basic middle split" ~:
        resultTest "##!####2  "
                   "   ##     "
     ~=? rangeTest "##########"
                   "   ++     "

    , "Same range" ~:
        resultTest "          "
                   "##########"
     ~=? rangeTest "##########"
                   "++++++++++"

    ,   "At the end" ~:
        resultTest "          "
                   "  ########"
     ~=? rangeTest "  ########"
                   "++++++++++"

    ,  "Two part split" ~:
        resultTest "          "
                   "  ## #####"
     ~=? rangeTest "  ## #####"
                   "++++++++++"

    ,  "Two part split with add rest" ~:
        resultTest "          "
                   "  ## ###  "
     ~=? rangeTest "  ## ###  "
                   "++++++++++"

    ,  "Two part split with rest" ~:
        resultTest "##7       "
                   "  ## ##   "
     ~=? rangeTest "  ## #####"
                   "+++++++   "

    ,  "Two part split with rest shifted" ~:
        resultTest " ##7       "
                   "   ## ##   "
     ~=? rangeTest "   ## #####"
                   " +++++++   "

    , "Cut add in middle" ~:
        resultTest "          "
                   "  ######  "
     ~=? rangeTest "  ######  "
                   "++++++++++"

    , "Range starting before" ~:
        resultTest "###    "
                   "   ####"
     ~=? rangeTest "#######"
                   "   ++++"

    , "Range starting before with overflow" ~:
        resultTest "###    "
                   "   ####"
     ~=? rangeTest "#######"
                   "   ++++++"

    , "Range starting before with follow" ~:
        resultTest "### #4 "
                   "   ####   "
     ~=? rangeTest "####### ##"
                   "   ++++   "

    , "Range starting before with overflow and follow" ~:
        resultTest "##!#5     "
                   "   ####   "
     ~=? rangeTest "####### ##"
                   "   +++++  "

    , "Range before" ~:
        resultTest "####      "
                   "          "
     ~=? rangeTest "####      "
                   "      ++++"

    , "Range before just" ~:
        resultTest "####    "
                   "        "
     ~=? rangeTest "####    "
                   "    ++++"

    , "Range before follow" ~:
        resultTest "####   #4    "
                   "             "
     ~=? rangeTest "####       ##"
                   "      ++++   "

    , "Range after" ~:
        resultTest "  ###4    "
                   "          "
     ~=? rangeTest "      ####"
                   "++++      "

    , "Range after just" ~:
        resultTest "###4    "
                   "          "
     ~=? rangeTest "    ####"
                   "++++      "
    ]

parseTest :: [Test]
parseTest = 
    [ [] ~=? parseRanges "              "
    , [] ~=? parseRanges ""
    , [(0,1,0)] ~=? parseRanges "#"
    , [(0,1,0)] ~=? parseRanges "#        "
    , [(5,1,0)] ~=? parseRanges "     #"
    , [(0,3,0)] ~=? parseRanges "###      "
    , [(2,3,0)] ~=? parseRanges "  ###      "
    , [(0,1,0), (2,3,0)] ~=? parseRanges "# ###      "
    , [(1,1,0), (3,3,0)] ~=? parseRanges " # ###      "
    , [(0,2,0), (2,3,0)] ~=? parseRanges "#!###"
    , [(0,2,0)] ~=? parseRanges "#!"
    , [(0, 4, 3)] ~=? parseRanges "###3"
    , [(0, 4, 3), (4, 2, 0)] ~=? parseRanges "###3##"
    ]

main :: IO ()
main =  runTestTT (test $ parseTest ++ splitTest) >>= (putStrLn . show)

