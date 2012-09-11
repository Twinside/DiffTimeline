import qualified Data.Vector as V
import Difftimeline.Diff
import Test.HUnit

diff :: (Eq a) => [a] -> [a] -> [DiffCommand]
diff l r = computeDiff (V.fromList l) (V.fromList r)

tests :: Test
tests = test
    [
      "test1" ~: "(foo 3)" ~: [] ~=? diff [1, 2, 3] [2]
    , "test1" ~: "(foo 3)" ~: [] ~=? diff [1] [2]
    {-, "test1" ~: "(foo 3)" ~: [] ~=? diff [1] [1]-}
    {-,  "bi 4" ~: "(foo 3)" ~: [] ~=? diff [1, 2] [3]-}
    {-, "bi 1" ~: "(foo 3)" ~: [DiffCommand DiffAddition 2 2 0] ~=? diff [1, 2] [1, 2]-}
    {-, "bi 2" ~: "(foo 3)" ~: [DiffCommand DiffDeletion 0 0 2,DiffCommand DiffAddition 2 0 2]-}
                                {-~=? diff [1, 2] [2, 1]-}
    {-, "bi 3" ~: "(foo 3)" ~: [DiffCommand DiffDeletion 0 0 2,DiffCommand DiffAddition 2 0 2]-}
                                {-~=? diff [1, 2] [3, 4]-}
    {-, "bi 4" ~: "(foo 3)" ~: [DiffCommand DiffDeletion 0 0 1,DiffCommand DiffAddition 2 1 1]-}
                                {-~=? diff [1, 2] [3, 4]-}
    {-, "bi 4" ~: "(foo 3)" ~: [] ~=? diff [1]    [3, 4]-}
    {-, "test1" ~: "(foo 3)" ~: [] ~=? diff [1] [1]-}
    ]

main :: IO ()
main =  runTestTT tests >>= print

