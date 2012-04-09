import Control.Applicative( (<$>) )
import Difftimeline.Diff
import qualified Data.Text as T

main :: IO ()
main = do
    f1 <- T.pack <$> readFile "test/file1_orig.txt"
    f2 <- T.pack <$> readFile "test/file1_dest.txt"
    mapM_ (putStrLn . show) $ computeTextScript f1 f2

