-- | Implement the LCS diff algorithm.
-- As I already implemented it in another language, I'm
-- being lazy and implement it in an imperative way.
module Difftimeline.Diff( -- * Types
                          DiffAction( .. )
                        , DiffCommand( .. )
                        , Index

                          -- * Diff conversion
                        , diffToJson

                          -- * Diff functions
                        , computeDiff
                        , computeTextDiff
                        , computeTextScript
                        , computeStringDiff 
                        ) where

import Prelude
import Data.Monoid( mappend )
import Control.Applicative( (<$>), (<*>) )
import Control.Monad.ST( ST, runST )
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Aeson

type Index = Int

-- | Represent the action to be taken for the diff
data DiffAction = DiffAddition  -- ^ Data which should be inserted
                | DiffDeletion  -- ^ Data which should be removed
                | DiffNeutral   -- ^ Only used to render context
                deriving (Eq, Show)

-- | Hold computed information about the diff
data DiffCommand = DiffCommand !DiffAction  -- ^ Addition or deletion
                               {-# UNPACK #-}!Int         -- ^ Beginning index in the original vector
                               {-# UNPACK #-}!Int         -- ^ Beginning index in the destination vector
                               {-# UNPACK #-}!Int         -- ^ Size of the modification
                 deriving (Eq, Show)


diffToJson :: DiffCommand -> [(T.Text, Value)]
diffToJson (DiffCommand way bego begdest s) =
         [ "way"      .= wayText way
         , "orig_idx" .= bego
         , "dest_idx" .= begdest
         , "size"     .= s]
    where wayText DiffAddition = "+" :: T.Text
          wayText DiffDeletion = "-"
          wayText DiffNeutral  = "="

-- | Merge diff commands which are contiguous of the same direction.
compactCommands :: [DiffCommand] -> [DiffCommand]
compactCommands    [] = []
compactCommands a@[_] = a
compactCommands ( DiffCommand DiffAddition oi di s
                : DiffCommand DiffAddition _ di' s' : xs) 
    | di + s == di' = compactCommands (DiffCommand DiffAddition oi di (s + s') : xs)
compactCommands ( DiffCommand DiffDeletion oi  di s
                : DiffCommand DiffDeletion oi' _ s' : xs) 
    | oi + s == oi' = compactCommands (DiffCommand DiffDeletion oi di (s + s') : xs)
compactCommands (x:xs) = x : compactCommands xs

-- | Context holding data for the actual diff, internal structure
data DiffContext s a = DiffContext
    { forwardSnake  :: MU.STVector s Index
    , backwardSnake :: MU.STVector s Index
    , origData      :: V.Vector a
    , destData      :: V.Vector a
    , arrayBias     :: Int
    }

-- | Compute diff between string, line by line
computeStringDiff :: String -> String -> [DiffCommand]
computeStringDiff a b = computeTextDiff (T.pack a) (T.pack b)

-- | Compute the script to edit difference between two text, line
-- by line.
computeTextDiff :: T.Text -> T.Text -> [DiffCommand]
computeTextDiff orig dest =
    computeDiff (V.fromList $ T.lines orig)
                (V.fromList $ T.lines dest)

addContextInformation :: Int -> Int -> Int -> [DiffCommand] -> [DiffCommand]
addContextInformation 0           _        _        = id
addContextInformation contextSize origSize destSize = inner False
  where inner _  [] = []
        inner True [a@(DiffCommand DiffAddition oi di s)]
            | oi + contextSize > origSize = [a]
            | otherwise =
                [a, DiffCommand DiffNeutral oi (di + s) endSize]
                    where endSize = min (destSize - oi) contextSize

        inner True [a@(DiffCommand DiffDeletion oi di s)]
            | di + s <= origSize = [a]
            | otherwise =
                [a, DiffCommand DiffNeutral (oi + s) (di + s) endSize]
                    where endSize = min (endSize - oi - s) contextSize

        inner True (a@(DiffCommand DiffAddition oi1 di1 s1) :
                    b@(DiffCommand DiffAddition _oi2 di2 _s2) : xs)
                | gapSize == 0 = a : inner True (b:xs)
                | gapSize > 0 && end1 <= contextSize =
                    a : DiffCommand DiffNeutral oi1 end1 gapSize : inner True (b:xs)
                     where end1 = di1 + s1
                           gapSize = end1 - di2

        inner True (a@(DiffCommand DiffDeletion oi1 di1 s1) :
                    b@(DiffCommand DiffDeletion oi2 _di2 _s2) : xs)
                | gapSize == 0 = a : inner True (b:xs)
                | gapSize > 0 && end1 <= contextSize =
                    a : DiffCommand DiffNeutral end1 di1 gapSize : inner True (b:xs)
                     where end1 = oi1 + s1
                           gapSize = end1 - oi2

        inner True (a@(DiffCommand DiffAddition oi1 di1 s1) :
                    b@(DiffCommand DiffDeletion oi2 _di2 _s2) : xs)
                | gapSize == 0 = a : inner True (b:xs)
                | gapSize > 0 && gapSize <= contextSize =
                    a : DiffCommand DiffNeutral oi1 end gapSize : inner True (b:xs)
                     where end = di1 + s1
                           gapSize = oi1 - oi2

        inner True (a@(DiffCommand DiffDeletion oi1 di1 s1) :
                    b@(DiffCommand DiffAddition _oi2 di2 _s2) : xs)
                | gapSize == 0 = a : inner True (b:xs)
                | gapSize > 0 && gapSize <= contextSize =
                    a : DiffCommand DiffNeutral end (di1 + s1) gapSize : inner True (b:xs)
                     where end = oi1 + s1
                           gapSize = di1 - di2

        inner False lst@(DiffCommand DiffAddition oi di _s:_) =
            DiffCommand DiffNeutral oi (di - contextSize) contextSize
                : inner True lst

        inner False lst@(DiffCommand DiffDeletion oi di s:_) =
            DiffCommand DiffNeutral (oi + s) (di - contextSize) contextSize
                : inner True lst

        inner False (a@(DiffCommand DiffNeutral _ _ _):xs) =
            a : inner False xs

        inner True (x@(DiffCommand _ oi di s):xs) =
            x : DiffCommand DiffNeutral (oi + s) (di + s) contextSize : inner False xs


-- | Compute the diff and extract the modification lines from the original text
computeTextScript :: Int -> T.Text -> T.Text -> [(DiffCommand, V.Vector T.Text)]
computeTextScript contextSize orig dest = map extract $ addNeutral diffs
    where origArray = V.fromList $ T.lines orig
          destArray = V.fromList $ T.lines dest
          diffs = computeDiff origArray destArray

          addNeutral =
              addContextInformation contextSize (V.length origArray) (V.length destArray)

          extract c@(DiffCommand DiffAddition _oi  di s) = (c, V.slice di s destArray)
          extract c@(DiffCommand DiffDeletion  oi _di s) = (c, V.slice oi s origArray)
          extract c@(DiffCommand DiffNeutral   oi _di s) = (c, V.slice oi s origArray)

-- | Compute the script to pass from one vector to another using the
-- Longuest common substring algorithm (implemented in the diff command)
computeDiff :: (Eq a) => V.Vector a -> V.Vector a -> [DiffCommand]
computeDiff orig dest = compactCommands $ runST $ do
    let oSize = V.length orig
        dSize = V.length dest

        bias = oSize + dSize
        coeffSize = bias * 2

    forward <- MU.replicate coeffSize (-1)
    backward <- MU.replicate coeffSize (-1)

    let ctxt = DiffContext {
            forwardSnake = forward,
            backwardSnake = backward,
            origData = orig,
            destData = dest,
            arrayBias =  bias
        }
    lcs ctxt (0, oSize) (0, dSize)

{-# INLINE (!!!) #-}
(!!!) :: V.Vector e -> Int -> e
(!!!) = (V.!)


-- | Increase range in the vector
increaseWithin :: (Index, Index) -> (Index, Index) -> MU.STVector s Index -> Index -> Index
               -> ST s (Index, Index)
increaseWithin (mi, ma) (mini, maxi) vec bias nullVal = do
    let (.<-.) v idx val = MU.write v (idx + bias) val
    newMin <- if mi > mini
                then do
                    (vec .<-. (mi - 2)) $ nullVal
                    return $ mi - 1
                else return $ mi + 1
    newMax <- if ma < maxi
                then do
                    (vec .<-. (ma + 2)) $ nullVal
                    return $ ma + 1
                else return $ ma - 1

    return (newMin, newMax)


type Range = (Index, Index)

-- | Compare data along the "current snake" and return the index of 
-- the first different element
findMaxReachingSnake :: (Eq a) => V.Vector a -> V.Vector a -> Range -> Range
                     -> Index
findMaxReachingSnake orig dest (xi, xMax) (yi, yMax) = inner xi yi
    where inner x y | x >= xMax || y >= yMax = x
                    | (orig !!! x) /= (dest !!! y) = x
                    | otherwise = inner (x + 1) (y + 1)

findMaxReachingSnakeBackward :: (Eq a) => V.Vector a -> V.Vector a -> Range -> Range
                             -> Index
findMaxReachingSnakeBackward orig dest (xi, xMin) (yi, yMin) = inner xi yi
    where inner x y | x <= xMin || y <= yMin                  = x
                    | (orig !!! (x - 1)) /= (dest !!! (y - 1)) = x
                    | otherwise = inner (x - 1) (y - 1)

inRange :: (Ord a) => a -> (a, a) -> Bool
inRange x (mini, maxi) = mini <= x && x <= maxi

-- | LCS perform the diff, the findMiddleSnake search the past of lowest
-- resistance, most important calculus function.
findMiddleSnake :: (Eq a) => DiffContext s a -> Index -> Index -> Index -> Index
                -> ST s (Index, Index)
findMiddleSnake ctxt xMin xMax yMin yMax = do
    let forwardMid =  xMin - yMin
        backwardMid = xMax - yMax

        bias = arrayBias ctxt
        (.<-.) v idx val = MU.write v (idx + bias) val
        (.!!!.) v idx = MU.read v (idx + bias)

        validDiagonal = (xMin - yMax, xMax - yMin)

        isOdd = odd $ forwardMid - backwardMid

    ((forwardSnake  ctxt) .<-.  forwardMid) $ xMin
    ((backwardSnake ctxt) .<-. backwardMid) $ xMax

    -- The forwardPass and backwardPass simulate an infinite loop with
    -- a return statement via mutual recursion.
    let forwardPass range backRange = do
            let snakeVec = forwardSnake ctxt
            newRange@(mini, maxi) <- increaseWithin range validDiagonal snakeVec bias (-1)

            let inner d | d < mini = backwardPass newRange backRange
                        | otherwise = do
                            lowerDiag <- snakeVec .!!!. (d - 1)
                            upperDiag <- snakeVec .!!!. (d + 1)

                            let x | lowerDiag >= upperDiag = lowerDiag + 1
                                  | otherwise = upperDiag

                                snake = findMaxReachingSnake (origData ctxt) (destData ctxt)
                                                             (x, xMax) (x - d, yMax) 

                            (snakeVec .<-. d) snake
                            backVal <- backwardSnake ctxt .!!!. d

                            if isOdd && d `inRange` backRange && backVal <= snake
                                then return (x, x - d)
                                else inner $ d - 2
            inner maxi

        backwardPass forwardRange range = do
            let snakeVec = backwardSnake ctxt
            newRange@(mini, maxi) <- increaseWithin range validDiagonal snakeVec bias maxBound

            let inner d | d < mini = forwardPass forwardRange newRange
                        | otherwise = do
                            lowerDiag <- snakeVec .!!!. (d - 1)
                            upperDiag <- snakeVec .!!!. (d + 1)

                            let x | lowerDiag >= upperDiag = upperDiag - 1
                                  | otherwise = lowerDiag
                                snake = findMaxReachingSnakeBackward (origData ctxt) (destData ctxt)
                                                                     (x, xMin) (x - d, yMin)

                            (snakeVec .<-. d) snake
                            forwardVal <- forwardSnake ctxt .!!!. d

                            if not isOdd && d `inRange` forwardRange && snake <= forwardVal
                                then return (x, x - d)
                                else inner $ d - 2

            inner maxi

    forwardPass (forwardMid, forwardMid) (backwardMid, backwardMid)

forwardAvancement :: (Eq a) => DiffContext s a -> Range -> Range -> (Index, Index)
forwardAvancement (DiffContext { origData = oData, destData = dData })
                    (origBegin, origEnd) (destBegin, destEnd) = inner origBegin destBegin
  where inner orig dest | orig >= origEnd = (orig, dest)
                        | dest >= destEnd = (orig, dest)
                        | oData !!! orig /= dData !!! dest = (orig, dest)
                        | otherwise = inner (orig + 1) $ dest + 1

backwardAvancement :: (Eq a) => DiffContext s a -> Range -> Range -> (Index, Index)
backwardAvancement (DiffContext { origData = oData, destData = dData })
                    (origBegin, origEnd) (destBegin, destEnd) = inner origEnd destEnd
  where inner orig dest | origBegin >= orig = (orig, dest)
                        | destBegin >= dest = (orig, dest)
                        | oData !!! (orig -  1) /= dData !!! (dest - 1) = (orig, dest)
                        | otherwise = inner (orig - 1) $ dest - 1

-- | Main recursive function, apply a divide and conquer strategy to find
-- diffs
lcs :: (Eq a) => DiffContext s a -> Range -> Range -> ST s [DiffCommand]
lcs ctxt origRange@(_, origEnd) destRange@(_, destEnd) =
    let (oBeg, dBeg) = forwardAvancement ctxt origRange destRange
        (oEnd, dEnd) = backwardAvancement ctxt (oBeg, origEnd) (dBeg, destEnd)
    in case (oBeg == oEnd, dBeg == dEnd) of
            ( True,     _) ->
                return [DiffCommand DiffAddition oBeg dBeg (dEnd - dBeg)]
            (_    ,  True) ->
                return [DiffCommand DiffDeletion oBeg dBeg (oEnd - oBeg)]
            (False, False) -> do
                (subBeg, subEnd) <- findMiddleSnake ctxt oBeg oEnd dBeg dEnd
                mappend <$> lcs ctxt (oBeg, subBeg) (dBeg, subEnd)
                        <*> lcs ctxt (subBeg, oEnd) (subEnd, dEnd)

