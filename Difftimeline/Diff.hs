{-# LANGUAGE OverloadedStrings #-}
-- | Implement the LCS diff algorithm.
-- As I already implemented it in another language, I'm
-- being lazy and implement it in an imperative way.
module Difftimeline.Diff( -- * Types
                          DiffAction( .. )
                        , DiffCommand( .. )
                        , SubModification( .. )
                        , Index

                          -- * Diff functions
                        , computeDiff
                        , computeTextDiff
                        , computeTextScript
                        , computeStringDiff
                        ) where

import Prelude
import Data.Monoid( mappend )
import Control.Applicative( (<$>), (<*>), pure )
import Control.Monad.ST( ST, runST )
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as MU

type Index = Int

-- | Represent the action to be taken for the diff
data DiffAction = DiffAddition  -- ^ Data which should be inserted
                | DiffDeletion  -- ^ Data which should be removed
                | DiffNeutral   -- ^ Only used to render context
                deriving (Eq, Show)

-- | Basic modification on a line, used for refined highlighting.
data SubModification = SubModification {-# UNPACK #-}!Int  -- ^ Begin
                                       {-# UNPACK #-}!Int  -- ^ End
        deriving (Eq, Show)

data RawDiffCommand = DiffAdd {-# UNPACK #-}!Int         -- ^ Beginning index in the original vector
                              {-# UNPACK #-}!Int         -- ^ Beginning index in the destination vector
                              {-# UNPACK #-}!Int         -- ^ Size of the modification
                    | DiffDel {-# UNPACK #-}!Int         -- ^ Beginning index in the original vector
                              {-# UNPACK #-}!Int         -- ^ Beginning index in the destination vector
                              {-# UNPACK #-}!Int         -- ^ Size of the modification
                    deriving (Eq, Show)

-- | Hold computed information about the diff
data DiffCommand = DiffCommand !DiffAction  -- ^ Addition or deletion
                               {-# UNPACK #-}!Int         -- ^ Beginning index in the original vector
                               {-# UNPACK #-}!Int         -- ^ Beginning index in the destination vector
                               {-# UNPACK #-}!Int         -- ^ Size of the modification
                 | DiffRefined !DiffAction
                               {-# UNPACK #-}!Int         -- ^ Beginning index in the original vector
                               {-# UNPACK #-}!Int         -- ^ Beginning index in the destination vector
                               {-# UNPACK #-}!Int         -- ^ Size of the modification
                               ![[SubModification]]       -- ^ Refined diff
                 deriving (Eq, Show)

-- | Merge diff commands which are contiguous of the same direction.
compactCommands :: [RawDiffCommand] -> [RawDiffCommand]
compactCommands    [] = []
compactCommands a@[_] = a
compactCommands ( DiffAdd oi di s
                : DiffAdd _ di' s' : xs)
    | di + s == di' = compactCommands (DiffAdd oi di (s + s') : xs)
compactCommands ( DiffDel oi  di s
                : DiffDel oi' _ s' : xs)
    | oi + s == oi' = compactCommands (DiffDel oi di (s + s') : xs)
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

refineMonolineDiff :: (Eq b) => (a -> V.Vector b) -> V.Vector a -> V.Vector a -> [DiffCommand]
                   -> [DiffCommand]
refineMonolineDiff converter orig dest = inner
    where inner [] = []
          inner [a] = [a]
          inner ( DiffCommand DiffDeletion oi1 di1 s1
                : DiffCommand DiffAddition oi2 di2 s2 : xs)
                | s1 == s2 && di1 == di2 =
                    DiffRefined DiffDeletion oi1 di1 s1 dels : 
                    DiffRefined DiffAddition oi2 di2 s2 adds : inner xs
                      where origs = V.map converter $ V.slice oi1 s1 orig
                            dests = V.map converter $ V.slice di2 s2 dest

                            subDiff = [computeDiff o d
                                            | (o,d) <- zip (V.toList origs) (V.toList dests)]

                            dels = [ [SubModification oi $ oi + s
                                                | DiffCommand DiffDeletion oi _ s <- line]
                                            | line <- subDiff ]

                            adds = [ [SubModification di $ di + s
                                                | DiffCommand DiffAddition _ di s <- line]
                                            | line <- subDiff ]

          inner (x:xs) = x : inner xs

toFullCommand :: RawDiffCommand -> DiffCommand
toFullCommand (DiffDel oi di s) = DiffCommand DiffDeletion oi di s
toFullCommand (DiffAdd oi di s) = DiffCommand DiffAddition oi di s

addContextInformation :: Int -> Int -> Int -> [RawDiffCommand] -> [DiffCommand]
addContextInformation 0           _                _ = map toFullCommand
addContextInformation contextSize origSize _destSize = inner False
  where maxGap = 2 * contextSize + 1

        inner _  [] = []
        inner True [a@(DiffAdd oi di s)]
            | origSize - (oi + s) <= 0 = [toFullCommand a]
            | otherwise =
                [toFullCommand a, DiffCommand DiffNeutral oi (di + s) endSize]
                    where endSize = min (origSize - oi) contextSize

        inner True [a@(DiffDel oi di s)]
            | origSize - (oi + s) <= 0 = [toFullCommand a]
            | otherwise =
                [toFullCommand a, DiffCommand DiffNeutral (oi + s) di endSize]
                    where endSize = min (origSize - oi - s - 1) contextSize

        inner True (a@(DiffAdd oi1 di1 s1) :
                    b@(DiffAdd _oi2 di2 _s2) : xs)
                | gapSize == 0 = toFullCommand a : inner True (b:xs)
                | 0 < gapSize && gapSize <= maxGap =
                    toFullCommand a : DiffCommand DiffNeutral oi1 end1 gapSize : inner True (b:xs)
                     where end1 = di1 + s1
                           gapSize = max 0 $ di2 - end1

        inner True (a@(DiffDel oi1 di1 s1) :
                    b@(DiffDel oi2 _di2 _s2) : xs)
                | gapSize == 0 = toFullCommand a : inner True (b:xs)
                | 0 < gapSize && gapSize <= maxGap =
                    toFullCommand a : DiffCommand DiffNeutral end1 di1 gapSize : inner True (b:xs)
                     where end1 = oi1 + s1
                           gapSize = max 0 $ oi2 - end1

        inner True (a@(DiffAdd oi1 di1 s1) :
                    b@(DiffDel oi2 _di2 _s2) : xs)
                | gapSize == 0 = toFullCommand a : inner True (b:xs)
                | 0 < gapSize && gapSize <= maxGap =
                    toFullCommand a : DiffCommand DiffNeutral oi1 end gapSize : inner True (b:xs)
                     where end = di1 + s1
                           gapSize = max 0 $ oi2 - oi1

        inner True (a@(DiffDel oi1 di1 s1) :
                    b@(DiffAdd oi2 _di2 _s2) : xs)
                | gapSize == 0 = toFullCommand a : inner True (b:xs)
                | 0 < gapSize && gapSize <= maxGap =
                    toFullCommand a : DiffCommand DiffNeutral end (di1 + s1) gapSize : inner True (b:xs)
                     where end = oi1 + s1
                           gapSize = oi2 - end

        inner False lst@(DiffAdd oi di _s:_) =
            DiffCommand DiffNeutral beg dbeg (min contextSize oi)
                : inner True lst
                where beg = max 0 $ oi - contextSize
                      dbeg = max 0 $ di - contextSize

        inner False lst@(DiffDel oi di _s:_) =
            DiffCommand DiffNeutral beg dbeg (min contextSize oi)
                : inner True lst
                where beg = max 0 $ oi - contextSize
                      dbeg = max 0 $ di - contextSize

        inner True (x@(DiffAdd oi di s):xs) =
            toFullCommand x : DiffCommand DiffNeutral oi (di + s) contextSize
                            : inner False xs

        inner True (x@(DiffDel oi di s):xs) =
            toFullCommand x : DiffCommand DiffNeutral (oi + s) (di + s) contextSize
                            : inner False xs


-- | Compute the diff and extract the modification lines from the original text
computeTextScript :: Int -> T.Text -> T.Text -> [(DiffCommand, V.Vector T.Text)]
computeTextScript contextSize orig dest = map extract
                                        . textRefiner origArray destArray
                                        $ addNeutral diffs
    where origArray = V.fromList $ T.lines orig
          destArray = V.fromList $ T.lines dest
          diffs = computeDiffRaw origArray destArray

          addNeutral = addContextInformation contextSize (V.length origArray)
                                                         (V.length destArray)

          slicer idx size array = V.slice idx size array

          extract c@(DiffCommand DiffAddition _oi  di s)   = (c, slicer di s destArray)
          extract c@(DiffRefined DiffAddition _oi  di s _) = (c, slicer di s destArray)
          extract c@(DiffCommand DiffDeletion  oi _di s)   = (c, slicer oi s origArray)
          extract c@(DiffRefined DiffDeletion  oi _di s _) = (c, slicer oi s origArray)
          extract c@(DiffCommand DiffNeutral   oi _di s)   = (c, slicer oi s origArray)
          extract c@(DiffRefined DiffNeutral   oi _di s _) = (c, slicer oi s origArray)

textRefiner :: V.Vector T.Text -> V.Vector T.Text -> [DiffCommand]
            -> [DiffCommand]
textRefiner = refineMonolineDiff (V.fromList . T.unpack)

-- | Compute the script to pass from one vector to another using the
-- Longuest common substring algorithm (implemented in the diff command)
computeDiff :: (Eq a) => V.Vector a -> V.Vector a -> [DiffCommand]
computeDiff orig dest = toFullCommand <$> computeDiffRaw orig dest

computeDiffRaw :: (Eq a) => V.Vector a -> V.Vector a -> [RawDiffCommand]
computeDiffRaw orig dest = compactCommands $ runST $ do
    let oSize = V.length orig
        dSize = V.length dest

        bias = oSize + dSize + 1
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
lcs :: (Eq a) => DiffContext s a -> Range -> Range -> ST s [RawDiffCommand]
lcs ctxt origRange@(_, origEnd) destRange@(_, destEnd) = rez
  where (oBeg, dBeg) = forwardAvancement ctxt origRange destRange
        (oEnd, dEnd) = backwardAvancement ctxt (oBeg, origEnd) (dBeg, destEnd)

        rez | oBeg == oEnd = pure $ if dEnd == dBeg then [] else [DiffAdd oBeg dBeg (dEnd - dBeg)]
            | dBeg == dEnd = pure $ if oEnd == oBeg then [] else [DiffDel oBeg dBeg (oEnd - oBeg)]
            | otherwise =  do
                (subBeg, subEnd) <- findMiddleSnake ctxt oBeg oEnd dBeg dEnd
                mappend <$> lcs ctxt (oBeg, subBeg) (dBeg, subEnd)
                        <*> lcs ctxt (subBeg, oEnd) (subEnd, dEnd)

