-- | This module manipulate diff ranges to help perform a "blame"
-- algorithm.
module Difftimeline.Blame ( -- * Manipulated types
                            BlameRange
                          , BlameRangeSource( .. )

                            -- * Most usable function
                          , shiftDiffs
                          , createBlameRangeForSize

                            -- * Separated manipulation functions.
                          , cutAddedLines
                          , shiftDeletions
                          ) where


import Prelude
import Data.Monoid( Monoid )
import Control.Applicative( Applicative, (<$>), pure )
import Control.Monad.Trans.Writer.Strict( tell, WriterT )

import Debug.Trace
import Text.Printf

--------------------------------------------------
----            Blaming
--------------------------------------------------
data BlameRange = BlameRange {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                             {-# UNPACK #-} !Int
                deriving (Eq, Show)

-- | Create the initial range for the split algorithm.
createBlameRangeForSize :: Int -> [BlameRange]
createBlameRangeForSize size = [BlameRange 0 size 0]

offsetListBy :: Int -> Int -> [BlameRange] -> [BlameRange]
offsetListBy n shift lst = map (flip (offsetBy n) shift) lst

offsetListBy' :: Int -> Int -> [BlameRange] -> [BlameRange]
offsetListBy' n shift lst = map (flip (offsetBy'' n) shift) lst

offsetBy :: Int -> BlameRange -> Int -> BlameRange
offsetBy n (BlameRange bStart bSize bOffset) shift =
    (if (bStart - shift) < 0
       then trace $ printf "- n:%d bStart:%d bSize:%d bOffset:%d shift:%d" n bStart bSize bOffset shift
       else id)
    BlameRange (bStart - shift) bSize (bOffset + shift)

offsetBy'' :: Int -> BlameRange -> Int -> BlameRange
offsetBy'' n (BlameRange bStart bSize bOffset) shift =
    (if (bStart - shift) < 0
       then trace $ printf "=+ n:%d bStart:%d bSize:%d bOffset:%d shift:%d" n bStart bSize bOffset shift
       else id)
    BlameRange (bStart - shift) bSize (bOffset + shift)

offsetBy' :: Int -> BlameRange -> Int -> BlameRange
offsetBy' n (BlameRange bStart bSize bOffset) shift =
    (if (bStart - shift) < 0
       then trace $ printf "+ n:%d bStart:%d bSize:%d bOffset:%d shift:%d" n bStart bSize bOffset shift
       else id)
    BlameRange (bStart - shift) bSize (bOffset + shift)

dropBy :: BlameRange -> Int -> BlameRange
dropBy (BlameRange bStart bSize bOffset) size =
    BlameRange (bStart + size) (bSize - size) bOffset

takeBy :: BlameRange -> Int -> BlameRange
takeBy (BlameRange bStart _ bOffset) size =
    BlameRange bStart size bOffset

shiftDiffs :: ( Monoid (container (BlameRangeSource a))
              , Applicative container
              , Applicative m
              , Monad       m)
           => Int -> a -> [BlameRange] -> [(Int, Int)] -> [(Int, Int)]
           -> WriterT (container (BlameRangeSource a)) m [BlameRange]
shiftDiffs n tag blame rems adds = do
    addRanges <- cutAddedLines n tag adds blame
    trace ("++++\n" ++ unlines (map show addRanges)) $ return $ shiftDeletions n rems addRanges

shiftDeletions :: Int -> [(Int, Int)] -> [BlameRange] -> [BlameRange]
shiftDeletions n remRanges blames = aux 0 blames remRanges
  where aux     _   []  _ = []
        aux shift rest [] = offsetListBy n shift rest
        -- rem before range
        --            #######
        -- #######
        aux shift ranges@((BlameRange bStart _ _):_) ((remBeg, remSize) : remRest)
          | remBeg <= bStart - shift = aux (shift - remSize) ranges remRest

        aux shift (blameElem@(BlameRange bStart bSize _):blameRest) rems@((remBeg, _) : _)
          | bStart + bSize - shift > remBeg = trace (printf "(%2d) - cut by %d" n cutLength) $
              offsetBy n preDelRange shift : aux shift ( blameElem `dropBy` cutLength : blameRest) rems
                where cutLength = remBeg - (bStart - shift)
                      preDelRange = blameElem `takeBy` cutLength 

        aux shift (blameElem:rangeRest) rems =
              offsetBy n blameElem shift : aux shift rangeRest rems


data BlameRangeSource taginfo = BlameRangeSource
    { sourceLineIndex     :: !Int
    , sourceSize          :: !Int
    , sourceOriginalIndex :: !Int
    , sourceTag           :: taginfo
    }
    deriving (Eq, Show)

instance (Eq a) => Ord (BlameRangeSource a) where
    compare (BlameRangeSource { sourceLineIndex = i1 })
            (BlameRangeSource { sourceLineIndex = i2 }) = compare i1 i2

cutAddedLines :: ( Monoid (container (BlameRangeSource tag))
                 , Applicative container
                 , Applicative m
                 , Monad       m )
              => Int
              -> tag
              -> [(Int, Int)] -- ^ Add begin, add size
              -> [BlameRange]
              -> WriterT (container (BlameRangeSource tag)) m [BlameRange]
cutAddedLines n tag addRanges blameRanges = aux 0 blameRanges addRanges
  where aux     _   []  _ = pure []
        aux shift rest [] = pure $ offsetListBy' n shift rest
        aux shift ((BlameRange _ 0 _):rangeRest) adds = aux shift rangeRest adds
        aux shift           ranges ((_, 0) : addRest) = aux shift ranges addRest
        aux shift
            ranges@(blameElem@(BlameRange bStart bSize bOffset):rangeRest)
              adds@((addBeg, addSize) : addRest)
          -- range before add
          -- #######
          --            #######
          | bStart + bSize <= addBeg = trace (printf "(%2d) + RANGE before ADD (%s)" n (show blameElem)) $
              (offsetBy' n blameElem shift :) <$> aux shift rangeRest adds

          -- add before range
          --            #######
          -- #######
          | addBeg + addSize <= bStart = trace (printf "(%2d) + ADD before RANGE" n) $
                aux (shift + addSize) ranges addRest

          -- intersection starting with add
          --    #####...
          -- ########...
          | addBeg < bStart = trace (printf "(%2d) + starting add" n) $
              let addBeforeSize = bStart - addBeg
              in aux (shift + addBeforeSize) ranges
                     ((addBeg + addBeforeSize, addSize - addBeforeSize):addRest)

          -- ########...
          --    #####...
          | bStart < addBeg = trace (printf "(%2d) + starting range" n) $
              let beforeRemainingSize = addBeg - bStart
              in (offsetBy' n (blameElem `takeBy` beforeRemainingSize) shift:) <$>
                        aux shift (blameElem `dropBy` beforeRemainingSize : rangeRest) adds

          -- ######
          -- ########...
          | addBeg == bStart && bStart + bSize <= addBeg + addSize = trace (printf "(%2d) + == nominal" n) $ do
              tell . pure $ BlameRangeSource (bStart + bOffset) bSize bStart tag
              aux (shift + bSize)  rangeRest $ (addBeg + bSize, addSize - bSize) : addRest

          -- ######
          -- ########...
          | addBeg == bStart && bStart + bSize > addBeg + addSize = trace (printf "(%2d) + == range overflow" n) $
              aux shift ( blameElem `takeBy` addSize
                        : blameElem `dropBy` addSize : rangeRest) adds

        aux _ _ _ = error "Hmm... no"

