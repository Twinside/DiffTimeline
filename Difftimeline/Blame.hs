-- | This module manipulate diff ranges to help perform a "blame"
-- algorithm.
module Difftimeline.Blame ( -- * Manipulated types
                            BlameRange

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
import qualified Data.Vector as V

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

shiftDiffs :: ( Monoid (container (Int, Int, a))
              , Applicative container
              , Applicative m
              , Monad       m)
           => Int -> a -> [BlameRange] -> [(Int, Int)] -> [(Int, Int)]
           -> WriterT (container (Int, Int, a)) m [BlameRange]
shiftDiffs n tag blame rems adds = do
    addRanges <- cutAddedLines n tag adds blame
    trace ("++++\n" ++ unlines (map show addRanges)) $ return $ shiftDeletions n rems addRanges

shiftDeletions :: Int -> [(Int, Int)] -> [BlameRange] -> [BlameRange]
shiftDeletions n remRanges blames = aux 0 blames remRanges
  where 
        aux     _   []  _ = []
        aux shift rest [] = offsetListBy n shift rest
        -- rem before range
        --            #######
        -- #######
        aux shift ranges@((BlameRange bStart _ _):_) ((remBeg, remSize) : remRest)
          | remBeg <= bStart = aux (shift - remSize) ranges remRest

        aux shift (blameElem@(BlameRange bStart bSize _):blameRest) rems@((remBeg, _) : _)
          |  bStart + bSize > remBeg =
              offsetBy n preDelRange shift : aux shift ( blameElem `dropBy` cutLength : blameRest) rems
              {-preDelRange `offsetBy` shift : aux shift ( blameElem `dropBy` cutLength : blameRest) rems-}
                where cutLength = remBeg - bStart
                      preDelRange = blameElem `takeBy` cutLength 

        aux shift (blameElem:rangeRest) rems =
              offsetBy n blameElem shift : aux shift rangeRest rems
              {-blameElem `offsetBy` shift : aux shift rangeRest rems-}



cutAddedLines :: ( Monoid (container (Int, Int, a))
                 , Applicative container
                 , Applicative m
                 , Monad       m)
              => Int
              -> a
              -> [(Int, Int)] -- ^ Add begin, add size
              -> [BlameRange]
              -> WriterT (container (Int, Int, a)) m [BlameRange]
cutAddedLines n tag addRanges blameRanges = aux 0 blameRanges addRanges
  where aux     _   []  _ = pure []
        aux shift rest [] = pure $ offsetListBy' n shift rest
        aux shift
            ranges@(blameElem@(BlameRange bStart bSize bOffset):rangeRest)
              adds@((addBeg, addSize) : addRest)
          -- range before add
          -- #######
          --            #######
          | bStart + bSize <= addBeg = trace (printf "(%2d) + RANGE before ADD (%s)" n (show blameElem)) $
              (offsetBy' n blameElem shift :) <$> aux shift rangeRest adds
              {-(blameElem `offsetBy` shift :) <$> aux shift rangeRest adds-}

          -- add before range
          --            #######
          -- #######
          | addBeg + addSize <= bStart = trace (printf "(%2d) + ADD before RANGE" n) $
                aux (shift + addSize) ranges addRest

          -- intersection starting with add
          -- ??????######???????
          -- ############
          | addBeg <= bStart && addBeg + addSize <= bStart + bSize = 
            trace (printf "(%2d) + Inter start ADD >=" n) $
                                                    do
              let addEnd = addBeg + addSize
                  toldSize = addEnd - bStart
                  nextRange
                    | toldSize < bSize = blameElem `dropBy` toldSize : rangeRest
                    | otherwise = rangeRest
                  shiftSize = bStart - addBeg
              tell $ pure (bStart + bOffset, toldSize, tag)
              aux (shift + toldSize + shiftSize) 
                        nextRange addRest

          -- intersection starting with add
          -- ??????####
          -- ###############
          | addBeg <= bStart && addBeg + addSize > bStart + bSize = 

            trace (printf "(%2d) + Inter start ADD >" n) $
                                                        do
              let beforeLeftSize = bStart - addBeg
                  remainingAddSize = addSize - bSize - beforeLeftSize
              tell $ pure (bStart + bOffset, bSize, tag)
              aux (shift + beforeLeftSize + bSize) rangeRest
                  $ (bStart + bSize, remainingAddSize) : addRest

          -- ###########????
          --    ############
          | bStart < addBeg && bStart + bSize <= addBeg + addSize = 
            trace (printf "(%2d) + Inter start RANGE" n) $
                                    do
              let beforeRemainingSize = addBeg - bStart
                  cutedAddSize = min (addBeg + addSize) (bStart + bSize) - addBeg
                  addNextRange
                    | cutedAddSize == addSize = addRest
                    | otherwise = (addBeg + cutedAddSize, addSize - cutedAddSize) : addRest
              tell $ pure (addBeg + bOffset, cutedAddSize , tag)
              (offsetBy' n (BlameRange bStart beforeRemainingSize bOffset) shift:) <$>
                        aux (shift + cutedAddSize) rangeRest addNextRange
              {-(BlameRange bStart beforeRemainingSize bOffset `offsetBy` shift:) <$>-}
                        {-aux (shift + cutedAddSize) rangeRest addNextRange-}

          -- #######################
          --    ############
          | otherwise = do
              tell $ pure (addBeg + bOffset, addSize, tag)
              let beforeSize = addBeg - bStart
                  restBefore = blameElem `takeBy` beforeSize
                  restAfter = blameElem `dropBy` (beforeSize + addSize)

              trace (printf "(%2d) + otherwise range:%s add:%d,%d before:%s after:%s" n (show blameElem) addBeg addSize (show restBefore) (show restAfter)) $
                (offsetBy' n restBefore shift:) <$> aux (shift + addSize) (restAfter : rangeRest) addRest
              {-(restBefore `offsetBy` shift:) <$> aux shift (restAfter : rangeRest) addRest-}

