module Difftimeline.Blame ( BlameRange( .. )
                          , cutAddedLines
                          , shiftDeletions
                          ) where

import Prelude
import Data.Monoid( Monoid )
import Control.Applicative( Applicative, (<$>), pure )
import Control.Monad.Trans.Writer.Strict( tell, Writer )

--------------------------------------------------
----            Blaming
--------------------------------------------------
data BlameRange = BlameRange
     -- { blameStart  :: 
            {-# UNPACK #-} !Int
     -- , blameSize   :: 
            {-# UNPACK #-} !Int
     -- , blameOffset :: 
            {-# UNPACK #-} !Int
     -- }
        deriving (Eq, Show)

offsetBy :: BlameRange -> Int -> BlameRange
offsetBy (BlameRange bStart bSize bOffset) shift =
    BlameRange (bStart - shift) bSize (bOffset + shift)

cutBy :: BlameRange -> Int -> BlameRange
cutBy (BlameRange bStart bSize bOffset) size =
    BlameRange (bStart + size) (bSize - size) bOffset


shiftDeletions :: [BlameRange] -> [(Int, Int)] -> [BlameRange]
shiftDeletions = aux 0
  where aux     _   []  _ = []
        aux shift rest [] =
            [BlameRange (beg + shift) size (offset - shift) | BlameRange beg size offset <- rest]
        aux shift _ _ = error "Unimplemented"

cutAddedLines :: ( Monoid (container (Int, Int, a))
                 , Applicative container )
              => a
              -> [BlameRange]
              -> [(Int, Int)] -- ^ Add begin, add size
              -> Writer (container (Int, Int, a)) [BlameRange]
cutAddedLines tag = aux 0
  where shiftListBy shift lst = map (flip offsetBy shift) lst

        aux     _   []  _ = pure []
        aux shift rest [] = pure $ shiftListBy shift rest

        aux shift ranges@(blameElem@(BlameRange bStart bSize bOffset):rangeRest) adds@((addBeg, addSize) : addRest)
          -- range before add
          -- #######
          --            #######
          | bStart + bSize <= addBeg =
              (blameElem `offsetBy` shift :) <$> aux shift rangeRest adds

          -- add before range
          --            #######
          -- #######
          | addBeg + addSize <= bStart = aux (shift + addSize) ranges addRest

          -- intersection starting with add
          -- ??????######???????
          -- ############
          | addBeg <= bStart && addBeg + addSize <= bStart + bSize = do
              let addEnd = addBeg + addSize
                  toldSize = addEnd - bStart
                  nextRange
                    | toldSize < bSize = blameElem `cutBy` toldSize `offsetBy` shift : rangeRest
                    | otherwise = rangeRest
                  shiftSize = bStart - addBeg
              tell $ pure (bStart + bOffset, toldSize, tag)
              aux (shift + toldSize + shiftSize) nextRange addRest

          -- intersection starting with add
          -- ??????####
          -- ###############
          | addBeg <= bStart && addBeg + addSize > bStart + bSize = do
              let beforeLeftSize = bStart - addBeg
                  remainingAddSize = addSize - bSize - beforeLeftSize
              tell $ pure (bStart + bOffset, bSize, tag)

              aux (shift + beforeLeftSize) rangeRest
                  $ (bStart + bSize, remainingAddSize) : addRest

          -- ###########????
          --    ############
          | bStart < addBeg && bStart + bSize <= addBeg + addSize = do
              let beforeRemainingSize = addBeg - bStart
                  cutedAddSize = min (addBeg + addSize) (bStart + bSize) - addBeg
                  addNextRange
                    | cutedAddSize == addSize = addRest
                    | otherwise = (addBeg + cutedAddSize, addSize - cutedAddSize) : addRest

              tell $ pure (addBeg + bOffset, cutedAddSize , tag)
              (BlameRange bStart beforeRemainingSize bOffset :) <$>
                        aux (shift + cutedAddSize) rangeRest addNextRange

          -- #######################
          --    ############
          | otherwise = do
              tell $ pure (addBeg + bOffset, addSize, tag)
              let restBefore = BlameRange bStart (addBeg - bStart) bOffset
                  restAfter = BlameRange addBeg (bSize - (addBeg - bStart) - addSize) (bOffset + addSize)
              (restBefore `offsetBy` shift:) <$> aux shift (restAfter : rangeRest) addRest

