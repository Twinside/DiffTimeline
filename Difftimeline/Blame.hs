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

offsetListBy :: Int -> [BlameRange] -> [BlameRange]
offsetListBy shift lst = map (flip offsetBy shift) lst

offsetBy :: BlameRange -> Int -> BlameRange
offsetBy (BlameRange bStart bSize bOffset) shift =
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
           => a -> [BlameRange] -> [(Int, Int)] -> [(Int, Int)]
           -> WriterT (container (BlameRangeSource a)) m [BlameRange]
shiftDiffs tag blame rems adds =
    shiftDeletions rems <$> cutAddedLines tag adds blame

shiftDeletions :: [(Int, Int)] -> [BlameRange] -> [BlameRange]
shiftDeletions remRanges blames = aux 0 blames remRanges
  where aux     _   []  _ = []
        aux shift rest [] = shift `offsetListBy` rest
        -- rem before range
        --            #######
        -- #######
        aux shift ranges@((BlameRange bStart _ _):_) ((remBeg, remSize) : remRest)
          | remBeg <= bStart - shift = aux (shift - remSize) ranges remRest

        aux shift (blameElem@(BlameRange bStart bSize _):blameRest) rems@((remBeg, _) : _)
          | bStart + bSize - shift > remBeg =
              preDelRange `offsetBy` shift : aux shift ( blameElem `dropBy` cutLength : blameRest) rems
                where cutLength = remBeg - (bStart - shift)
                      preDelRange = blameElem `takeBy` cutLength 

        aux shift (blameElem:rangeRest) rems =
              blameElem `offsetBy` shift : aux shift rangeRest rems


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
              => tag
              -> [(Int, Int)] -- ^ Add begin, add size
              -> [BlameRange]
              -> WriterT (container (BlameRangeSource tag)) m [BlameRange]
cutAddedLines tag addRanges blameRanges = aux 0 blameRanges addRanges
  where aux     _   []  _ = pure []
        aux shift rest [] = pure $ shift `offsetListBy` rest
        aux shift ((BlameRange _ 0 _):rangeRest) adds = aux shift rangeRest adds
        aux shift           ranges ((_, 0) : addRest) = aux shift ranges addRest
        aux shift
            ranges@(blameElem@(BlameRange bStart bSize bOffset):rangeRest)
              adds@((addBeg, addSize) : addRest)
          -- range before add
          -- #######
          --            #######
          | bStart + bSize <= addBeg =
              (blameElem `offsetBy` shift :) <$> aux shift rangeRest adds

          -- add before range
          --            #######
          -- #######
          | addBeg + addSize <= bStart =
                aux (shift + addSize) ranges addRest

          -- intersection starting with add
          --    #####...
          -- ########...
          | addBeg < bStart =
              let addBeforeSize = bStart - addBeg
              in aux (shift + addBeforeSize) ranges
                     ((addBeg + addBeforeSize, addSize - addBeforeSize):addRest)

          -- ########...
          --    #####...
          | bStart < addBeg =
              let beforeRemainingSize = addBeg - bStart
              in ((blameElem `takeBy` beforeRemainingSize) `offsetBy` shift:) <$>
                        aux shift (blameElem `dropBy` beforeRemainingSize : rangeRest) adds

          -- ######
          -- ########...
          | addBeg == bStart && bStart + bSize <= addBeg + addSize = do
              tell . pure $ BlameRangeSource (bStart + bOffset) bSize bStart tag
              aux (shift + bSize)  rangeRest $ (addBeg + bSize, addSize - bSize) : addRest

          -- ######
          -- ########...
          | addBeg == bStart && bStart + bSize > addBeg + addSize =
              aux shift ( blameElem `takeBy` addSize
                        : blameElem `dropBy` addSize : rangeRest) adds

        aux _ _ _ = error "Hmm... no"

