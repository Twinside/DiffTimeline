
data Act = Add
         | Sub
         | AddSub
         | SubAdd
         deriving (Eq, Show, Enum)

data Range = Range Act Int Int
    deriving (Eq, Show)

combine :: Act -> Act -> Act
combine Add Sub = AddSub
combine Sub Add = SubAdd
combine _ _ = error "Impossible case"

type Offset = Int
type Size = Int

mergeRanges :: [(Act, (Offset, Offset))] -> [(Act, (Offset, Offset))] -> [Range]
mergeRanges []     [] = []
mergeRanges []   vals = [Range a i j | (a, (i,j)) <- vals, i <= j]
mergeRanges vals   [] = [Range a i j | (a, (i,j)) <- vals, i <= j]
mergeRanges lefts@((a, (leftBeg, leftEnd)):leftRest) 
            rights@((b, (rightBeg, rightEnd)):rightRest)
    | rightBeg >= rightEnd = mergeRanges lefts rightRest
    | leftBeg >= leftEnd = mergeRanges leftRest rights

    -- inverse
    | rightBeg < leftBeg = mergeRanges rights lefts

    -- ############
    --                  ############
    | leftBeg < rightBeg && leftEnd < rightBeg =
        Range a leftBeg leftEnd : mergeRanges leftRest rights

    --  ############
    --         ##############
    --         or
    --  ############
    --         #####
    | leftBeg < rightBeg && leftEnd <= rightEnd =
        Range a leftBeg (rightBeg - 1)
            : Range (combine a b) rightBeg leftEnd
            : mergeRanges leftRest ((b, (leftEnd + 1, rightEnd)) : rightRest)

    --  ##########
    --  ############
    | leftBeg == rightBeg = if leftEnd <= rightEnd
        then Range (combine a b) leftBeg leftEnd 
                : mergeRanges leftRest ((b, (leftEnd + 1, rightEnd)):rightRest)
        -- leftEnd > rightEnd
        else Range (combine a b) leftBeg rightEnd
                : mergeRanges ((a, (rightEnd + 1, leftEnd)):leftRest) rightRest

    --    ##################
    --          #######
    | leftBeg < rightBeg && leftEnd > rightEnd =
        Range a leftBeg (rightBeg  - 1)
            : Range (combine a b) rightBeg rightEnd
            : mergeRanges ((a, (rightEnd + 1, leftEnd)) : leftRest) rightRest

    | otherwise = error "Should never happen"

main :: IO ()
main = do
    print $ mergeRanges [(Add, (2,4))] [(Sub, (5,6))]
    print $ mergeRanges [(Sub, (5,6))] [(Add, (2,4))] 
    print $ mergeRanges [(Add, (1,7))] [(Sub, (5, 10))]
    print $ mergeRanges [(Add, (1,7))] [(Sub, (5, 7))]

    print $ mergeRanges [(Add, (1,7))] [(Sub, (1, 7))]
    print $ mergeRanges [(Add, (1,7))] [(Sub, (1, 9))]
    print $ mergeRanges [(Add, (1,9))] [(Sub, (1, 7))]

    print $ mergeRanges [(Add, (3, 29))]
                        [(Sub, (1, 4)), (Sub, (8, 10)), (Sub, (16, 32))]

