
data Act = Add 
         | Sub 
         | AddSub 
         | SubAdd
         deriving (Eq, Show, Enum)

data Range = Range Act Int Int

mergeRanges :: [(Int, Int)] -> [(Int, Int)] -> [Range]
mergeRanges []   adds = adds
mergeRanges rems   [] = rems
mergeRanges rems@((remBeg, remEnd):remRest) adds@((addBeg, addEnd):addRest)
    -- ############
    --                  ############
    | remBeg < addBeg && remEnd < addBeg = 
        Range Sub remBeg remEnd : mergeRanges remRest adds

    --                  ############
    -- ############
    | addBeg < remBeg && addEnd < remBeg =
        Range Add addBeg addEnd : mergeRanges rems addRest

    --  ############
    --         ##############
    | remBeg < addBeg && remEnd < addEnd =
        Range Sub remBeg addbeg : Range SubAdd addBeg remEnd 
                                : mergeRanges remRest ( (remEnd ,addEnd)
                                                      : addRest)

    --         ##############
    --  ############
    | addBeg < remBeg && addEnd < remEnd =
        Range Add addBeg remBeg : Range AddSub remBeg addEnd
                                : mergeRanges addRest ((addEnd, remEnd) : remRest)
    --    ##################
    --          #######
    | remBeg < addBeg && remEnd < addEnd
        Range Sub remBeg addBeg : Range SubAdd addBeg addEnd
                                : mergeRanges ((addEnd, remEnd) : remRest)
                                              addRest
    --          #######
    --    ##################
    | addBeg < remBeg && addEnd < remEnd
        Range Add addBeg remBeg : Range AddSub remBeg remEnd
                                : mergeRanges remRest
                                             ((remEnd, addEnd) : addRest)

main :: IO ()
main = return ()

