module GEval.TextF1
    ( AggregatedResult
    , aggregatedResultZero
    , getTextF1SingleLine
    , getSentenceF1SingleLine
    , polevalAgg
    , f1TextPoleval
    ) where


import           Data.Conduit             (ConduitM)
import           Data.Conduit.Combinators (foldl)
import           Data.List.Split          (splitOn)
import           GHC.Float                (int2Double)


data AggregatedResult = AggregatedResult
    { col0  :: [Int]
    , col1  :: [Int]
    , col2  :: [Int]
    , col3  :: [Int]
    , col4  :: [Int]
    , col5  :: [Int]
    , col6  :: [Int]
    , col7  :: [Int]
    , col8  :: [Int]
    , col9  :: [Int]
    , col10 :: [Int]
    } deriving (Show, Read)

aggregatedResultZero :: AggregatedResult
aggregatedResultZero = AggregatedResult
    { col0  = [0, 0, 0, 0, 0, 0]
    , col1  = [0, 0, 0, 0, 0, 0]
    , col2  = [0, 0, 0, 0, 0, 0]
    , col3  = [0, 0, 0, 0, 0, 0]
    , col4  = [0, 0, 0, 0, 0, 0]
    , col5  = [0, 0, 0, 0, 0, 0]
    , col6  = [0, 0, 0, 0, 0, 0]
    , col7  = [0, 0, 0, 0, 0, 0]
    , col8  = [0, 0, 0, 0, 0, 0]
    , col9  = [0, 0, 0, 0, 0, 0]
    , col10 = [0, 0, 0, 0, 0, 0]
    }

{-
Legend:

0 - Irrelevant
1 - True Positive
2 - True Negative
3 - False Positive
4 - False Negative
5 - no parse
-}
score :: (String, String) -> Int
score x
    | x == ("True", "True")   = 1
    | x == ("False", "False") = 2
    | x == ("False", "True")  = 3
    | x == ("True", "False")  = 4
    | otherwise               = 5


getTextF1SingleLine :: String -> String -> String -> [Int]
getTextF1SingleLine input expected output
    | input == "###########################" = result
    | otherwise = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    where
        expectedList = splitOn "\t" expected
        outputList = splitOn "\t" output
        zipped = zip expectedList outputList
        result = [score x | x <- zipped]


getSentenceF1SingleLine :: String -> String -> String -> [Int]
getSentenceF1SingleLine input expected output
    | input == "###########################" = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    | otherwise = result
    where
        expectedList = splitOn "\t" expected
        outputList = splitOn "\t" output
        zipped = zip expectedList outputList
        result = [score x | x <- zipped]


countPolevalAgg :: AggregatedResult -> [Int] -> AggregatedResult
countPolevalAgg agg xs = AggregatedResult
    { col0  = take (xs !! 0) oldCol0 ++ [succ (oldCol0 !! (xs !! 0))] ++ drop (succ (xs !! 0)) oldCol0
    , col1  = take (xs !! 1) oldCol1 ++ [succ (oldCol1 !! (xs !! 1))] ++ drop (succ (xs !! 1)) oldCol1
    , col2  = take (xs !! 2) oldCol2 ++ [succ (oldCol2 !! (xs !! 2))] ++ drop (succ (xs !! 2)) oldCol2
    , col3  = take (xs !! 3) oldCol3 ++ [succ (oldCol3 !! (xs !! 3))] ++ drop (succ (xs !! 3)) oldCol3
    , col4  = take (xs !! 4) oldCol4 ++ [succ (oldCol4 !! (xs !! 4))] ++ drop (succ (xs !! 4)) oldCol4
    , col5  = take (xs !! 5) oldCol5 ++ [succ (oldCol5 !! (xs !! 5))] ++ drop (succ (xs !! 5)) oldCol5
    , col6  = take (xs !! 6) oldCol6 ++ [succ (oldCol6 !! (xs !! 6))] ++ drop (succ (xs !! 6)) oldCol6
    , col7  = take (xs !! 7) oldCol7 ++ [succ (oldCol7 !! (xs !! 7))] ++ drop (succ (xs !! 7)) oldCol7
    , col8  = take (xs !! 8) oldCol8 ++ [succ (oldCol8 !! (xs !! 8))] ++ drop (succ (xs !! 8)) oldCol8
    , col9  = take (xs !! 9) oldCol9 ++ [succ (oldCol9 !! (xs !! 9))] ++ drop (succ (xs !! 9)) oldCol9
    , col10 = take (xs !! 10) oldCol10 ++ [succ (oldCol10 !! (xs !! 10))] ++ drop (succ (xs !! 10)) oldCol10
    }
    where
        oldCol0  = col0 agg
        oldCol1  = col1 agg
        oldCol2  = col2 agg
        oldCol3  = col3 agg
        oldCol4  = col4 agg
        oldCol5  = col5 agg
        oldCol6  = col6 agg
        oldCol7  = col7 agg
        oldCol8  = col8 agg
        oldCol9  = col9 agg
        oldCol10 = col10 agg


polevalAgg :: Monad m => ConduitM [Int] o m AggregatedResult
polevalAgg = Data.Conduit.Combinators.foldl countPolevalAgg aggregatedResultZero

f1TextPoleval :: AggregatedResult -> Double
f1TextPoleval aggResult = sumAll / 11
    where
        f1Col0 = getF1Col $ col0 aggResult
        f1Col1 = getF1Col $ col1 aggResult
        f1Col2 = getF1Col $ col2 aggResult
        f1Col3 = getF1Col $ col3 aggResult
        f1Col4 = getF1Col $ col4 aggResult
        f1Col5 = getF1Col $ col5 aggResult
        f1Col6 = getF1Col $ col6 aggResult
        f1Col7 = getF1Col $ col7 aggResult
        f1Col8 = getF1Col $ col8 aggResult
        f1Col9 = getF1Col $ col9 aggResult
        f1Col10 = getF1Col $ col10 aggResult

        sumAll = sum [f1Col0, f1Col1, f1Col2, f1Col3, f1Col4, f1Col5, f1Col6, f1Col7, f1Col8, f1Col9, f1Col10]

        getF1Col :: [Int] -> Double
        getF1Col col = tpCol / (tpCol + ((fpCol + fnCol) / 2))
            where
                tpCol = int2Double $ col !! 1
                fpCol = int2Double $ col !! 3
                fnCol = int2Double $ col !! 4

