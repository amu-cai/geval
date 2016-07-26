module GEval.PrecisionRecall
       where

f2Measure :: (a -> b -> Bool) -> [a] -> [b] -> Double
f2Measure = fMeasure 2.0

fMeasure :: Double -> (a -> b -> Bool) -> [a] -> [b] -> Double
fMeasure beta matchingFun expected got =
  (1 + betaSquared) * (p + r) / (betaSquared * p + 2)
  where betaSquared = beta ^ 2
        (p, r) = precisionAndRecall matchingFun expected got

precisionAndRecall :: (a -> b -> Bool) -> [a] -> [b] -> (Double, Double)
precisionAndRecall matchFun expected got = precisionAndRecallFromCounts cs
  where cs = counts matchFun expected got

precisionAndRecallFromCounts :: (Int, Int, Int) -> (Double, Double)
precisionAndRecallFromCounts (fp, fn, tp) = (tp / (tp+fp+epsilon), tp / (tp+fn+epsilon))
  where epsilon = 0.000001

precision :: (a -> b -> Bool) -> [a] -> [b] -> Double
precision = first . precisionAndRecall

recall :: (a -> b -> Bool) -> [a] -> [b] -> Double
recall = second . precisionAndRecall

counts :: (a -> b -> Bool) -> [a] -> [b] -> (Int, Int, Int)
counts matchFun expected got = f


countStep :: ([a], (Int, Int)) -> b -> ([a], (Int, Int))
countStep (expectedLeft, fp, tp) itemGot =
  (rest, case firstMatched of
      Just _ -> (fp, tp+1)
      Nothing -> (fp+1, tp))
  where (rest, firstMatched) = findFirst ((flip matchingFun) itemGot) expectedLeft

findFirst :: (a -> Bool) -> [a] -> ([a], Maybe a)
findFirst _ [] -> ([], Nothing)
findFirst condition (x:xs) = if (condition x)
                                then
                                  (xs, Just x)
                                else
                                  (x:xs', m)
                                  where
                                  (xs',m) = findFirst condition xs
