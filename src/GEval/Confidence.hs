module GEval.Confidence
  (totalLineConfidence)
  where

import Data.Text
import GEval.ProbList
import GEval.Probability

totalLineConfidence :: Text -> Double
totalLineConfidence t = case parseIntoProbList t of
  ProbList [] -> 1.0
  ProbList wordWithProbs -> (product $ Prelude.map (\(WordWithProb _ p) -> getP p) wordWithProbs) ** (1/l)
     where l = fromIntegral $ Prelude.length wordWithProbs
