module Data.Statistics.Loess
   (loess) where

import qualified Statistics.Matrix.Types as SMT
import Statistics.Regression (ols)
import Data.Vector.Unboxed((!), zipWith, length, (++), map)
import Statistics.Matrix(transpose)


triCube :: Double -> Double
triCube d = (1.0 - (abs d) ** 3) ** 3

loess :: SMT.Vector -> SMT.Vector -> Double -> Double
loess inputs outputs x = a * x + b
  where a = params ! 1
        b = params ! 0
        params = ols inputMatrix scaledOutputs
        weights = Data.Vector.Unboxed.map (\v -> triCube (x - v)) inputs
        scaledOutputs = Data.Vector.Unboxed.zipWith (*) outputs weights
        scaledInputs = Data.Vector.Unboxed.zipWith (*) inputs weights
        inputMatrix = transpose (SMT.Matrix 2 (Data.Vector.Unboxed.length inputs) 1000 (weights Data.Vector.Unboxed.++ scaledInputs))
