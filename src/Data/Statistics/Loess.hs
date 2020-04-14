module Data.Statistics.Loess
   (loess, clippedLoess) where

import qualified Statistics.Matrix.Types as SMT
import Statistics.Regression (ols)
import Data.Vector.Unboxed((!), zipWith, length, (++), map)
import Statistics.Matrix(transpose)

import Statistics.Distribution.Normal (standard)
import Statistics.Distribution (density)

lambda :: Double
lambda = 8.0

triCube :: Double -> Double
triCube d = (1.0 - (abs d) ** 3) ** 3

gaussian :: Double -> Double
gaussian = density standard

clippedLoess :: SMT.Vector -> SMT.Vector -> Double -> Double
clippedLoess inputs outputs x = min 1.0 $ max 0.0 $ loess inputs outputs x

loess :: SMT.Vector -> SMT.Vector -> Double -> Double
loess inputs outputs x = a * x + b
  where a = params ! 1
        b = params ! 0
        params = ols inputMatrix scaledOutputs
        weights = Data.Vector.Unboxed.map (\v -> lambda * gaussian (lambda * (x - v))) inputs
        scaledOutputs = Data.Vector.Unboxed.zipWith (*) outputs weights
        scaledInputs = Data.Vector.Unboxed.zipWith (*) inputs weights
        inputMatrix = transpose (SMT.Matrix 2 (Data.Vector.Unboxed.length inputs) (weights Data.Vector.Unboxed.++ scaledInputs))
