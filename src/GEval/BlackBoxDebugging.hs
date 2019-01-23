module GEval.BlackBoxDebugging
  (BlackBoxDebuggingOptions(..))
  where

data BlackBoxDebuggingOptions = BlackBoxDebuggingOptions {
  bbdoMinFrequency :: Integer,
  bbdoWordShapes :: Bool,
  bbdoBigrams :: Bool,
  bbdoCartesian :: Bool,
  bbdoMinCartesianFrequency :: Maybe Integer,
  bbdoConsiderNumericalFeatures :: Bool
}
