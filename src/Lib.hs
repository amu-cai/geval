module Lib
    ( geval
    ) where

geval :: String -> String -> IO (Double)
geval expectedFilePath outFilePath = do
  return $ 3.14
