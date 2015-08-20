module GEval
    ( geval
    ) where

import Data.Conduit
import Data.Conduit.Combinators as CC
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT
import Control.Monad.Trans.Resource
import qualified Data.Conduit.List as CL
import Data.Text
import Data.Text.Read as TR
import Control.Applicative

geval :: String -> String -> IO (Double)
geval expectedFilePath outFilePath = do
  mse <- runResourceT $
    (getZipSource $ (,)
       <$> ZipSource (items expectedFilePath)
       <*> ZipSource (items outFilePath))
     $$ (CL.map itemError
         =$ averageC)
  return $ mse ** 0.5

averageC :: MonadResource m => Sink Double m Double
averageC = getZipSink
    $ (\total count -> total / fromIntegral count)
  <$> ZipSink CC.sum
  <*> ZipSink CC.length

items :: MonadResource m => String -> Source m Double
items filePath =
  CB.sourceFile filePath
  $= (CT.decode CT.utf8
      =$= CT.lines
      =$= CL.map TR.double
      =$= CC.map getValue)


itemError :: (Double, Double) -> Double
itemError (exp, out) = (exp-out)**2

getValue :: Either String (Double, Text) -> Double
getValue (Right (x, _)) = x
