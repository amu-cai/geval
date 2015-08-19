module Lib
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

geval :: String -> String -> IO (Double)
geval expectedFilePath outFilePath = do
  runResourceT $
    CB.sourceFile expectedFilePath
    $$ CT.decode CT.utf8
    =$= CT.lines
    =$= CL.map TR.double
    =$= CC.map getValue
    =$ CC.sum

getValue :: Either String (Double, Text) -> Double
getValue (Right (x, _)) = x
