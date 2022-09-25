{-# LANGUAGE OverloadedStrings #-}

module GEval.Selector
    ( Selector(..),
      DataFormat(..),
      ItemTarget(..),
      TargetRecord(..),
      liftOp,
      select,
      parseSelector ) where

import GEval.Common (SourceItem(..))

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Text.Encoding as DTE
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder(toLazyByteString)
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL

import Data.Aeson.KeyMap as DAKM
import Data.Aeson.Key as DAK

data Selector = Selector [T.Text]
     deriving (Eq, Show)

data DataFormat = Tsv | Jsonl
     deriving (Eq, Show)

data ItemTarget = RawItemTarget T.Text | PartiallyParsedItemTarget [T.Text]
     deriving (Eq, Show)

data TargetRecord = TargetRecord (SourceItem ItemTarget) (SourceItem ItemTarget) (SourceItem ItemTarget)

parseSelector :: String -> Selector
parseSelector = Selector . T.splitOn "/" . T.pack

liftOp :: (T.Text -> a) -> (ItemTarget -> a)
liftOp fun (RawItemTarget t) = fun t
liftOp fun (PartiallyParsedItemTarget t) = fun (T.intercalate " " t)

select :: DataFormat -> Maybe Selector -> T.Text -> ItemTarget
select _ Nothing t = RawItemTarget t
select Tsv (Just _) _ = error "selectors not handled for TSVs"
select Jsonl (Just selector) t = case selectInJson selector $ decode'' $ t of
       Just v -> finalSelect v
       Nothing -> error "selector failed"

finalSelect :: Value -> ItemTarget
finalSelect (Array array) = PartiallyParsedItemTarget $ V.toList $ V.map (\e -> DTE.decodeUtf8 $ toStrict $ encodePretty' encConfig e) array
finalSelect val = RawItemTarget $ DTE.decodeUtf8 $ toStrict $ encodePretty' encConfig val

encConfig = Config {
          confIndent = Spaces 0,
          confCompare = compare,
          confNumFormat = Generic,
          confTrailingNewline = False }

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

-- TODO get rid of this
decode'' :: FromJSON a => T.Text -> Maybe a
decode'' = decode . toLazyByteString . encodeUtf8Builder

selectInJson :: Selector -> Maybe Value -> Maybe Value
selectInJson _ Nothing = Nothing
selectInJson (Selector []) value = value
selectInJson (Selector (h:r)) (Just (Object object)) =
    selectInJson (Selector r) (DAKM.lookup (DAK.fromText h) object)
selectInJson _ _ = Nothing
