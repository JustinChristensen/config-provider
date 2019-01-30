module System.Environment.Config.Helpers (
      lcase
    , toVal
) where

import Data.Char (toLower)
import Data.Aeson (Value(..))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as A

toVal :: Either String B.ByteString -> Value
toVal (Left v) = toVal $ Right $ B.pack v
toVal (Right v) = fromMaybe (String $ decodeUtf8 v) $ A.decodeStrict' v

lcase :: String -> String
lcase = map toLower