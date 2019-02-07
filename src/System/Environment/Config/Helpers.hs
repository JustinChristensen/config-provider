module System.Environment.Config.Helpers (
      envNameVar
    , lcase
    , tryDecodeS
    , tryDecodeT
    , tryDecodeBS
    , splitAtEl
    , mergeVal
) where

import Data.Char (toLower)
import Data.Aeson (Value(..))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

envNameVar :: String
envNameVar = "env"

splitAtEl :: Eq a => a -> [a] -> ([a], [a])
splitAtEl x xs = let (pre, rest) = span (/= x) xs
                 in (pre, if null rest then rest else tail rest)

tryDecodeS :: String -> Value
tryDecodeS = tryDecodeBS . B.pack

tryDecodeT :: T.Text -> Value
tryDecodeT = tryDecodeBS . encodeUtf8

tryDecodeBS :: B.ByteString -> Value
tryDecodeBS bs = fromMaybe (String $ decodeUtf8 bs) $ A.decodeStrict' bs

lcase :: String -> String
lcase = map toLower

mergeVal :: Value -> Value -> Value
mergeVal (Object o2) (Object o1) = Object $ H.unionWith mergeVal o2 o1
mergeVal Null v1 = v1
mergeVal v2 _ = v2