module System.Environment.Config.Helpers (
      Node(..)
    , Content(..)
    , lcase
    , toVal
    , nodeArray
    , splitAtEl
) where

import Data.Char (toLower, isSpace)
import Data.Aeson (Value(..), ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as A
import qualified Xeno.DOM as X
import qualified Data.Vector as V

newtype Node = Node X.Node
newtype Content = Content X.Content

instance ToJSON Node where
    toJSON n@(Node node) = A.object [(decodeUtf8 $ X.name node, nodeArray n)]

instance ToJSON Content where
    toJSON (Content (X.Element node)) = A.toJSON (Node node)
    toJSON (Content (X.Text text)) = toVal $ Right text
    toJSON (Content (X.CData cdata)) = String $ decodeUtf8 cdata

splitAtEl :: Eq a => a -> [a] -> ([a], [a])
splitAtEl x xs = let (pre, rest) = span (/= x) xs
                 in (pre, if null rest then rest else tail rest)

toVal :: Either String B.ByteString -> Value
toVal (Left v) = toVal $ Right $ B.pack v
toVal (Right v) = fromMaybe (String $ decodeUtf8 v) $ A.decodeStrict' v

lcase :: String -> String
lcase = map toLower

nodeArray :: Node -> Value
nodeArray (Node node) = let contents = A.toJSON <$> (Content <$> skipWhitespace (X.contents node))
                            attrs = A.object $ toAttrPair <$> X.attributes node
                        in toArr $ contents ++ [attrs]
    where toArr = Array . V.fromList
          toAttrPair (k, v) = (decodeUtf8 k, toVal $ Right v)
          skipWhitespace = filter (\c -> case c of
              X.Text text -> B.all (not . isSpace) text
              _ -> True)
