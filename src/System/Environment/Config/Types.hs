{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config.Types (
      FlatConfigMap(..)
    , ConfigSourceException(..)
    , ConfigGetException(..)
    , EnvPairs(..)
    , Node(..)
    , Content(..)
    , Ini(..)
    , EnvReader
    , Value(..)
    , FromJSON
    , ToJSON
    , nodeArray
) where

import System.Environment.Config.Helpers
import GHC.Generics
import Control.Monad.Catch
import Control.Monad.State (StateT(..))
import Data.Char (isSpace)
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Text.Encoding (decodeUtf8)
import Xeno.Types (XenoException)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.HashMap.Strict as H

newtype EnvPairs = EnvPairs { unEnvPairs :: [(String, Value)] }
newtype Node = Node X.Node
newtype Content = Content X.Content
newtype Ini = Ini I.Ini

newtype FlatConfigMap = FlatConfigMap { unFlatConfigMap :: H.HashMap String Value }
    deriving (Show, Eq, Generic)

type EnvReader s = StateT s IO ()

data ConfigSourceException = 
      AesonError String 
    | YamlError YL.ParseException
    | XenoError XenoException
    | IniError String
    deriving (Show)

data ConfigGetException = 
      KeyNotFoundError String 
    | ParseValueError String 
    deriving (Show)

instance Exception ConfigGetException where
    displayException e = case e of
        KeyNotFoundError k -> "key " ++ k ++ " not found in configuration"
        ParseValueError s -> s

instance Exception ConfigSourceException where
    displayException e = case e of
        AesonError s -> s
        YamlError e' -> displayException e'
        XenoError e' -> displayException e'
        IniError s -> s

instance Semigroup FlatConfigMap where
    (<>) (FlatConfigMap c2) (FlatConfigMap c1) = FlatConfigMap $ H.unionWith pickVal c2 c1
        where
            pickVal Null v1 = v1
            pickVal v2 _ = v2

instance Monoid FlatConfigMap where
    mempty = FlatConfigMap H.empty
    mappend = (<>)

instance FromJSON FlatConfigMap where
    parseJSON o@(Object _) = return $ flatConfigMap o
    parseJSON a@(Array _) = return $ flatConfigMap a
    parseJSON invalid = typeMismatch "FlatConfigMap" invalid

instance ToJSON Node where
    toJSON n@(Node node) = A.object [(decodeUtf8 $ X.name node, nodeArray n)]

instance ToJSON Content where
    toJSON (Content (X.Element node)) = A.toJSON (Node node)
    toJSON (Content (X.Text text)) = toVal $ Right text
    toJSON (Content (X.CData cdata)) = String $ decodeUtf8 cdata

instance ToJSON EnvPairs where
    toJSON = A.object . map packKey . unEnvPairs
        where packKey (k, v) = (T.pack k, v)

instance ToJSON Ini where
    toJSON (Ini ini) = let globals = toPair <$> I.iniGlobals ini
                           sections = H.foldrWithKey toObj [] $ I.iniSections ini
                       in A.object $ sections ++ globals
        where toPair (k, v) = (k, toVal $ Left $ T.unpack v)
              toObj section pairs acc = (section, A.object $ toPair <$> pairs) : acc

flatConfigMap :: Value -> FlatConfigMap
flatConfigMap v = FlatConfigMap $ H.fromList $ flatten "" (Right "") v []

nodeArray :: Node -> Value
nodeArray (Node node) = let contents = A.toJSON <$> (Content <$> skipWhitespace (X.contents node))
                            attrs = A.object $ toAttrPair <$> X.attributes node
                        in toArr $ contents ++ [attrs]
    where toArr = Array . V.fromList
          toAttrPair (k, v) = (decodeUtf8 k, toVal $ Right v)
          skipWhitespace = filter (\c -> case c of
              X.Text text -> B.all (not . isSpace) text
              _ -> True)

dot :: String -> Either Int T.Text -> String
dot "" (Right k) = T.unpack k
dot p (Left k) = p ++ '[' : show k ++ "]"
dot p (Right k) = p ++ '.' : T.unpack k

flatten :: String -> Either Int T.Text -> Value -> [(String, Value)] -> [(String, Value)]
flatten path key val acc = case val of
    Object o -> H.foldrWithKey (flatten (dot path key) . Right) acc o
    Array a -> flattenArray path key a acc
    v -> (lcase $ dot path key, v) : acc

flattenArray :: String -> Either Int T.Text -> A.Array -> [(String, Value)] -> [(String, Value)]
flattenArray path key arr acc = if hasObjects arr then
            V.foldr (flatten path key) acc arr
        else 
            V.ifoldr (flatten (dot path key) . Left) acc arr
    where hasObjects = V.any isObject
          isObject (Object _) = True
          isObject _ = False