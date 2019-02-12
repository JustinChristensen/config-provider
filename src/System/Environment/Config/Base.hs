{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.Base where

import Control.Monad.State (StateT(..))
import Data.Char (toLower, isSpace)
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.Catch
import Xeno.Types (XenoException)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Xeno.DOM as X
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H

data ConfigNode = ConfigNode Value (H.HashMap String ConfigNode)
    deriving (Show, Eq)

instance Semigroup ConfigNode where
    (<>) (ConfigNode v2 p2) (ConfigNode v1 p1) = ConfigNode (mergeVal v2 v1) $ H.unionWith (<>) p2 p1

instance Monoid ConfigNode where
    mempty = ConfigNode Null H.empty
    mappend = (<>)

newtype JsonSourceException = AesonError String 
    deriving (Show)

instance Exception JsonSourceException where
    displayException (AesonError e) = e

newtype YamlSourceException = YamlError YL.ParseException
    deriving (Show)

instance Exception YamlSourceException where
    displayException e = case e of
        YamlError e' -> displayException e'

newtype XmlSourceException = XenoError XenoException
    deriving (Show)

instance Exception XmlSourceException where
    displayException (XenoError e) = displayException e

newtype IniSourceException = IniError String
    deriving (Show)

instance Exception IniSourceException where
    displayException (IniError e) = e

data ConfigGetException = 
      KeyNotFoundError String
    | ParseValueError String 
    deriving (Show)

instance Exception ConfigGetException where
    displayException e = case e of
        KeyNotFoundError k -> "key " ++ k ++ " not found in configuration"
        ParseValueError s -> s

newtype EnvPairs = EnvPairs { unEnvPairs :: [(String, String)] }

type EnvReader s = StateT s IO ()

instance FromJSON ConfigNode where
    parseJSON = return . toConfig

instance ToJSON ConfigNode where
    toJSON (ConfigNode v m) | H.null m = v
                            | otherwise = Object $ H.fromList $ H.foldrWithKey toPair mempty m
        where toPair k c acc = (T.pack k, A.toJSON c) : acc

class ToConfig a where
    toConfig :: a -> ConfigNode
    default toConfig :: ToJSON a => a -> ConfigNode
    toConfig = toConfig . A.toJSON

instance ToConfig ConfigNode where
    toConfig = id

instance ToConfig Value where
    toConfig (Object o) = H.foldrWithKey setProp mempty o
        where setProp k v = setC (T.unpack k) (toConfig v)
    toConfig v = ConfigNode v H.empty

instance ToConfig EnvPairs where
    toConfig = foldr setPair mempty . unEnvPairs
        where setPair (path, v) = swap (mergeVal $ tryDecodeS v) path

instance ToConfig I.Ini where
    toConfig ini = let globals = setPairs mempty $ I.iniGlobals ini
                   in H.foldrWithKey setSection globals $ I.iniSections ini
        where setPairs = foldr setPair
              setSection section pairs c = setPairs (swap id (T.unpack section) c) pairs
              setPair (path, v) = swap (mergeVal $ tryDecodeT v) (T.unpack path)

instance ToConfig X.Node where
    toConfig node = setC (B.unpack $ X.name node) (nodeConfig node) mempty

instance ToConfig X.Content where
    toConfig (X.Element node) = toConfig node
    toConfig (X.Text text) = ConfigNode (tryDecodeBS text) H.empty
    toConfig (X.CData cdata) = ConfigNode (String $ decodeUtf8 cdata) H.empty

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

nodeConfig :: X.Node -> ConfigNode
nodeConfig node = let attrs = X.attributes node
                      contents = skipWhitespace (X.contents node)
                      attrC = foldr setAttr mempty attrs
                  in foldr setContent attrC contents 
    where setAttr (k, v) = set (B.unpack k) (tryDecodeBS v)
          setContent x c = toConfig x <> c
          skipWhitespace = filter (not . isWhitespace)
          isWhitespace (X.Text text) = B.all isSpace text
          isWhitespace _ = False

setC :: String -> ConfigNode -> ConfigNode -> ConfigNode
setC k c = swapC (const c) k

swapC :: (ConfigNode -> ConfigNode) -> String -> ConfigNode -> ConfigNode
swapC f "" c = f c
swapC f path (ConfigNode v m) = let (key, rest) = splitAtEl '.' path
                                    c = swapC f rest mempty
                            in ConfigNode v $ H.insertWith (<>) key c m

find :: String -> ConfigNode -> Maybe ConfigNode
find "" c = Just c
find p (ConfigNode _ m) = let (k, rest) = splitAtEl '.' p
                      in case H.lookup k m of
                          Just c -> find rest c
                          _ -> Nothing

makeGet:: FromJSON a => (ConfigNode -> Either ConfigGetException a) -> String -> ConfigNode -> Either ConfigGetException a
makeGet f p = maybe (Left $ KeyNotFoundError p) f . find p

getE:: FromJSON a => String -> ConfigNode -> Either ConfigGetException a
getE = makeGet tryParse
    where
        tryParse (ConfigNode v _) = case A.fromJSON v of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

getM :: FromJSON a => String -> ConfigNode -> Maybe a
getM = get

get :: forall a m. (MonadThrow m, FromJSON a) => String -> ConfigNode -> m a
get p = either throwM return . getE p

bindE:: FromJSON a => String -> ConfigNode -> Either ConfigGetException a
bindE = makeGet tryBind
    where
        tryBind c = case A.fromJSON $ A.toJSON c of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

bindM :: FromJSON a => String -> ConfigNode -> Maybe a
bindM = bind

bind :: forall a m. (MonadThrow m, FromJSON a) => String -> ConfigNode -> m a
bind p = either throwM return . bindE p

swap :: (Value -> Value) -> String -> ConfigNode -> ConfigNode
swap f = let f' (ConfigNode v m) = ConfigNode (f v) m in swapC f'

set :: String -> Value -> ConfigNode -> ConfigNode
set k v = swap (const v) k