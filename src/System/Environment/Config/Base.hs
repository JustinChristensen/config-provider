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

data Config = Config Value (H.HashMap String Config)
    deriving (Show, Eq)

instance Semigroup Config where
    (<>) (Config v2 p2) (Config v1 p1) = Config (mergeVal v2 v1) $ H.unionWith (<>) p2 p1

instance Monoid Config where
    mempty = Config Null H.empty
    mappend = (<>)

newtype JsonSourceException = AesonError String 
    deriving (Show)

instance Exception JsonSourceException where
    displayException (AesonError e) = e

data YamlSourceException = YamlAesonError String | YamlError YL.ParseException
    deriving (Show)

instance Exception YamlSourceException where
    displayException e = case e of
        YamlAesonError s -> s
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

instance FromJSON Config where
    parseJSON = return . toConfig

instance ToJSON Config where
    toJSON (Config v m) | H.null m = v
                        | otherwise = Object $ H.fromList $ H.foldrWithKey toPair mempty m
        where toPair k c acc = (T.pack k, A.toJSON c) : acc

class ToConfig a where
    toConfig :: a -> Config

instance ToConfig Value where
    toConfig (Object o) = H.foldrWithKey setProp mempty o
        where setProp k v = setC (T.unpack k) (toConfig v)
    toConfig v = Config v H.empty

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
    toConfig (X.Text text) = Config (tryDecodeBS text) H.empty
    toConfig (X.CData cdata) = Config (String $ decodeUtf8 cdata) H.empty

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

nodeConfig :: X.Node -> Config
nodeConfig node = let attrs = X.attributes node
                      contents = skipWhitespace (X.contents node)
                      attrC = foldr setAttr mempty attrs
                  in foldr setContent attrC contents 
    where setAttr (k, v) = set (B.unpack k) (tryDecodeBS v)
          setContent x c = toConfig x <> c
          skipWhitespace = filter (not . isWhitespace)
          isWhitespace (X.Text text) = B.all isSpace text
          isWhitespace _ = False

setC :: String -> Config -> Config -> Config
setC k c = swapC (const c) k

swapC :: (Config -> Config) -> String -> Config -> Config
swapC f "" c = f c
swapC f path (Config v m) = let (key, rest) = splitAtEl '.' path
                                c = swapC f rest mempty
                            in Config v $ H.insertWith (<>) key c m

find :: String -> Config -> Maybe Config
find "" c = Just c
find p (Config _ m) = let (k, rest) = splitAtEl '.' p
                      in case H.lookup k m of
                          Just c -> find rest c
                          _ -> Nothing

makeGet:: FromJSON a => (Config -> Either ConfigGetException a) -> String -> Config -> Either ConfigGetException a
makeGet f p = maybe (Left $ KeyNotFoundError p) f . find p

getE:: FromJSON a => String -> Config -> Either ConfigGetException a
getE = makeGet tryParse
    where
        tryParse (Config v _) = case A.fromJSON v of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

getM :: FromJSON a => String -> Config -> Maybe a
getM = get

get :: forall a m. (MonadThrow m, FromJSON a) => String -> Config -> m a
get p = either throwM return . getE p

bindE:: FromJSON a => String -> Config -> Either ConfigGetException a
bindE = makeGet tryBind
    where
        tryBind c = case A.fromJSON $ A.toJSON c of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

bindM :: FromJSON a => String -> Config -> Maybe a
bindM = bind

bind :: forall a m. (MonadThrow m, FromJSON a) => String -> Config -> m a
bind p = either throwM return . bindE p

swap :: (Value -> Value) -> String -> Config -> Config
swap f = let f' (Config v m) = Config (f v) m in swapC f'

set :: String -> Value -> Config -> Config
set k v = swap (const v) k