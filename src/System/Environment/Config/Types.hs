{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config.Types (
      Mergeable
    , ToConfig
    , Config(..)
    , ConfigSourceException(..)
    , ConfigGetException(..)
    , EnvPairs(..)
    , EnvReader
    , Value(..)
    , FromJSON
    , ToJSON
    , toConfig      
    , empty      
    , merge
    , getEnv
    , get
    , getM
    , getE
    , set
    , swap
) where

import Data.Char (isSpace)
import System.Environment.Config.Helpers
import Control.Monad.Catch
import Control.Monad.State (StateT(..))
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Text.Encoding (decodeUtf8)
import Xeno.Types (XenoException)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Xeno.DOM as X
import qualified Data.Yaml as YL
import qualified Data.HashMap.Strict as H

data Config = Config Value (H.HashMap String Config)
    deriving (Show, Eq)
newtype EnvPairs = EnvPairs { unEnvPairs :: [(String, String)] }

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

class ToConfig a where
    toConfig :: a -> Config

class Mergeable a where
    empty :: a
    merge :: a -> a -> a
    getEnv :: a -> Maybe String
    getEnv _ = Nothing
    {-# MINIMAL empty, merge #-}

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

instance Semigroup Config where
    (<>) (Config v2 p2) (Config v1 p1) = Config (mergeVal v2 v1) $ H.unionWith (<>) p2 p1

instance Monoid Config where
    mempty = Config Null H.empty
    mappend = (<>)

instance Mergeable Config where
    empty = mempty
    merge = (<>)
    getEnv = get envNameVar

instance ToConfig Value where
    toConfig (Object o) = H.foldrWithKey setProp empty o
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

find :: String -> Config -> Maybe Config
find "" c = Just c
find p (Config _ m) = let (k, rest) = splitAtEl '.' p
                      in case H.lookup k m of
                          Just c -> find rest c
                          _ -> Nothing

getE:: FromJSON a => String -> Config -> Either ConfigGetException a
getE path = maybe (Left $ KeyNotFoundError path) tryParse . find path
    where
        tryParse (Config v _) = case A.fromJSON v of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

getM :: FromJSON a => String -> Config -> Maybe a
getM = get

get :: forall a m. (MonadThrow m, FromJSON a) => String -> Config -> m a
get p = either throwM return . getE p

swapC :: (Config -> Config) -> String -> Config -> Config
swapC f "" c = f c
swapC f path (Config v m) = let (key, rest) = splitAtEl '.' path
                                c = swapC f rest empty
                            in Config v $ H.insertWith (<>) key c m

swap :: (Value -> Value) -> String -> Config -> Config
swap f = let f' (Config v m) = Config (f v) m in swapC f'

setC :: String -> Config -> Config -> Config
setC k c = swapC (const c) k

set :: String -> Value -> Config -> Config
set k v = swap (const v) k

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
