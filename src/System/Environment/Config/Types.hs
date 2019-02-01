{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config.Types (
      Mergeable
    , ConfigMap(..)
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
    , empty      
    , merge
    , getEnv
    , get
    , getM
    , getE
) where

import System.Environment.Config.Helpers
import GHC.Generics
import Control.Monad.Catch
import Control.Monad.State (StateT(..))
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch)
import Xeno.Types (XenoException)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.HashMap.Strict as H

newtype EnvPairs = EnvPairs { unEnvPairs :: [(String, Value)] }
newtype Ini = Ini I.Ini

newtype ConfigMap = ConfigMap { unConfigMap :: Value }
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

instance Semigroup ConfigMap where
    (<>) (ConfigMap c2) (ConfigMap c1) = ConfigMap $ mergeVal c2 c1
        where
            mergeVal (Object o2) (Object o1) = Object $ H.unionWith mergeVal o2 o1
            mergeVal Null v1 = v1
            mergeVal v2 _ = v2

instance Monoid ConfigMap where
    mempty = ConfigMap $ Object H.empty
    mappend = (<>)

instance Mergeable ConfigMap where
    empty = mempty
    merge = (<>)
    getEnv = get envNameVar

instance FromJSON ConfigMap where
    parseJSON v = case v of 
        Object _ -> return $ ConfigMap v
        _ -> typeMismatch "ConfigMap" v

instance ToJSON EnvPairs where
    toJSON = A.object . map packKey . unEnvPairs
        where packKey (k, v) = (T.pack k, v)

instance ToJSON Ini where
    toJSON (Ini ini) = let globals = toPair <$> I.iniGlobals ini
                           sections = H.foldrWithKey toObj [] $ I.iniSections ini
                       in A.object $ sections ++ globals
        where toPair (k, v) = (k, toVal $ Left $ T.unpack v)
              toObj section pairs acc = (section, A.object $ toPair <$> pairs) : acc

getE:: FromJSON a => String -> ConfigMap -> Either ConfigGetException a
getE path fm = getE' path $ unConfigMap fm
    where 
        getE' "" v = case A.fromJSON v of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e
        getE' p (Object m) = let (k, rest) = splitAtEl '.' p
                             in case H.lookup (T.pack k) m of
                                Just v -> getE' rest v
                                _ -> Left $ KeyNotFoundError path
        getE' _ _ = Left $ KeyNotFoundError path

getM :: FromJSON a => String -> ConfigMap -> Maybe a
getM = get

get :: forall a m. (MonadThrow m, FromJSON a) => String -> ConfigMap -> m a
get path fm = either throwM return $ getE path fm