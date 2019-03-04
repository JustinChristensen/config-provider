{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config.Base where

import Data.Functor.Identity (runIdentity)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT(..), Reader, ask, mapReaderT, runReader)
import Control.Monad.Catch
import GHC.Generics
import Data.Default
import Xeno.Types (XenoException)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Scientific as S
import qualified Xeno.DOM as X
import qualified Data.Ini as I
import qualified Data.Yaml as Y
import qualified Data.Map.Strict as M

class ToConfig a where
    toConfig :: a -> Reader Options ConfigNode

class FromConfig a where
    fromConfig :: ConfigNode -> Reader Options a

newtype EnvReader o a = EnvReader { runEnvReader' :: ReaderT o IO a }
    deriving (Functor, Applicative, Monad, 
              MonadIO, MonadReader o, 
              MonadThrow, MonadCatch, MonadMask)

data Options = Options {
        envNameKey :: String,
        envPrefixFilter :: [String]
    } deriving (Show, Eq)

type Path = String
type Key = String
data Value = 
    String !T.Text | 
    Number !S.Scientific |
    Bool !Bool | 
    Null deriving (Eq, Show, Read, Generic)

instance Default Value where
    def = Null

instance Semigroup Value where 
    (<>) Null y = y
    (<>) x _ = x

instance Monoid Value where 
    mempty = Null
    mappend = (<>)

data ConfigNodeF a = ConfigNodeF a (M.Map Key (ConfigNodeF a))
    deriving (Show, Eq)

type ConfigNode = ConfigNodeF Value

instance Semigroup a => Semigroup (ConfigNodeF a) where
    (<>) (ConfigNodeF v2 p2) (ConfigNodeF v1 p1) = ConfigNodeF (v2 <> v1) $ M.unionWith (<>) p2 p1

instance Monoid a => Monoid (ConfigNodeF a) where
    mempty = ConfigNodeF mempty mempty
    mappend = (<>)

instance FromConfig ConfigNode where
    fromConfig = return

instance ToConfig ConfigNode where
    toConfig = return

newtype Json = Json A.Value 
    deriving (Show, Eq)

newtype Yaml = Yaml Y.Value 
    deriving (Show, Eq)

newtype Xml = Xml X.Node 
    deriving (Show, Eq)

newtype Ini = Ini I.Ini 
    deriving (Show, Eq)  

newtype Env = Env [(String, String)] 
    deriving (Show, Eq)

newtype Args = Args [String] 
    deriving (Show, Eq)

instance ToConfig a => ToConfig (Maybe a) where
    toConfig = maybe (return mempty) toConfig

instance ToConfig Json where
    toConfig (Json v) = valueToConfig v

instance ToConfig Yaml where
    toConfig (Yaml v) = valueToConfig v

instance ToConfig Xml where
    toConfig (Xml n) = nodeToConfig n

instance ToConfig Ini where
    toConfig (Ini i) = iniToConfig i

instance ToConfig Env where
    toConfig e = ask >>= \opts -> envFilter (envPrefixFilter opts) e

instance ToConfig Args where
    toConfig (Args a) = argsToConfig a

newtype JsonSourceException = AesonError String 
    deriving (Show)

instance Exception JsonSourceException where
    displayException (AesonError e) = e

newtype YamlSourceException = YamlError Y.ParseException
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

runEnvReader :: MonadIO m => Options -> EnvReader Options a -> m a
runEnvReader o = liftIO . (`runReaderT` o) . runEnvReader'

liftReader :: Reader Options a -> EnvReader Options a
liftReader = EnvReader . mapReaderT (return . runIdentity)

valueToConfig :: A.Value -> Reader Options ConfigNode
valueToConfig = undefined

nodeToConfig :: X.Node -> Reader Options ConfigNode
nodeToConfig = undefined

iniToConfig :: I.Ini -> Reader Options ConfigNode
iniToConfig = undefined

argsToConfig :: [String] -> Reader Options ConfigNode
argsToConfig = undefined

splitAtEl :: Eq a => a -> [a] -> ([a], [a])
splitAtEl x xs = 
    let (pre, rest) = span (/= x) xs
    in (pre, if null rest then rest else tail rest)

envFilter :: [String] -> Env -> Reader Options ConfigNode
envFilter = undefined

get :: forall a m. (MonadThrow m, FromConfig a) => Options -> Path -> ConfigNode -> m a
get = undefined

bind :: forall a m. (MonadThrow m, FromConfig a) => Options -> Path -> ConfigNode -> m a
bind = undefined

getConfig :: forall b a m. (MonadIO m, ToConfig a, FromConfig b) => Options -> EnvReader Options a -> m b
getConfig opts r = liftIO $ runEnvReader opts r >>= 
    bind opts "" . flip runReader opts . toConfig

argToPair :: String -> (String, String)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = let (k, v) = splitAtEl '=' arg
                in (k, v)

mapArgs :: [String] -> [(String, String)]
mapArgs (a1:a2:args') | a1 == "--" = mapArgs (a2:args')
                      | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args'
                      | otherwise = argToPair a1 : mapArgs (a2:args')
    where wantsArg ('-':'-':a) = '=' `notElem` a
          wantsArg _ = False
          isArg ('-':'-':_) = False
          isArg a = '=' `notElem` a
mapArgs (arg:args') = argToPair arg : mapArgs args'
mapArgs [] = []