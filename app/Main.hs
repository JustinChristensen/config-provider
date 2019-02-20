{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, Reader)
import Control.Monad.State (StateT)

-- fromSource then 
-- toConfig then 
-- merge then 
-- fromConfig

data Value'
data Node'
data Ini'

data ConfigNode
data Options

type EnvReader o s a = ReaderT o (StateT s IO) a

newtype Json = Json Value'
newtype Yaml = Yaml Value'
newtype Xml = Xml Node'
newtype Ini = Ini Ini'
newtype Pairs = Pairs [(String, String)]

class FromConfig a where
    fromConfig :: ConfigNode -> Reader Options a
    fromConfig = undefined

instance FromConfig ConfigNode

class ToConfig a where
    toConfig :: a -> Reader Options ConfigNode

instance ToConfig ConfigNode where
    toConfig = return

instance ToConfig Json where
    toConfig (Json v) = valueToConfig v

instance ToConfig Yaml where
    toConfig (Yaml v) = valueToConfig v

instance ToConfig Xml where
    toConfig (Xml n) = nodeToConfig n

instance ToConfig Ini where
    toConfig (Ini i) = iniToConfig i

instance ToConfig Pairs where
    toConfig (Pairs p) = pairsToConfig p

valueToConfig :: Value' -> Reader Options ConfigNode; valueToConfig = undefined
nodeToConfig :: Node' -> Reader Options ConfigNode; nodeToConfig = undefined
iniToConfig :: Ini' -> Reader Options ConfigNode; iniToConfig = undefined
pairsToConfig :: [(String, String)] -> Reader Options ConfigNode; pairsToConfig = undefined
envToConfig :: [String] -> [(String, String)] -> Reader Options ConfigNode; envToConfig = undefined

class FromSource a where

instance FromSource Json
instance FromSource Yaml
instance FromSource Xml
instance FromSource Ini
instance FromSource Pairs

class FromSource a => FromFile a where
    fromFile :: FilePath -> IO a 
    fromFile = undefined

    fromRequiredFile :: FilePath -> IO a
    fromRequiredFile = undefined

instance FromFile Json -- read json file with aeson
instance FromFile Yaml -- read yaml file with yaml
instance FromFile Xml -- read xml file with xeno
instance FromFile Ini -- read ini file with ini

from :: (a -> IO b) -> IO b
from = undefined

fromEnv :: [String] -> IO a
fromEnv = undefined

fromArgs :: a -> IO b
fromArgs _ = undefined

get' :: FromSource a => (a -> Reader Options ConfigNode) -> (b -> IO a) -> b -> EnvReader Options ConfigNode ConfigNode
get' = undefined

merge' :: FromSource a => (a -> Reader Options ConfigNode) -> (b -> IO a) -> b -> EnvReader Options ConfigNode ()
merge' = undefined

get :: (ToConfig a, FromSource a) => (b -> IO a) -> b -> EnvReader Options ConfigNode ConfigNode
get = get' toConfig

merge :: (ToConfig a, FromSource a) => (b -> IO a) -> b -> EnvReader Options ConfigNode ()
merge = merge' toConfig

runEnvReader :: MonadIO m => Options -> ConfigNode -> EnvReader Options ConfigNode a -> m ()
runEnvReader = undefined

getConfig'' :: forall a m. (MonadIO m, FromConfig a) => Options -> EnvReader Options ConfigNode () -> m a
getConfig'' = undefined

getConfig' :: forall a m. (MonadIO m, FromConfig a) => EnvReader Options ConfigNode () -> m a
getConfig' = undefined

getConfig :: forall a m. (MonadIO m, FromConfig a) => m a
getConfig = undefined

getApiKeyFromVault  :: IO ()
getApiKeyFromVault = undefined

getVal :: forall a m. (MonadThrow m, FromConfig a) => String -> ConfigNode -> m a
getVal = undefined

-- TODO: think about how the arity of get and merge can depend on the arity of toConfig and from
main :: IO ()
main = do
    config <- getConfig' $ do
        _ <- get @Json fromFile "config/app.json" -- >>= \configNode
        _ <- get @Json fromRequiredFile "config/app.json" -- >>= \configNode

        _ <- get @Yaml fromFile "config/app.yaml" -- >>= \configNode
        _ <- get @Yaml fromRequiredFile "config/app.yaml" -- >>= \configNode

        _ <- get @Xml fromFile "config/app.xml" -- >>= \configNode
        _ <- get @Xml fromRequiredFile "config/app.xml" -- >>= \configNode

        _ <- get @Ini fromFile "config/app.ini" -- >>= \configNode
        _ <- get @Ini fromRequiredFile "config/app.ini" -- >>= \configNode

        _ <- get @Pairs fromEnv ["env", "host", "port"]
        _ <- get @Pairs fromArgs ()

        merge @Json fromFile "config/app.json"
        merge @Json fromRequiredFile "config/app.json"

        merge @Json from $ return undefined

        merge @Yaml fromFile "config/app.yaml"
        merge @Yaml fromRequiredFile "config/app.yaml"

        merge @Xml fromFile "config/app.xml"
        merge @Xml fromRequiredFile "config/app.xml"

        merge @Ini fromFile "config/app.ini"
        merge @Ini fromRequiredFile "config/app.ini"

        merge @Pairs fromEnv ["env", "host", "port"]
        merge @Pairs fromArgs ()

    _ <- getVal @ConfigNode "foo.bar" config 

    return ()