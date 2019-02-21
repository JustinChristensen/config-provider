{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, Reader)
import Control.Monad.State (MonadState, StateT)

-- fromSource then 
-- toConfig then 
-- merge then 
-- fromConfig

data Value'
data Node'
data Ini'

data ConfigNode
data Options

newtype EnvReader o s a = EnvReader { runEnvReader' :: ReaderT o (StateT s IO) a }
    deriving (Functor, Applicative, Monad, 
              MonadState s, MonadReader o, 
              MonadThrow, MonadCatch, MonadMask)

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
    file :: FilePath -> EnvReader Options ConfigNode a 
    file = undefined

    requiredFile :: FilePath -> EnvReader Options ConfigNode a
    requiredFile = undefined

instance FromFile Json -- read json file with aeson
instance FromFile Yaml -- read yaml file with yaml
instance FromFile Xml -- read xml file with xeno
instance FromFile Ini -- read ini file with ini

infixl 4 `from`
from :: EnvReader Options ConfigNode (a -> b) -> EnvReader Options ConfigNode a -> EnvReader Options ConfigNode b
from = (<*>)

env :: EnvReader Options ConfigNode Pairs
env = undefined

args :: EnvReader Options ConfigNode Pairs
args = undefined

get' :: ToConfig a => (a -> Reader Options ConfigNode) -> EnvReader Options ConfigNode (a -> ConfigNode)
get' = undefined

merge' :: ToConfig a => (a -> Reader Options ConfigNode) -> EnvReader Options ConfigNode (a -> ())
merge' = undefined

get :: ToConfig a => EnvReader Options ConfigNode (a -> ConfigNode)
get = get' toConfig

merge :: ToConfig a => EnvReader Options ConfigNode (a -> ())
merge = merge' toConfig

mergeJson :: EnvReader Options ConfigNode (Json -> ())
mergeJson = merge @Json

mergeJsonFile :: FilePath -> EnvReader Options ConfigNode () 
mergeJsonFile = (merge @Json `from`) . file

runEnvReader :: MonadIO m => Options -> ConfigNode -> EnvReader Options ConfigNode a -> m ()
runEnvReader = undefined

getConfigO :: forall a m. (MonadIO m, FromConfig a) => Options -> EnvReader Options ConfigNode () -> m a
getConfigO = undefined

getConfigR :: forall a m. (MonadIO m, FromConfig a) => EnvReader Options ConfigNode () -> m a
getConfigR = undefined

getConfig :: forall a m. (MonadIO m, FromConfig a) => m a
getConfig = undefined

getVal :: forall a m. (MonadThrow m, FromConfig a) => String -> ConfigNode -> m a
getVal = undefined

main :: IO ()
main = do
    config <- getConfigR $ do
        _ <- get @Json `from` file "config/app.json" -- >>= \configNode
        _ <- get @Json `from` requiredFile "config/app.json" -- >>= \configNode

        _ <- get @Yaml `from` file "config/app.yaml" -- >>= \configNode
        _ <- get @Yaml `from` requiredFile "config/app.yaml" -- >>= \configNode

        _ <- get @Xml `from` file "config/app.xml" -- >>= \configNode
        _ <- get @Xml `from` requiredFile "config/app.xml" -- >>= \configNode

        _ <- get @Ini `from` file "config/app.ini" -- >>= \configNode
        _ <- get @Ini `from` requiredFile "config/app.ini" -- >>= \configNode

        _ <- get @Pairs `from` env
        _ <- get @Pairs `from` args

        merge @Json `from` file "config/app.json" `from` file "config/foo.json"
        merge @Json `from` requiredFile "config/app.json"

        merge @Json `from` return undefined

        merge @Yaml `from` file "config/app.yaml"
        merge @Yaml `from` requiredFile "config/app.yaml"

        merge @Xml `from` file "config/app.xml"
        merge @Xml `from` requiredFile "config/app.xml"

        merge @Ini `from` file "config/app.ini"
        merge @Ini `from` requiredFile "config/app.ini"

        merge @Pairs `from` env
        merge @Pairs `from` args

    _ <- getVal @ConfigNode "foo.bar" config 

    return ()
