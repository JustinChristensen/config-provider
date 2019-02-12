{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.Reader where

import System.Environment.Config.Types
import System.Environment.Config.Source
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (execStateT, liftIO)
import Data.Aeson (FromJSON)

readRemote :: ToConfig a => (ConfigNode -> IO a) -> EnvReader ConfigNode
readRemote = makeEnvReader

requireJsonFile :: FilePath -> EnvReader ConfigNode
requireJsonFile path = makeEnvReader $ const $ jsonFile path

readJsonFile :: FilePath -> EnvReader ConfigNode
readJsonFile path = makeEnvReader $ \c -> optionalJsonFile c path

requireYamlFile :: FilePath -> EnvReader ConfigNode
requireYamlFile path = makeEnvReader $ const $ jsonFile path

readYamlFile :: FilePath -> EnvReader ConfigNode
readYamlFile path = makeEnvReader $ \c -> optionalYamlFile c path

requireXmlFile :: FilePath -> EnvReader ConfigNode
requireXmlFile path = makeEnvReader $ const $ xmlFile path

readXmlFile :: FilePath -> EnvReader ConfigNode
readXmlFile path = makeEnvReader $ \c -> optionalXmlFile c path

requireIniFile :: FilePath -> EnvReader ConfigNode
requireIniFile path = makeEnvReader $ const $ iniFile path

readIniFile :: FilePath -> EnvReader ConfigNode
readIniFile path = makeEnvReader $ \c -> optionalIniFile c path

readEnv :: [String] -> EnvReader ConfigNode
readEnv prefixes = makeEnvReader $ const $ envSource prefixes

readArgs :: EnvReader ConfigNode
readArgs = makeEnvReader $ const argsSource

getConfig :: forall a m. (MonadIO m, FromJSON a) => EnvReader ConfigNode -> m a
getConfig reader = liftIO $ execStateT reader mempty >>= bind ""