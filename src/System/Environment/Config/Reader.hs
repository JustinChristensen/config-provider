{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.Reader where

import System.Environment.Config.Types
import System.Environment.Config.Source
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (Reader)
import Data.Aeson (FromJSON)
import qualified Xeno.DOM as X
import qualified Data.Ini as I

makeEnvReader :: (ConfigNode -> IO ConfigNode) -> EnvReader Options ConfigNode ()
makeEnvReader source = state $ \c1 -> do
    c2 <- source c1 
    ((), toConfig c2 <> c1)

readRemote :: (ConfigNode -> IO ConfigNode) -> EnvReader Options ConfigNode ()
readRemote = makeEnvReader

requireJsonFile :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
requireJsonFile toConfig path = makeEnvReader $ const $ jsonFileSource toConfig path

readJsonFile :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
readJsonFile toConfig path = makeEnvReader $ \c -> optionalJsonFileSource toConfig c path

requireYamlFile :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
requireYamlFile toConfig path = makeEnvReader $ const $ jsonFileSource toConfig path

readYamlFile :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
readYamlFile toConfig path = makeEnvReader $ \c -> optionalYamlFileSource toConfig c path

requireXmlFile :: (X.Node -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
requireXmlFile toConfig path = makeEnvReader $ const $ xmlFileSource toConfig path

readXmlFile :: (X.Node -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
readXmlFile toConfig path = makeEnvReader $ \c -> optionalXmlFileSource toConfig c path

requireIniFile :: (I.Ini -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
requireIniFile toConfig path = makeEnvReader $ const $ iniFileSource toConfig path

readIniFile :: (I.Ini -> Reader Options ConfigNode) -> FilePath -> EnvReader Options ConfigNode ()
readIniFile toConfig path = makeEnvReader $ \c -> optionalIniFileSource toConfig c path

readEnv :: ([String] -> [(String, String)] -> Reader Options ConfigNode) -> [String] -> EnvReader Options ConfigNode ()
readEnv toConfig prefixes = makeEnvReader $ const $ envSource toConfig prefixes

readArgs :: ([(String, String)] -> Reader Options ConfigNode) -> EnvReader Options ConfigNode ()
readArgs toConfig = makeEnvReader $ const $ argsSource toConfig

getConfig :: forall a m. (MonadIO m, FromConfig a) => Options -> EnvReader Options ConfigNode () -> m a
getConfig opts reader = runEnvReader opts reader mempty >>= bind ""