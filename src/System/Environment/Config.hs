{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config (
      module System.Environment.Config.Types

    , defaultOptions
    , getConfig

    , readRemote
    , requireJsonFile
    , readJsonFile
    , requireYamlFile
    , readYamlFile
    , requireXmlFile
    , readXmlFile
    , requireIniFile
    , readIniFile
    , readEnv
    , readArgs

    , jsonFileSourceE
    , jsonFileSource
    , optionalJsonFileSource
    , yamlFileSourceE
    , yamlFileSource
    , optionalYamlFileSource
    , xmlFileSourceE
    , xmlFileSource
    , optionalXmlFileSource
    , iniFileSourceE
    , iniFileSource
    , optionalIniFileSource
    , envSource
    , argsSource
) where

import System.Environment.Config.Types hiding (getM, getE, bindM, bindE, set, swap)
import Control.Monad.Reader (runReaderT)
import System.Environment.Config.Reader (readRemote)
import qualified System.Environment.Config.Reader as R
import qualified System.Environment.Config.Source as S

defaultOptions :: Options
defaultOptions = Options {
        envNameVar = "env"
    }

getConfig :: forall a m. (MonadIO m, FromConfig a) => EnvReader Options ConfigNode () -> m a
getConfig = R.getConfig defaultOptions

requireJsonFile :: FilePath -> EnvReader Options ConfigNode ()
requireJsonFile = R.requireJsonFile valueToConfig

readJsonFile :: FilePath -> EnvReader Options ConfigNode ()
readJsonFile = R.readJsonFile valueToConfig

requireYamlFile :: FilePath -> EnvReader Options ConfigNode ()
requireYamlFile = R.requireYamlFile valueToConfig

readYamlFile :: FilePath -> EnvReader Options ConfigNode ()
readYamlFile = R.readYamlFile valueToConfig

requireXmlFile :: FilePath -> EnvReader Options ConfigNode ()
requireXmlFile = R.requireXmlFile nodeConfig

readXmlFile :: FilePath -> EnvReader Options ConfigNode ()
readXmlFile = R.readXmlFile nodeConfig

requireIniFile :: FilePath -> EnvReader Options ConfigNode ()
requireIniFile = R.requireIniFile iniToConfig

readIniFile :: FilePath -> EnvReader Options ConfigNode ()
readIniFile = R.readIniFile iniToConfig

readEnv :: [String] -> EnvReader Options ConfigNode ()
readEnv = R.readEnv filterPairsToConfig

readArgs :: EnvReader Options ConfigNode ()
readArgs = R.readArgs pairsToConfig

jsonFileSourceE :: FilePath -> EnvSource Options (Either JsonSourceException ConfigNode)
jsonFileSourceE = S.jsonFileSourceE valueToConfig

jsonFileSource :: FilePath -> EnvSource Options ConfigNode
jsonFileSource = S.jsonFileSource valueToConfig

optionalJsonFileSource :: ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalJsonFileSource = S.optionalJsonFileSource valueToConfig

yamlFileSourceE :: FilePath -> EnvSource Options (Either YamlSourceException ConfigNode)
yamlFileSourceE = S.yamlFileSourceE valueToConfig

yamlFileSource :: FilePath -> EnvSource Options ConfigNode
yamlFileSource = S.yamlFileSource valueToConfig 

optionalYamlFileSource :: ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalYamlFileSource = S.optionalYamlFileSource valueToConfig

xmlFileSourceE :: FilePath -> EnvSource Options (Either XmlSourceException ConfigNode)
xmlFileSourceE = S.xmlFileSourceE nodeConfig

xmlFileSource :: FilePath -> EnvSource Options ConfigNode
xmlFileSource = S.xmlFileSource nodeConfig

optionalXmlFileSource :: ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalXmlFileSource = S.optionalXmlFileSource nodeConfig 

iniFileSourceE :: FilePath -> EnvSource Options (Either IniSourceException ConfigNode)
iniFileSourceE = S.iniFileSourceE iniToConfig

iniFileSource :: FilePath -> EnvSource Options ConfigNode
iniFileSource = S.iniFileSource iniToConfig

optionalIniFileSource :: ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalIniFileSource = S.optionalIniFileSource iniToConfig

envSource :: [String] -> EnvSource Options ConfigNode
envSource = S.envSource filterPairs

argsSource :: EnvSource Options ConfigNode
argsSource = S.argsSource pairsToConfig