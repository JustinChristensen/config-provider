{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config.Source where

import System.Environment.Config.Types
import System.Environment.Config.Base
import System.Environment (getArgs, getEnvironment)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import System.IO.Error (isDoesNotExistError)
import Control.Monad.State (StateT(..), liftIO)
import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON)
import Data.Either (either)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as Y

type SourceFn a e = (a -> Reader Options ConfigNode) -> EnvSource Options (Either e ConfigNode)
type FileSourceFn a e = (a -> Reader Options ConfigNode) -> FilePath -> EnvSource Options (Either e ConfigNode)

makeThrow :: Exception e => SourceFn a e -> (a -> Reader Options ConfigNode) -> EnvSource Options ConfigNode
makeThrow source toConfig = source toConfig >>= either throwM return

makeThrowF :: Exception e => FileSourceFn a e -> (a -> Reader Options ConfigNode) -> FilePath -> EnvSource Options ConfigNode
makeThrowF source toConfig path = makeThrow (`source` path) toConfig

makeOptional :: ((a -> Reader Options ConfigNode) -> ConfigNode -> FilePath -> EnvSource Options ConfigNode) -> (a -> Reader Options ConfigNode) -> ConfigNode -> FilePath -> EnvSource Options ConfigNode
makeOptional source toConfig config path = catchIf isDoesNotExistError (source toConfig path) (const $ return config)

jsonFileSourceE :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvSource Options (Either JsonSourceException ConfigNode)
jsonFileSourceE toConfig path = liftIO $ A.eitherDecodeFileStrict' path >>= 
    return . either (Left . AesonError) (return . toConfig)

jsonFileSource :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvSource Options ConfigNode
jsonFileSource = makeThrowF jsonFileSourceE

optionalJsonFileSource :: FromJSON a => (a -> Reader Options ConfigNode) -> ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalJsonFileSource = makeOptional jsonFileSource

yamlFileSourceE :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvSource Options (Either YamlSourceException ConfigNode)
yamlFileSourceE toConfig path = liftIO $ Y.decodeFileEither path >>= 
    return . either (Left . YamlError) (return . toConfig)

yamlFileSource :: FromJSON a => (a -> Reader Options ConfigNode) -> FilePath -> EnvSource Options ConfigNode
yamlFileSource = makeThrowF yamlFileSourceE

optionalYamlFileSource :: FromJSON a => (a -> Reader Options ConfigNode) -> ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalYamlFileSource = makeOptional yamlFileSource

xmlFileSourceE :: (X.Node -> Reader Options ConfigNode) -> FilePath -> EnvSource Options (Either XmlSourceException ConfigNode)
xmlFileSourceE toConfig path = liftIO $ B.readFile path >>= return . X.parse >>= 
    return . either (Left . XenoError) (return . toConfig)

xmlFileSource :: (X.Node -> Reader Options ConfigNode) -> FilePath -> EnvSource Options ConfigNode
xmlFileSource = makeThrowF xmlFileSourceE

optionalXmlFileSource :: (X.Node -> Reader Options ConfigNode) -> ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalXmlFileSource = makeOptional xmlFileSource

iniFileSourceE :: (I.Ini -> Reader Options ConfigNode) -> FilePath -> EnvSource Options (Either IniSourceException ConfigNode)
iniFileSourceE toConfig path = liftIO $ I.readIniFile path >>=
    return . either (Left . IniError) (return . toConfig)

iniFileSource :: (I.Ini -> Reader Options ConfigNode) -> FilePath -> EnvSource Options ConfigNode
iniFileSource = makeThrowF iniFileSourceE

optionalIniFileSource :: (I.Ini -> Reader Options ConfigNode) -> ConfigNode -> FilePath -> EnvSource Options ConfigNode
optionalIniFileSource = makeOptional iniFileSource

envSource :: ([String] -> [(String, String)] -> Reader Options ConfigNode) -> [String] -> EnvSource Options ConfigNode
envSource toConfig prefixes = do
    envPairs <- liftIO getEnvironment
    return $ toConfig prefixes envPairs

argsSource :: ([(String, String)] -> Reader Options ConfigNode) -> EnvSource Options ConfigNode
argsSource toConfig = do
    argPairs <- liftIO getArgs
    return $ toConfig argPairs