module System.Environment.Config.Source where

import System.Environment.Config.Types
import System.Environment.Config.Base
import System.Environment (getArgs, getEnvironment)
import Control.Monad.Catch
import System.IO.Error (isDoesNotExistError)
import Control.Monad.Reader (Reader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as Y

class ToConfig a => FromSource a

instance FromSource Json
instance FromSource Yaml
instance FromSource Xml
instance FromSource Ini
instance FromSource Env
instance FromSource Args
instance FromSource a => FromSource (Maybe a)

class FromSource a => FromFile a where
    file :: FilePath -> EnvReader Options (Maybe a)
    requiredFile :: FilePath -> EnvReader Options a

instance FromFile Json where
    file = makeOptional requiredFile
    requiredFile = makeThrowF jsonFileE

instance FromFile Yaml where
    file = makeOptional requiredFile
    requiredFile = makeThrowF yamlFileE

instance FromFile Xml where
    file = makeOptional requiredFile
    requiredFile = makeThrowF xmlFileE

instance FromFile Ini where
    file = makeOptional requiredFile
    requiredFile = makeThrowF iniFileE

infix 2 `mapUsing`
mapUsing :: EnvReader Options a -> (a -> Reader Options ConfigNode) -> EnvReader Options ConfigNode
mapUsing src toConf = src >>= liftReader . toConf

env :: EnvReader Options Env
env = liftIO $ getEnvironment >>= return . Env

args :: EnvReader Options Args
args = liftIO $ getArgs >>= return . Args

makeThrow :: (MonadThrow m, Exception e, FromSource a) => m (Either e a) -> m a
makeThrow src = src >>= either throwM return

makeThrowF :: (MonadThrow m, Exception e, FromFile a) => (FilePath -> m (Either e a)) -> FilePath -> m a
makeThrowF src path = makeThrow $ src path

makeOptional :: (MonadCatch m, MonadIO m, FromFile a) => (FilePath -> m a) -> FilePath -> m (Maybe a)
makeOptional src path = catchIf isDoesNotExistError (Just <$> src path) (return . const Nothing)

jsonFileE :: MonadIO m => FilePath -> m (Either JsonSourceException Json)
jsonFileE path = liftIO $ A.eitherDecodeFileStrict' path >>=
    return . either (Left . AesonError) (return . Json)

yamlFileE :: MonadIO m => FilePath -> m (Either YamlSourceException Yaml)
yamlFileE path = liftIO $ Y.decodeFileEither path >>=
    return . either (Left . YamlError) (return . Yaml)

xmlFileE :: MonadIO m => FilePath -> m (Either XmlSourceException Xml)
xmlFileE path = liftIO $ B.readFile path >>= return . X.parse >>=
    return . either (Left . XenoError) (return . Xml)

iniFileE :: MonadIO m => FilePath -> m (Either IniSourceException Ini)
iniFileE path = liftIO $ I.readIniFile path >>=
    return . either (Left . IniError) (return . Ini)