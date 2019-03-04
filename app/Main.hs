{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, Reader)

data ConfigNode = ConfigNode
data Options = Options

instance Semigroup ConfigNode where
    (<>) = undefined

instance Monoid ConfigNode where
    mappend = (<>)
    mempty = undefined

newtype EnvReader o a = EnvReader { runEnvReader' :: ReaderT o IO a }
    deriving (Functor, Applicative, Monad, 
              MonadIO, MonadReader o, 
              MonadThrow, MonadCatch, MonadMask)

newtype Json = Json ()
newtype Yaml = Yaml ()
newtype Xml = Xml ()
newtype Ini = Ini ()
newtype Env = Env [(String, String)]
newtype Args = Args [String]

class FromConfig a where
    fromConfig :: ConfigNode -> Reader Options a
    fromConfig = undefined

instance FromConfig ConfigNode

class ToConfig a where
    toConfig :: a -> Reader Options ConfigNode
    toConfig = undefined

instance ToConfig ConfigNode 
instance ToConfig a => ToConfig (Maybe a)
instance ToConfig Json
instance ToConfig Yaml
instance ToConfig Xml
instance ToConfig Ini
instance ToConfig Env
instance ToConfig Args

class ToConfig a => FromSource a where
instance FromSource a => FromSource (Maybe a)
instance FromSource Json
instance FromSource Yaml
instance FromSource Xml
instance FromSource Ini
instance FromSource Env
instance FromSource Args

class FromSource a => FromFile a where
    file :: FilePath -> EnvReader Options (Maybe a)
    file = undefined

    requiredFile :: FilePath -> EnvReader Options a
    requiredFile = undefined

instance FromFile Json 
instance FromFile Yaml
instance FromFile Xml
instance FromFile Ini

env :: EnvReader Options Env
env = undefined

args :: EnvReader Options Args
args = undefined

runEnvReader :: MonadIO m => Options -> EnvReader Options a -> m a
runEnvReader = undefined

getConfigO :: forall a m. (MonadIO m, FromConfig a) => Options -> EnvReader Options () -> m a
getConfigO = undefined

getConfigR :: forall b a m. (MonadIO m, ToConfig a, FromConfig b) => EnvReader Options a -> m b
getConfigR = undefined

getConfig :: forall a m. (MonadIO m, FromConfig a) => m a
getConfig = undefined

infix 2 `mapUsing`
mapUsing :: EnvReader Options a -> (a -> Reader Options ConfigNode) -> EnvReader Options ConfigNode
mapUsing = undefined

infixl 1 `merge`
merge :: (ToConfig a, ToConfig b) => EnvReader Options a -> EnvReader Options b -> EnvReader Options ConfigNode
merge = undefined

envFilter :: [String] -> Env -> Reader Options ConfigNode
envFilter = undefined

main :: IO ()
main = do
    config <- getConfigR @ConfigNode $ 
        file @Json "app.json" `merge`
        file @Xml "app.qa.xml" `merge`
        env `mapUsing` envFilter ["foo", "bar", "baz"] `merge`
        args

    return ()
