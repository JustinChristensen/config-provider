module System.Environment.Config.Default (
      FlatConfigMap(..)
    , Value(..)
    , EnvReader
    , HasEnv
    , ToJSON
    , FromJSON
    , getConfig
    , getEnvName
    , appFileReader
    , defaultReader
    , get
    , getMaybe
    , getEither
) where

import Control.Monad.State (liftIO, gets)
import System.Environment.Config hiding (getConfig)
import Control.Applicative ((<|>))
import qualified Control.Monad.State as S (get)
import qualified System.Environment.Config as C (getConfig)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

class HasEnv a where
    env :: a -> Maybe String

instance HasEnv FlatConfigMap where
    env = getMaybe envNameVar

envNameVar :: String
envNameVar = "env"

envPrefixFilter :: [String]
envPrefixFilter = [
      envNameVar
    , "~hs__"
    , "host"
    , "port"
    ]

getEnvName :: IO (Maybe String)
getEnvName = let
        e = envNameVar
    in do
        am <- getArgPairs >>= return . unEnvPairs
        em <- getEnvPairs envPrefixFilter >>= return . unEnvPairs
        return $ lookup e am <|> lookup e em >>= \v -> case v of
            String s -> Just $ T.unpack s
            _ -> Nothing

appFileReader :: (HasEnv a, FromJSON a, Monoid a) => EnvReader a
appFileReader = let 
        readEnvFile env = jsonFileReader $ "app." ++ env ++ ".json"
    in do
        jsonFileReader "app.json"
        mEnv <- liftIO getEnvName
        fEnv <- gets env
        maybe S.get readEnvFile $ mEnv <|> fEnv

defaultReader :: (HasEnv a, FromJSON a, Monoid a) => EnvReader a
defaultReader = do
    appFileReader
    envReader envPrefixFilter
    argsReader

getConfig :: (HasEnv a, FromJSON a, Monoid a) => IO a
getConfig = C.getConfig defaultReader
