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
) where

import Control.Monad.State (liftIO, gets, get)
import System.Environment.Config hiding (getConfig)
import Control.Applicative ((<|>))
import qualified System.Environment.Config as C (getConfig)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

class HasEnv a where
    env :: a -> Maybe String

instance HasEnv FlatConfigMap where
    env = maybe Nothing unVal . H.lookup envNameVar . unFlatConfigMap
        where unVal (String s) = Just $ T.unpack s
              unVal _ = Nothing

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
        maybe get readEnvFile $ mEnv <|> fEnv

defaultReader :: (HasEnv a, FromJSON a, Monoid a) => EnvReader a
defaultReader = do
    appFileReader
    envReader envPrefixFilter
    argsReader

getConfig :: (HasEnv a, FromJSON a, Monoid a) => IO a
getConfig = C.getConfig defaultReader
