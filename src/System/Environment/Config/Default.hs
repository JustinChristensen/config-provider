{-# LANGUAGE LambdaCase #-}
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
    , getE
) where

import Control.Monad.State (liftIO, gets)
import System.Environment.Config hiding (getConfig)
import Control.Applicative ((<|>))
import qualified Control.Monad.State as S (get)
import qualified System.Environment.Config as C (getConfig)
import qualified Data.Text as T

class HasEnv a where
    env :: a -> Maybe String

instance HasEnv FlatConfigMap where
    env = get envNameVar

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
        return $ lookup e am <|> lookup e em >>= \case
            String s -> Just $ T.unpack s
            _ -> Nothing

appFileReader :: (HasEnv a, FromJSON a, Semigroup a) => EnvReader a
appFileReader = let 
        readEnvFile e = jsonFileReader $ "app." ++ e ++ ".json"
    in do
        jsonFileReader "app.json"
        mEnv <- liftIO getEnvName
        fEnv <- gets env
        maybe S.get readEnvFile $ mEnv <|> fEnv

defaultReader :: (HasEnv a, FromJSON a, Semigroup a, Show a) => EnvReader a
defaultReader = do
    appFileReader 
    envReader envPrefixFilter
    argsReader

getConfig :: (HasEnv a, FromJSON a, Monoid a, Show a) => IO a
getConfig = C.getConfig defaultReader
