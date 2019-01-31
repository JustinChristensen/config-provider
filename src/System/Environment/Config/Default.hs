module System.Environment.Config.Default (
      module System.Environment.Config
    , module System.Environment.Config.Types
    , HasEnv
    , getEnv
    , getConfig
    , getEnvName
    , appFileReader
    , defaultReader
) where

import System.Environment.Config.Types
import Control.Monad.State (liftIO, gets)
import System.Environment.Config hiding (getConfig)
import Control.Applicative ((<|>))
import qualified System.Environment.Config as C (getConfig)
import qualified Data.Text as T

class HasEnv a where
    getEnv :: a -> Maybe String

instance HasEnv FlatConfigMap where
    getEnv = get envNameVar

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

appFileReader :: (HasEnv a, FromJSON a, Semigroup a) => EnvReader a
appFileReader = let 
        readEnvFile e = optionalJsonFileReader $ "app." ++ e ++ ".json"
    in do
        optionalJsonFileReader "app.json"
        mEnv <- liftIO getEnvName
        fEnv <- gets getEnv
        maybe (return ()) readEnvFile $ mEnv <|> fEnv

defaultReader :: (HasEnv a, FromJSON a, Semigroup a) => EnvReader a
defaultReader = do
    appFileReader
    envReader envPrefixFilter
    argsReader

getConfig :: (HasEnv a, FromJSON a, Monoid a) => IO a
getConfig = C.getConfig defaultReader
