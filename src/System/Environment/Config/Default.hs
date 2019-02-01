module System.Environment.Config.Default (
      module System.Environment.Config
    , module System.Environment.Config.Types
    , getConfig
    , appFileReader
    , defaultReader
) where

import System.Environment.Config.Types
import System.Environment.Config hiding (getConfig)
import System.Environment.Config.Helpers (envNameVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<|>))
import qualified Control.Monad.State as S (get, gets, put)
import qualified System.Environment.Config as C (getConfig)

envPrefixFilter :: [String]
envPrefixFilter = [
      envNameVar
    , "~hs__"
    , "host"
    , "port"
    ]

appFileReader :: (FromJSON a, Mergeable a) => Maybe String -> EnvReader a
appFileReader env = do
        optionalJsonFileReader "app.json"
        fEnv <- S.gets getEnv
        maybe (return ()) readEnvFile (env <|> fEnv)
    where 
        readEnvFile e = optionalJsonFileReader $ "app." ++ e ++ ".json"

defaultReader :: (FromJSON a, Mergeable a) => EnvReader a
defaultReader = do
    envReader envPrefixFilter
    argsReader 
    prev <- S.get
    env <- S.gets getEnv
    appFileReader env
    curr <- S.get
    S.put (prev `merge` curr)

getConfig :: (MonadIO m, FromJSON a, Mergeable a) => m a
getConfig = C.getConfig defaultReader
