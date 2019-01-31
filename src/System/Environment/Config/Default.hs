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
import qualified Control.Monad.State as S (get, gets, put)
import qualified System.Environment.Config as C (getConfig)

envPrefixFilter :: [String]
envPrefixFilter = [
      envNameVar
    , "~hs__"
    , "host"
    , "port"
    ]

appFileReader :: (FromJSON a, Mergeable a) => EnvReader a
appFileReader = do
        optionalJsonFileReader "app.json"
        mEnv <- S.gets getEnv
        maybe (return ()) readEnvFile mEnv
    where 
        readEnvFile e = optionalJsonFileReader $ "app." ++ e ++ ".json"

defaultReader :: (FromJSON a, Mergeable a) => EnvReader a
defaultReader = do
    envReader envPrefixFilter
    argsReader 
    prev <- S.get
    appFileReader
    curr <- S.get
    S.put (prev `merge` curr)

getConfig :: (MonadIO m, FromJSON a, Mergeable a) => m a
getConfig = C.getConfig defaultReader
