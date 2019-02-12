{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config.Default (
      module System.Environment.Config
    , module System.Environment.Config.Types
    , getConfig
    , appFileReader
    , defaultReader
) where

import System.Environment.Config.Types
import System.Environment.Config hiding (getConfig)
import System.Environment.Config.Base (envNameVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<|>))
import qualified Control.Monad.State as S (gets, modify)
import qualified System.Environment.Config as C (getConfig)

envPrefixFilter :: [String]
envPrefixFilter = [
      envNameVar
    , "~hs__"
    , "host"
    , "port"
    ]

appFileReader :: Maybe String -> EnvReader Config
appFileReader env = do
        optionalJsonFileReader "app.json"
        fileEnv <- S.gets $ get envNameVar
        maybe (return ()) readEnvFile (env <|> fileEnv)
    where 
        readEnvFile e = optionalJsonFileReader $ "app." ++ e ++ ".json"

defaultReader :: EnvReader Config
defaultReader = do
    env <- envSource envPrefixFilter
    args <- argsSource 
    let prev = args <> env
    appFileReader $ get envNameVar prev
    S.modify (prev <>)

getConfig :: forall a m. (MonadIO m, FromJSON a) => m a
getConfig = C.getConfig defaultReader
