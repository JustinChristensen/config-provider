{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config.Default (
      module System.Environment.Config
    , getConfig
    , readAppFiles
    , defaultReader
) where

import System.Environment.Config.Source
import System.Environment.Config.Types
import System.Environment.Config hiding (getConfig)
import System.Environment.Config.Base (envNameVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON)
import qualified Control.Monad.State as S (gets, modify)
import qualified System.Environment.Config as C (getConfig)

envPrefixFilter :: [String]
envPrefixFilter = [
      envNameVar
    , "~hs__"
    , "host"
    , "port"
    ]

readAppFiles :: Maybe String -> EnvReader ConfigNode
readAppFiles env = do
        readJsonFile "app.json"
        fileEnv <- S.gets $ get envNameVar
        maybe (return ()) readEnvFile (env <|> fileEnv)
    where 
        readEnvFile e = readJsonFile $ "app." ++ e ++ ".json"

defaultReader :: EnvReader ConfigNode
defaultReader = do
    env <- envSource envPrefixFilter
    args <- argsSource 
    let prev = args <> env
    readAppFiles $ get envNameVar prev
    S.modify (prev <>)

getConfig :: forall a m. (MonadIO m, FromJSON a) => m a
getConfig = C.getConfig defaultReader
