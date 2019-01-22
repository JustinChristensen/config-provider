module System.Environment.Config.Default (
      Config
    , EnvReader
    , Value(..)
    , getConfig
    , getEnvName
    , appFileReader
    , defaultReader
) where

import Control.Monad.State (liftIO, gets)
import System.Environment.Config hiding (getConfig)
import Control.Applicative ((<|>))
import qualified System.Environment.Config as C (getConfig)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H

envNameVar :: String
envNameVar = "env"

envPrefixFilter :: [String]
envPrefixFilter = [
      envNameVar
    , "~hs__"
    , "host"
    , "port"
    ]

getEnvName :: IO (Maybe Value)
getEnvName = let
        e = envNameVar
    in do
        am <- getArgMap
        em <- getEnvMap envPrefixFilter
        return $ lookup e am <|> lookup e em

appFileReader :: EnvReader ()
appFileReader = let 
        readEnvFile env = case env of
            String env -> jsonFileReader $ "app." ++ T.unpack env ++ ".json"
            _ -> return ()
    in do
        jsonFileReader "app.json"
        mEnv <- liftIO getEnvName
        fEnv <- gets (H.lookup "env")
        maybe (return ()) readEnvFile $ mEnv <|> fEnv

defaultReader :: EnvReader ()
defaultReader = do
    appFileReader
    envReader envPrefixFilter
    argsReader

getConfig :: IO Config
getConfig = C.getConfig defaultReader
