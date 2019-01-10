module System.Environment.Config (
      getConfig
    , getConfigDefault
    , jsonFileReader
    , xmlFileReader
    , yamlFileReader
    , iniFileReader
    , remoteReader
    , envReader
    , argsReader
    , defaultReader
    , getEnvName
) where

import System.Environment (lookupEnv, getArgs, getEnvironment)
import Control.Monad.State (StateT, runStateT, state, liftIO)
import qualified Data.Map as M

type Config = M.Map String String
type EnvReader = StateT Config IO

defaultEnvNameVar :: String
defaultEnvNameVar = "ENV"

defaultEnvPrefixFilter :: [String]
defaultEnvPrefixFilter = [
      defaultEnvNameVar
    , "HS_"
    , "HOST"
    , "PORT"]

getEnvName :: IO (Maybe String)
getEnvName = lookupEnv defaultEnvNameVar

jsonFileReader :: FilePath -> EnvReader ()
jsonFileReader path = return ()

xmlFileReader :: FilePath -> EnvReader ()
xmlFileReader path = return ()

yamlFileReader :: FilePath -> EnvReader ()
yamlFileReader path = return ()

iniFileReader :: FilePath -> EnvReader ()
iniFileReader path = return ()

remoteReader :: IO Config -> EnvReader ()
remoteReader action = return ()

envReader :: [String] -> EnvReader ()
envReader filter = return ()

argsReader :: EnvReader ()
argsReader = return ()

defaultReader :: EnvReader ()
defaultReader = do
    jsonFileReader "config/app.json"
    do
        mEnv <- liftIO getEnvName 
        case mEnv of
            Just env -> jsonFileReader $ "config/app." ++ env ++ ".json"
            _ -> return ()
    envReader defaultEnvPrefixFilter
    argsReader

getConfigDefault :: IO Config
getConfigDefault = getConfig defaultReader

getConfig :: EnvReader () -> IO Config
getConfig reader = do
    (_, config) <- runStateT reader M.empty
    return config
