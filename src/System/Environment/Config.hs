module System.Environment.Config (getConfig) where

import System.Environment (lookupEnv, getArgs, getEnvironment)
import Control.Monad.State (StateT, runStateT, state, liftIO)
import qualified Data.Map as M

type Config = M.Map String String
type EnvReader = StateT Config IO

jsonFileSource :: FilePath -> EnvReader ()
jsonFileSource path = return ()

xmlFileSource :: FilePath -> EnvReader ()
xmlFileSource path = return ()

yamlFileSource :: FilePath -> EnvReader ()
yamlFileSource path = return ()

iniFileSource :: FilePath -> EnvReader ()
iniFileSource path = return ()

envSource :: EnvReader ()
envSource = return ()

argsSource :: EnvReader ()
argsSource = return ()

getEnvName :: IO (Maybe String)
getEnvName = lookupEnv "ENV"

defaultReader :: EnvReader ()
defaultReader = do
    jsonFileSource "config/app.json"
    do
        mEnv <- liftIO getEnvName 
        case mEnv of
            Just env -> jsonFileSource $ "config/app." ++ env ++ ".json"
    envSource
    argsSource

getConfigDefault :: IO Config
getConfigDefault = getConfig defaultReader

getConfig :: EnvReader () -> IO Config
getConfig reader = do
    (_, config) <- runStateT reader M.empty
    return config
