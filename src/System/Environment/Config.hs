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
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower)
import Data.List (isPrefixOf)
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
    , "PORT"
    ]

getEnvName :: IO (Maybe String)
getEnvName = lookupEnv defaultEnvNameVar

unifyConfig :: (Config -> IO Config) -> Config -> IO ((), Config) 
unifyConfig f c1 = do
    c2 <- f c1
    return ((), M.union c2 c1)

normalizeEnvKey :: (String, String) -> (String, String)
normalizeEnvKey (k, v) = (normalize k, v) 
    where
        normalize ('_':xs) = '.' : normalize (dropWhile (== '_') xs)
        normalize (c:xs) = toLower c : normalize xs
        normalize [] = []

filterEnv :: [(String, String)] -> [String] -> [(String, String)]
filterEnv env prefixes = map normalizeEnvKey $ filter keyMatchesPrefix env
    where
        keyMatchesPrefix (k, _) = any (`isPrefixOf` k) prefixes

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
envReader prefixes = StateT $ unifyConfig $ \config -> do
    env <- getEnvironment
    return $ M.fromList $ filterEnv env prefixes

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
getConfig reader = execStateT reader M.empty
