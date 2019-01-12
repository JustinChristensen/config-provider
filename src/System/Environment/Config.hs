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
    , unifyConfig
    , mapArgs
    , argToPair
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

normalizeEnvKey :: String -> String
normalizeEnvKey ('_':'_':cs) = '.' : normalizeEnvKey (dropWhile (== '_') cs)
normalizeEnvKey (c:cs) = toLower c : normalizeEnvKey cs
normalizeEnvKey [] = []

filterEnv :: [(String, String)] -> [String] -> [(String, String)]
filterEnv env prefixes = map normalizeEnvKey' $ filter keyMatchesPrefix env
    where
        normalizeEnvKey' (k, v) = (normalizeEnvKey k, v)
        keyMatchesPrefix (k, _) = any (`isPrefixOf` k) prefixes

argToPair :: String -> (String, String)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = splitAtChar '=' arg
    where 
        splitAtChar c str = let 
                key = normalizeEnvKey $ takeWhile (/= c) str
                val = case dropWhile (/= c) str of
                    "" -> ""
                    cs -> tail cs
            in (key, val)

mapArgs :: [String] -> [(String, String)]
mapArgs (a1:a2:args) | isFull a1 || isFull a2 = argToPair a1 : mapArgs (a2:args)             
                     | otherwise = argToPair (a1 ++ "=" ++ a2) : mapArgs args
    where isFull a = '=' `elem` a || not ("--" `isPrefixOf` a)
mapArgs (arg:args) = argToPair arg : mapArgs args
mapArgs [] = []

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
argsReader = StateT $ unifyConfig $ \config -> do
    args <- getArgs
    return $ M.fromList $ mapArgs args

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
