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
import Control.Applicative ((<|>))
import qualified Data.Map as M

type Config = M.Map String String
type EnvReader = StateT Config IO

lcase = map toLower

unifyConfig :: (Config -> IO Config) -> Config -> IO ((), Config)
unifyConfig f c1 = do
    c2 <- f c1
    return ((), M.union c2 c1)

normalizeEnvKey :: String -> String
normalizeEnvKey ('_':'_':cs) = '.' : normalizeEnvKey (dropWhile (== '_') cs)
normalizeEnvKey (c:cs) = toLower c : normalizeEnvKey cs
normalizeEnvKey [] = []

filterEnv :: [String] -> [(String, String)] -> [(String, String)]
filterEnv prefixes env = map normalizeEnvKey' $ filter keyMatchesPrefix env
    where
        normalizeEnvKey' (k, v) = (normalizeEnvKey k, v)
        keyMatchesPrefix (k, _) = any ((`isPrefixOf` lcase k) . lcase) prefixes

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
mapArgs (a1:a2:args) | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args
                     | otherwise = argToPair a1 : mapArgs (a2:args)
    where wantsArg a = "--" `isPrefixOf` a && '=' `notElem` a
          isArg a = not ("--" `isPrefixOf` a) && '=' `notElem` a
mapArgs (arg:args) = argToPair arg : mapArgs args
mapArgs [] = []

getEnvMap :: [String] -> IO [(String, String)]
getEnvMap prefixes = getEnvironment >>= return . filterEnv prefixes

getArgMap :: IO [(String, String)]
getArgMap = getArgs >>= return . mapArgs

-- env readers
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
    envMap <- getEnvMap prefixes
    return $ M.fromList envMap

argsReader :: EnvReader ()
argsReader = StateT $ unifyConfig $ \config -> do
    argMap <- getArgMap
    return $ M.fromList argMap

defaultEnvNameVar :: String
defaultEnvNameVar = "env"

defaultEnvPrefixFilter :: [String]
defaultEnvPrefixFilter = [
      defaultEnvNameVar
    , "hs_"
    , "host"
    , "port"
    ]

getEnvName :: IO (Maybe String)
getEnvName = let
        e = defaultEnvNameVar
    in do
        am <- getArgMap
        em <- getEnvMap defaultEnvPrefixFilter
        return $ lookup e am <|> lookup e em

defaultReader :: EnvReader ()
defaultReader = do
    jsonFileReader "app.json"
    do
        mEnv <- liftIO getEnvName
        case mEnv of
            Just env -> jsonFileReader $ "app." ++ env ++ ".json"
            _ -> return ()
    envReader defaultEnvPrefixFilter
    argsReader

getConfigDefault :: IO Config
getConfigDefault = getConfig defaultReader

getConfig :: EnvReader () -> IO Config
getConfig reader = execStateT reader M.empty
