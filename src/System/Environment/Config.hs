{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config (
      Value(..)
    , getConfig
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
import GHC.Generics
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Aeson as A

data Value = String T.Text
           | Number Scientific
           | Bool Bool
           | Null
             deriving (Eq, Read, Show, Generic)

type Config = M.Map String Value
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

toVal :: String -> Value
toVal v = if null v then Bool True else String (T.pack v)

filterEnv :: [String] -> [(String, String)] -> [(String, Value)]
filterEnv prefixes env = map normalizeEnvKey' $ filter keyMatchesPrefix env
    where
        normalizeEnvKey' (k, v) = (normalizeEnvKey k, toVal v)
        keyMatchesPrefix (k, _) = any ((`isPrefixOf` lcase k) . lcase) prefixes

argToPair :: String -> (String, Value)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = splitAtChar '=' arg
    where
        splitAtChar c str = let
                key = normalizeEnvKey $ takeWhile (/= c) str
                val = case dropWhile (/= c) str of
                    "" -> toVal ""
                    cs -> toVal $ tail cs
            in (key, val)

mapArgs :: [String] -> [(String, Value)]
mapArgs (a1:a2:args) | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args
                     | otherwise = argToPair a1 : mapArgs (a2:args)
    where wantsArg a = "--" `isPrefixOf` a && '=' `notElem` a
          isArg a = not ("--" `isPrefixOf` a) && '=' `notElem` a
mapArgs (arg:args) = argToPair arg : mapArgs args
mapArgs [] = []

-- instance A.FromJSON (String, Value) where

getEnvMap :: [String] -> IO [(String, Value)]
getEnvMap prefixes = getEnvironment >>= return . filterEnv prefixes

getArgMap :: IO [(String, Value)]
getArgMap = getArgs >>= return . mapArgs

-- env readers
jsonFileReader :: FilePath -> EnvReader ()
jsonFileReader path = StateT $ unifyConfig $ \config -> 
    return config
    -- mValue <- A.decodeFileStrict' path 
    -- return $ maybe config (M.fromList . mapJSONValue) (mValue :: Maybe A.Value)

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

getEnvName :: IO (Maybe Value)
getEnvName = let
        e = defaultEnvNameVar
    in do
        am <- getArgMap
        em <- getEnvMap defaultEnvPrefixFilter
        return $ lookup e am <|> lookup e em

maybeEnvFileReader :: Maybe Value -> EnvReader ()
maybeEnvFileReader (Just (String env)) = jsonFileReader $ "app." ++ T.unpack env ++ ".json"
maybeEnvFileReader Nothing = return ()
maybeEnvFileReader _ = return ()

defaultReader :: EnvReader ()
defaultReader = do
    jsonFileReader "app.json"
    liftIO getEnvName >>= maybeEnvFileReader
    envReader defaultEnvPrefixFilter
    argsReader

getConfigDefault :: IO Config
getConfigDefault = getConfig defaultReader

getConfig :: EnvReader () -> IO Config
getConfig reader = execStateT reader M.empty
