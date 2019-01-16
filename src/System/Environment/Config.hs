{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config (
      EnvReader
    , Value(..)
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
) where

import System.Environment (lookupEnv, getArgs, getEnvironment)
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf)
import Control.Applicative ((<|>))
import GHC.Generics
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Aeson.Types (typeMismatch)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.HashMap.Strict as H

-- TODO: test the performance impact of strictness here
data Value = String !T.Text
           | Double !Double
           | Integer !Integer
           | Bool !Bool
           | Null
             deriving (Eq, Read, Show, Generic)

type Config = M.Map String Value
type EnvReader = StateT Config IO

instance A.FromJSON Value where
    parseJSON s@(A.String _) = return $ mapJsonVal s
    parseJSON n@(A.Number _) = return $ mapJsonVal n
    parseJSON b@(A.Bool _) = return $ mapJsonVal b
    parseJSON n@A.Null = return $ mapJsonVal n
    parseJSON invalid = typeMismatch "Value" invalid

newtype FlatValueMap = FlatValueMap { runFlatValueMap :: [(String, Value)] }
    deriving (Eq, Read, Show)

instance A.FromJSON FlatValueMap where
    parseJSON o@(A.Object _) = return $ FlatValueMap $ flatten "" (Right "") o []
    parseJSON a@(A.Array _) = return $ FlatValueMap $ flatten "" (Right "") a []
    parseJSON invalid = typeMismatch "[(String, Value)]" invalid

mapJsonVal :: A.Value -> Value
mapJsonVal (A.String s) = String s
mapJsonVal (A.Number n) = case floatingOrInteger n of
    Left r -> Double r
    Right i -> Integer i
mapJsonVal (A.Bool b) = Bool b
mapJsonVal A.Null = Null
mapJsonVal _ = error "use parseJSON to convert aeson arrays and objects"

vmToConfig :: FlatValueMap -> Config
vmToConfig = M.fromList . runFlatValueMap

flatten :: String -> Either Int T.Text -> A.Value -> [(String, Value)] -> [(String, Value)]
flatten path key val acc = case val of
        A.Object o -> H.foldrWithKey (flatten (dot path key) . Right) acc o
        A.Array a -> V.ifoldr (flatten (dot path key) . Left) acc a
        v -> (lcase $ dot path key, mapJsonVal v) : acc
    where 
        dot "" (Right k) = T.unpack k
        dot p (Left k) = p ++ '[' : show k ++ "]"
        dot p (Right k) = p ++ '.' : T.unpack k

unifyConfig :: (Config -> IO Config) -> Config -> IO ((), Config)
unifyConfig f c1 = do
    c2 <- f c1
    return ((), M.union c2 c1)

normalizeKey :: String -> String
normalizeKey ('_':'_':cs) = '.' : normalizeKey (dropWhile (== '_') cs)
normalizeKey (c:cs) = toLower c : normalizeKey cs
normalizeKey [] = []

toVal :: String -> Value
toVal v = if null v then Bool True else String (T.pack v)

lcase :: String -> String
lcase = map toLower

filterEnv :: [String] -> [(String, String)] -> [(String, Value)]
filterEnv prefixes env = map normalizeKey' $ filter keyMatchesPrefix env
    where
        normalizeKey' (k, v) = (normalizeKey k, toVal v)
        keyMatchesPrefix (k, _) = any ((`isPrefixOf` lcase k) . lcase) prefixes

argToPair :: String -> (String, Value)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = splitAtChar '=' arg
    where
        splitAtChar c str = let
                key = normalizeKey $ takeWhile (/= c) str
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

parseVal :: String -> Value
parseVal v | lcase v == "true" = Bool True
           | lcase v == "false" = Bool False
           | otherwise = maybe (String $ T.pack v) fromJust $ tryInteger v <|> tryDouble v
    where 
        tryInteger v = (readMaybe v :: Maybe Integer) >>= \v -> return $ Just (Integer v)
        tryDouble v = (readMaybe v :: Maybe Double) >>= \v -> return $ Just (Double v)

mapIniToConfig :: I.Ini -> [(String, Value)]
mapIniToConfig ini = let globals = toPair <$> I.iniGlobals ini
                     in H.foldrWithKey flattenIni globals $ I.iniSections ini
    where
        toPair (k, v) = (lcase $ T.unpack k, parseVal $ T.unpack v)
        dot p (k, v) = (T.concat [p, ".", k], v)
        flattenIni section pairs acc = foldr ((:) . toPair . dot section) acc pairs

mapXmlToConfig :: X.Node -> [(String, Value)]
mapXmlToConfig node = let attrPairs = flattenAttrs "" (X.attributes node) []
                      in flattenContents "" (X.contents node) attrPairs
    where 
        dot "" k = k
        dot p k = B.concat [p, ".", k]
        flattenNode path node acc = let
                path' = dot path (X.name node)
                attrPairs = flattenAttrs path' (X.attributes node) acc
            in flattenContents path' (X.contents node) attrPairs
        flattenAttrs path attrs acc = foldr (flattenAttr path) acc attrs
        flattenAttr path (k, v) acc = (lcase $ B.unpack $ dot path k, parseVal $ B.unpack v) : acc
        flattenContents path contents acc = foldr (flattenContent path) acc contents
        flattenContent path content acc = case content of
            X.Element node -> flattenNode path node acc
            X.Text text -> if B.all isSpace text then acc
                else flattenAttr "" (path, text) acc
            X.CData cdata -> flattenAttr "" (path, cdata) acc

getEnvMap :: [String] -> IO [(String, Value)]
getEnvMap prefixes = getEnvironment >>= return . filterEnv prefixes

getArgMap :: IO [(String, Value)]
getArgMap = getArgs >>= return . mapArgs

-- env readers
remoteReader :: (Config -> IO Config) -> EnvReader ()
remoteReader = StateT . unifyConfig

jsonFileReader :: FilePath -> EnvReader ()
jsonFileReader path = remoteReader $ \config -> do
    mValue <- A.decodeFileStrict' path 
    return $ maybe config vmToConfig (mValue :: Maybe FlatValueMap)

yamlFileReader :: FilePath -> EnvReader ()
yamlFileReader path = remoteReader $ \config -> do
    eValue <- YL.decodeFileEither path
    return $ case (eValue :: Either YL.ParseException FlatValueMap) of 
        Right vm -> vmToConfig vm
        Left _ -> config -- for now we'll ignore exceptions, TODO: debate error handling API

xmlFileReader :: FilePath -> EnvReader ()
xmlFileReader path = remoteReader $ \config -> do
    eNode <- B.readFile path >>= return . X.parse
    return $ case eNode of 
        Right node -> M.fromList $ mapXmlToConfig node
        Left _ -> config -- again, ignoring exceptions

iniFileReader :: FilePath -> EnvReader ()
iniFileReader path = remoteReader $ \config -> do
    eIni <- I.readIniFile path
    return $ case eIni of
        Right ini -> M.fromList $ mapIniToConfig ini
        Left _ -> config -- again, ignoring exceptions

envReader :: [String] -> EnvReader ()
envReader prefixes = remoteReader $ \config -> do
    envMap <- getEnvMap prefixes
    return $ M.fromList envMap

argsReader :: EnvReader ()
argsReader = remoteReader $ \config -> do
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
