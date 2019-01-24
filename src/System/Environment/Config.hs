{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config (
      FlatConfigMap(..)
    , EnvPairs(..)
    , Value(..)
    , EnvReader
    , FromJSON
    , ToJSON
    , getConfig
    , jsonFileReader
    , yamlFileReader
    , xmlFileReader
    , iniFileReader
    , remoteReader
    , envReader
    , argsReader
    , getArgPairs
    , getEnvPairs
    , get
    , getMaybe
    , getEither
) where

import System.Directory (getPermissions, Permissions, readable)
import System.Environment (lookupEnv, getArgs, getEnvironment)
import Control.Exception (try, IOException)
import Control.Monad (when)
import Control.Applicative ((<|>))
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Control.Monad.Fail (MonadFail)
import GHC.Generics
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.Text.Encoding (decodeUtf8)
import qualified Control.Monad.State as S (get)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.HashMap.Strict as H

newtype FlatConfigMap = FlatConfigMap { unFlatConfigMap :: H.HashMap String Value }
    deriving (Show, Generic)

getEither :: FromJSON a => String -> FlatConfigMap -> Either String a
getEither k m = case H.lookup k $ unFlatConfigMap m of
    Just v -> case A.fromJSON v of
        A.Success a -> Right a
        A.Error e -> Left e
    _ -> Left $ "key " ++ k ++ " not found in configuration"

getMaybe :: FromJSON a => String -> FlatConfigMap -> Maybe a
getMaybe k m = case getEither k m of
    Right v -> Just v
    Left _ -> Nothing

get :: forall a m. (MonadFail m, FromJSON a) => String -> FlatConfigMap -> m a
get k m = case getEither k m of
    Right v -> return v
    Left e -> fail e

newtype EnvPairs = EnvPairs { unEnvPairs :: [(String, Value)] }

instance Semigroup FlatConfigMap where
    (<>) (FlatConfigMap c2) (FlatConfigMap c1) = FlatConfigMap $ H.unionWith pickVal c2 c1
        where 
            pickVal Null v1 = v1
            pickVal v2 _ = v2

instance Monoid FlatConfigMap where
    mempty = FlatConfigMap H.empty            
    mappend = (<>)

instance FromJSON FlatConfigMap where
    parseJSON o@(Object _) = return $ FlatConfigMap $ H.fromList $ flatten "" (Right "") o []
    parseJSON a@(Array _) = return $ FlatConfigMap $ H.fromList $ flatten "" (Right "") a []
    parseJSON invalid = typeMismatch "FlatConfigMap" invalid

instance ToJSON X.Node where
    toJSON node = A.object [
            ("name", String $ decodeUtf8 $ X.name node),
            ("attrs", A.object $ toPair <$> X.attributes node),
            ("contents", A.toJSONList $ X.contents node)]
        where toPair (k, v) = (decodeUtf8 k, String $ decodeUtf8 v)

instance ToJSON X.Content where
    toJSON (X.Element node) = A.toJSON node
    toJSON (X.Text text) = String $ decodeUtf8 text
    toJSON (X.CData cdata) = String $ decodeUtf8 cdata

instance ToJSON EnvPairs where
    toJSON = A.object . map packKey . unEnvPairs
        where packKey (k, v) = (T.pack k, v)

instance ToJSON I.Ini where
    toJSON ini = A.object [
            ("globals", A.object $ toPair <$> I.iniGlobals ini),
            ("sections", A.object $ H.foldrWithKey toObj [] $ I.iniSections ini)]
        where toPair (k, v) = (k, String v)
              toObj section pairs acc = (section, A.object $ toPair <$> pairs) : acc
        
type EnvReader s = StateT s IO s

flatten :: String -> Either Int T.Text -> Value -> [(String, Value)] -> [(String, Value)]
flatten path key val acc = case val of
        Object o -> H.foldrWithKey (flatten (dot path key) . Right) acc o
        Array a -> V.ifoldr (flatten (dot path key) . Left) acc a
        v -> (lcase $ dot path key, v) : acc
    where 
        dot "" (Right k) = T.unpack k
        dot p (Left k) = p ++ '[' : show k ++ "]"
        dot p (Right k) = p ++ '.' : T.unpack k

normalizeKey :: String -> String
normalizeKey ('_':'_':cs) = '.' : normalizeKey (dropWhile (== '_') cs)
normalizeKey (c:cs) = toLower c : normalizeKey cs
normalizeKey [] = []

toVal :: String -> Value
toVal v = String (T.pack v)

toConfig :: (ToJSON a, FromJSON b) => b -> a -> b
toConfig def a = case A.fromJSON $ A.toJSON a of
    A.Success b -> b
    _ -> def

lcase :: String -> String
lcase = map toLower

filterEnv :: [String] -> [(String, String)] -> [(String, Value)]
filterEnv prefixes env = map envPair $ filter keyMatchesPrefix env
    where
        keyMatchesPrefix (k, _) = any ((`isPrefixOf` lcase k) . tailIfTilde . lcase) prefixes
        tailIfTilde ('~':cs) = cs
        tailIfTilde cs = cs
        envPair (k, v) = let    
                isLongestMatched lk ('~':p) Nothing  | lcase p `isPrefixOf` lk = Just p
                                                     | otherwise = Nothing
                isLongestMatched lk ('~':p) (Just l) | lcase p `isPrefixOf` lk && length p > length l = Just p
                                                     | otherwise = Just l
                isLongestMatched lk _ l = l
                stripLongestPrefix lk = fromMaybe lk $ 
                    foldr (isLongestMatched lk) Nothing prefixes >>= \p ->
                        stripPrefix (lcase p) lk
            in (normalizeKey $ stripLongestPrefix $ lcase k, toVal v)

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

getEnvPairs :: [String] -> IO EnvPairs
getEnvPairs prefixes = getEnvironment >>= return . EnvPairs . filterEnv prefixes

getArgPairs :: IO EnvPairs
getArgPairs = getArgs >>= return . EnvPairs . mapArgs

unifyConfig :: (FromJSON a, Monoid a) => a -> EnvReader a
unifyConfig c2 = StateT $ \c1 -> let c = c2 <> c1 in return (c, c)

remoteReader :: (FromJSON a, Monoid a) => (a -> IO a) -> EnvReader a
remoteReader f = do
    c1 <- S.get
    c2 <- liftIO $ f c1
    unifyConfig c2

whenReadable :: (FromJSON a, Monoid a) => FilePath -> (a -> IO a) -> EnvReader a
whenReadable path action = do
    ePerms <- liftIO $ try $ getPermissions path
    case (ePerms :: Either IOException Permissions) of
        Right perms -> if readable perms then remoteReader action
                       else S.get
        _ -> S.get

jsonFileReader :: (FromJSON a, Monoid a) => FilePath -> EnvReader a
jsonFileReader path = whenReadable path $ \config -> do
    eValue <- A.eitherDecodeFileStrict' path 
    return $ fromRight config eValue -- TODO: error handling

yamlFileReader :: (FromJSON a, Monoid a) => FilePath -> EnvReader a
yamlFileReader path = whenReadable path $ \config -> do
    eValue <- YL.decodeFileEither path
    return $ fromRight config eValue -- TODO: error handling

xmlFileReader :: (FromJSON a, Monoid a) => FilePath -> EnvReader a
xmlFileReader path = whenReadable path $ \config -> do
    eNode <- B.readFile path >>= return . X.parse
    return $ either (const config) (toConfig config) eNode 

iniFileReader :: (FromJSON a, Monoid a) => FilePath -> EnvReader a
iniFileReader path = whenReadable path $ \config -> do
    eIni <- I.readIniFile path
    return $ either (const config) (toConfig config) eIni

envReader :: (FromJSON a, Monoid a) => [String] -> EnvReader a
envReader prefixes = remoteReader $ \config -> do
    envPairs <- getEnvPairs prefixes
    return $ toConfig config envPairs

argsReader :: (FromJSON a, Monoid a) => EnvReader a
argsReader = remoteReader $ \config -> do
    argPairs <- getArgPairs
    return $ toConfig config argPairs

getConfig :: (FromJSON a, Monoid a) => EnvReader a -> IO a
getConfig reader = execStateT reader mempty
