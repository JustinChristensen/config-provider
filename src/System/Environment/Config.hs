{-# LANGUAGE LambdaCase #-}
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
    , getE
) where

import System.Directory (getPermissions, Permissions, readable)
import System.Environment (getArgs, getEnvironment)
import Control.Exception (try, IOException)
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Control.Monad.Fail (MonadFail)
import GHC.Generics
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch)
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

newtype EnvPairs = EnvPairs { unEnvPairs :: [(String, Value)] }
newtype Node = Node X.Node
newtype Content = Content X.Content
newtype Ini = Ini I.Ini

newtype FlatConfigMap = FlatConfigMap { unFlatConfigMap :: H.HashMap String Value }
    deriving (Show, Eq, Generic)

getE :: FromJSON a => String -> FlatConfigMap -> Either String a
getE k m = case H.lookup k $ unFlatConfigMap m of
    Just v -> case A.fromJSON v of
        A.Success a -> Right a
        A.Error e -> Left e
    _ -> Left $ "key " ++ k ++ " not found in configuration"

get :: forall a m. (MonadFail m, FromJSON a) => String -> FlatConfigMap -> m a
get k m = case getE k m of
    Right v -> return v
    Left e -> fail $ k ++ ", " ++ e

flatConfigMap :: Value -> FlatConfigMap
flatConfigMap v = FlatConfigMap $ H.fromList $ flatten "" (Right "") v []

instance Semigroup FlatConfigMap where
    (<>) (FlatConfigMap c2) (FlatConfigMap c1) = FlatConfigMap $ H.unionWith pickVal c2 c1
        where
            pickVal Null v1 = v1
            pickVal v2 _ = v2

instance Monoid FlatConfigMap where
    mempty = FlatConfigMap H.empty
    mappend = (<>)

instance FromJSON FlatConfigMap where
    parseJSON o@(Object _) = return $ flatConfigMap o
    parseJSON a@(Array _) = return $ flatConfigMap a
    parseJSON invalid = typeMismatch "FlatConfigMap" invalid

nodeArray :: Node -> Value
nodeArray (Node node) = let contents = A.toJSON <$> (Content <$> skipWhitespace (X.contents node))
                            attrs = A.object $ toAttrPair <$> X.attributes node
                        in toArr $ contents ++ [attrs]
    where toArr = Array . V.fromList
          toAttrPair (k, v) = (decodeUtf8 k, toVal $ Right v)
          skipWhitespace = filter (\case
              X.Text text -> B.all (not . isSpace) text
              _ -> True)

instance ToJSON Node where
    toJSON n@(Node node) = A.object [(decodeUtf8 $ X.name node, nodeArray n)]

instance ToJSON Content where
    toJSON (Content (X.Element node)) = A.toJSON (Node node)
    toJSON (Content (X.Text text)) = toVal $ Right text
    toJSON (Content (X.CData cdata)) = String $ decodeUtf8 cdata

instance ToJSON EnvPairs where
    toJSON = A.object . map packKey . unEnvPairs
        where packKey (k, v) = (T.pack k, v)

instance ToJSON Ini where
    toJSON (Ini ini) = let globals = toPair <$> I.iniGlobals ini
                           sections = H.foldrWithKey toObj [] $ I.iniSections ini
                       in A.object $ sections ++ globals
        where toPair (k, v) = (k, toVal $ Left $ T.unpack v)
              toObj section pairs acc = (section, A.object $ toPair <$> pairs) : acc

type EnvReader s = StateT s IO s

dot :: String -> Either Int T.Text -> String
dot "" (Right k) = T.unpack k
dot p (Left k) = p ++ '[' : show k ++ "]"
dot p (Right k) = p ++ '.' : T.unpack k

flatten :: String -> Either Int T.Text -> Value -> [(String, Value)] -> [(String, Value)]
flatten path key val acc = case val of
    Object o -> H.foldrWithKey (flatten (dot path key) . Right) acc o
    Array a -> flattenArray path key a acc
    v -> (lcase $ dot path key, v) : acc

-- TODO: define merging behavior for arrays
flattenArray :: String -> Either Int T.Text -> A.Array -> [(String, Value)] -> [(String, Value)]
flattenArray path key arr acc = if hasObjects arr then
            V.foldr (flatten path key) acc arr
        else 
            V.ifoldr (flatten (dot path key) . Left) acc arr
    where hasObjects = V.any isObject
          isObject (Object _) = True
          isObject _ = False

normalizeKey :: String -> String
normalizeKey ('_':'_':cs) = '.' : normalizeKey (dropWhile (== '_') cs)
normalizeKey (c:cs) = toLower c : normalizeKey cs
normalizeKey [] = []

toVal :: Either String B.ByteString -> Value
toVal (Left v) = toVal $ Right $ B.pack v
toVal (Right v) = fromMaybe (String $ decodeUtf8 v) $ A.decodeStrict' v

toConfigWith :: (ToJSON a, FromJSON b) => (a -> Value) -> b -> a -> b
toConfigWith fn def a = case A.fromJSON $ fn a of
    A.Success b -> b
    _ -> def

toConfig :: (ToJSON a, FromJSON b) => b -> a -> b
toConfig = toConfigWith A.toJSON

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
                isLongestMatched _ _ l = l
                stripLongestPrefix lk = fromMaybe lk $
                    foldr (isLongestMatched lk) Nothing prefixes >>= \p ->
                        stripPrefix (lcase p) lk
            in (normalizeKey $ stripLongestPrefix $ lcase k, toVal $ Left v)

argToPair :: String -> (String, Value)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = splitAtChar '=' arg
    where
        splitAtChar c str = let
                key = normalizeKey $ takeWhile (/= c) str
                val = case dropWhile (/= c) str of
                    "" -> toVal $ Left ""
                    cs -> toVal $ Left $ tail cs
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

unifyConfig :: (FromJSON a, Semigroup a) => a -> EnvReader a
unifyConfig c2 = StateT $ \c1 -> let c = c2 <> c1 in return (c, c)

remoteReader :: (FromJSON a, Semigroup a) => (a -> IO a) -> EnvReader a
remoteReader f = do
    c1 <- S.get
    c2 <- liftIO $ f c1
    unifyConfig c2

whenReadable :: (FromJSON a, Semigroup a) => FilePath -> (a -> IO a) -> EnvReader a
whenReadable path action = do
    ePerms <- liftIO $ try $ getPermissions path
    case (ePerms :: Either IOException Permissions) of
        Right perms -> if readable perms then remoteReader action
                       else S.get
        _ -> S.get

jsonFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
jsonFileReader path = whenReadable path $ \config -> do
    eValue <- A.eitherDecodeFileStrict' path
    return $ fromRight config eValue -- TODO: error handling

yamlFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
yamlFileReader path = whenReadable path $ \config -> do
    eValue <- YL.decodeFileEither path
    return $ fromRight config eValue -- TODO: error handling

xmlFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
xmlFileReader path = whenReadable path $ \config -> do
    eNode <- B.readFile path >>= return . X.parse
    return $ either (const config) (toConfigWith nodeArray config) (Node <$> eNode)

iniFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
iniFileReader path = whenReadable path $ \config -> do
    eIni <- I.readIniFile path
    return $ either (const config) (toConfig config) (Ini <$> eIni)

envReader :: (FromJSON a, Semigroup a) => [String] -> EnvReader a
envReader prefixes = remoteReader $ \config -> do
    envPairs <- getEnvPairs prefixes
    return $ toConfig config envPairs

argsReader :: (FromJSON a, Semigroup a) => EnvReader a
argsReader = remoteReader $ \config -> do
    argPairs <- getArgPairs
    return $ toConfig config argPairs

getConfig :: (FromJSON a, Monoid a) => EnvReader a -> IO a
getConfig reader = execStateT reader mempty
