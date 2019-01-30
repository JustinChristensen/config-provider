{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config (
      FlatConfigMap(..)
    , ConfigGetException(..)
    , EnvPairs(..)
    , Value(..)
    , EnvReader
    , FromJSON
    , ToJSON
    , jsonFileE
    , jsonFile
    , optionalJsonFile
    , jsonFileReader
    , optionalJsonFileReader
    , yamlFileE
    , yamlFile
    , optionalYamlFile
    , yamlFileReader
    , optionalYamlFileReader
    , xmlFileE
    , xmlFile
    , optionalXmlFile
    , xmlFileReader
    , optionalXmlFileReader
    , iniFileE
    , iniFile
    , optionalIniFile
    , iniFileReader
    , optionalIniFileReader
    , envReader
    , argsReader
    , remoteReader
    , getArgPairs
    , getEnvPairs
    , getConfig
    , get
    , getM
    , getE
) where

import GHC.Generics
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (throwIO)
import System.IO.Error (isDoesNotExistError)
import System.Environment (getArgs, getEnvironment)
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, stripPrefix)
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Aeson.Types (typeMismatch)
import Data.Maybe (fromMaybe)
import Data.Either (either)
import Data.Text.Encoding (decodeUtf8)
import Xeno.Types (XenoException)
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

type EnvReader s = StateT s IO ()

data ConfigSourceException = 
      AesonError String 
    | YamlError YL.ParseException
    | XenoError XenoException
    | IniError String
    deriving (Show)

data ConfigGetException = 
      KeyNotFoundError String 
    | ParseValueError String 
    deriving (Show)

instance Exception ConfigGetException where
    displayException e = case e of
        KeyNotFoundError k -> "key " ++ k ++ " not found in configuration"
        ParseValueError s -> s

instance Exception ConfigSourceException where
    displayException e = case e of
        AesonError s -> s
        YamlError e' -> displayException e'
        XenoError e' -> displayException e'
        IniError s -> s

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

getE:: FromJSON a => String -> FlatConfigMap -> Either ConfigGetException a
getE k m = case H.lookup k $ unFlatConfigMap m of
    Just v -> case A.fromJSON v of
        A.Success a -> Right a
        A.Error e -> Left $ ParseValueError e
    _ -> Left $ KeyNotFoundError k

getM :: FromJSON a => String -> FlatConfigMap -> Maybe a
getM = get

get :: forall a m. (MonadThrow m, FromJSON a) => String -> FlatConfigMap -> m a
get k m = either throwM return $ getE k m

flatConfigMap :: Value -> FlatConfigMap
flatConfigMap v = FlatConfigMap $ H.fromList $ flatten "" (Right "") v []

nodeArray :: Node -> Value
nodeArray (Node node) = let contents = A.toJSON <$> (Content <$> skipWhitespace (X.contents node))
                            attrs = A.object $ toAttrPair <$> X.attributes node
                        in toArr $ contents ++ [attrs]
    where toArr = Array . V.fromList
          toAttrPair (k, v) = (decodeUtf8 k, toVal $ Right v)
          skipWhitespace = filter (\case
              X.Text text -> B.all (not . isSpace) text
              _ -> True)

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

lcase :: String -> String
lcase = map toLower

filterEnv :: [String] -> [(String, String)] -> [(String, Value)]
filterEnv prefixes envVars = map envPair $ filter keyMatchesPrefix envVars
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
mapArgs (a1:a2:args') | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args'
                     | otherwise = argToPair a1 : mapArgs (a2:args')
    where wantsArg a = "--" `isPrefixOf` a && '=' `notElem` a
          isArg a = not ("--" `isPrefixOf` a) && '=' `notElem` a
mapArgs (arg:args') = argToPair arg : mapArgs args'
mapArgs [] = []

getEnvPairs :: [String] -> IO EnvPairs
getEnvPairs prefixes = getEnvironment >>= return . EnvPairs . filterEnv prefixes

getArgPairs :: IO EnvPairs
getArgPairs = getArgs >>= return . EnvPairs . mapArgs

fromSource :: (ToJSON a, FromJSON b) => a -> Either ConfigSourceException b
fromSource a = case A.fromJSON $ A.toJSON a of
    A.Success b -> Right b
    A.Error s -> Left $ AesonError s

makeThrow :: (MonadThrow m, FromJSON a) => (FilePath -> m (Either ConfigSourceException a)) -> FilePath -> m a
makeThrow source path = source path >>= either throwM return

makeOptional :: (MonadCatch m, MonadIO m, FromJSON a) => (FilePath -> m a) -> a -> FilePath -> m a
makeOptional source c1 path = catchIf isDoesNotExistError (source path) (const $ return c1)

makeEnvReader :: (FromJSON a, Semigroup a) => (a -> IO a) -> EnvReader a
makeEnvReader source = StateT $ \c1 -> do
    c2 <- source c1 
    return ((), c2 <> c1)

remoteReader :: (FromJSON a, Semigroup a) => (a -> IO a) -> EnvReader a
remoteReader = makeEnvReader

jsonFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
jsonFileE path = liftIO $ A.eitherDecodeFileStrict' path >>= 
    return . either (Left . AesonError) return

jsonFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
jsonFile = makeThrow jsonFileE

optionalJsonFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalJsonFile = makeOptional jsonFile

jsonFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
jsonFileReader path = makeEnvReader $ const $ jsonFile path

optionalJsonFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
optionalJsonFileReader path = makeEnvReader $ \c -> optionalJsonFile c path

yamlFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
yamlFileE path = liftIO $ YL.decodeFileEither path >>= 
    return . either (Left . YamlError) return

yamlFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
yamlFile = makeThrow yamlFileE

optionalYamlFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalYamlFile = makeOptional yamlFile

yamlFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
yamlFileReader path = makeEnvReader $ const $ jsonFile path

optionalYamlFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
optionalYamlFileReader path = makeEnvReader $ \c -> optionalYamlFile c path

xmlFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
xmlFileE path = liftIO $ B.readFile path >>= return . X.parse >>= 
    return . either (Left . XenoError) (fromSource . Node)

xmlFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
xmlFile = makeThrow xmlFileE

optionalXmlFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalXmlFile = makeOptional xmlFile

xmlFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
xmlFileReader path = makeEnvReader $ const $ xmlFile path

optionalXmlFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
optionalXmlFileReader path = makeEnvReader $ \c -> optionalXmlFile c path

iniFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
iniFileE path = liftIO $ I.readIniFile path >>=
    return . either (Left . IniError) (fromSource . Ini)

iniFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
iniFile = makeThrow iniFileE

optionalIniFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalIniFile = makeOptional iniFile

iniFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
iniFileReader path = makeEnvReader $ const $ iniFile path

optionalIniFileReader :: (FromJSON a, Semigroup a) => FilePath -> EnvReader a
optionalIniFileReader path = makeEnvReader $ \c -> optionalIniFile c path

envReader :: (FromJSON a, Semigroup a) => [String] -> EnvReader a
envReader prefixes = makeEnvReader $ const $ do
    envPairs <- getEnvPairs prefixes
    either (liftIO . throwIO) return $ fromSource envPairs

argsReader :: (FromJSON a, Semigroup a) => EnvReader a
argsReader = makeEnvReader $ const $ do
    argPairs <- getArgPairs
    either (liftIO . throwIO) return $ fromSource argPairs

getConfig :: (FromJSON a, Monoid a) => EnvReader a -> IO a
getConfig reader = execStateT reader mempty