{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.Reader where

import System.Environment.Config.Types
import System.Environment.Config.Helpers
import System.Environment (getArgs, getEnvironment)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Control.Exception (throwIO)
import System.IO.Error (isDoesNotExistError)
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Either (either)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as YL

normalizeKey :: String -> String
normalizeKey ('_':'_':cs) = '.' : normalizeKey (dropWhile (== '_') cs)
normalizeKey (c:cs) = toLower c : normalizeKey cs
normalizeKey [] = []

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
argToPair arg = let (k, v) = splitAtEl '=' arg 
                in (normalizeKey k, toVal $ Left v)

mapArgs :: [String] -> [(String, Value)]
mapArgs (a1:a2:args') | a1 == "--" = mapArgs (a2:args')
                      | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args'
                      | otherwise = argToPair a1 : mapArgs (a2:args')
    where wantsArg ('-':'-':a) = '=' `notElem` a
          wantsArg _ = False
          isArg ('-':'-':_) = False 
          isArg a = '=' `notElem` a
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

makeEnvReader :: (FromJSON a, Mergeable a) => (a -> IO a) -> EnvReader a
makeEnvReader source = StateT $ \c1 -> do
    c2 <- source c1 
    return ((), c2 `merge` c1)

remoteReader :: (FromJSON a, Mergeable a) => (a -> IO a) -> EnvReader a
remoteReader = makeEnvReader

jsonFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
jsonFileE path = liftIO $ A.eitherDecodeFileStrict' path >>= 
    return . either (Left . AesonError) return

jsonFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
jsonFile = makeThrow jsonFileE

optionalJsonFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalJsonFile = makeOptional jsonFile

jsonFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
jsonFileReader path = makeEnvReader $ const $ jsonFile path

optionalJsonFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
optionalJsonFileReader path = makeEnvReader $ \c -> optionalJsonFile c path

yamlFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
yamlFileE path = liftIO $ YL.decodeFileEither path >>= 
    return . either (Left . YamlError) return

yamlFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
yamlFile = makeThrow yamlFileE

optionalYamlFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalYamlFile = makeOptional yamlFile

yamlFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
yamlFileReader path = makeEnvReader $ const $ jsonFile path

optionalYamlFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
optionalYamlFileReader path = makeEnvReader $ \c -> optionalYamlFile c path

xmlFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
xmlFileE path = liftIO $ B.readFile path >>= return . X.parse >>= 
    return . either (Left . XenoError) (fromSource . nodeList . Node)

xmlFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
xmlFile = makeThrow xmlFileE

optionalXmlFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalXmlFile = makeOptional xmlFile

xmlFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
xmlFileReader path = makeEnvReader $ const $ xmlFile path

optionalXmlFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
optionalXmlFileReader path = makeEnvReader $ \c -> optionalXmlFile c path

iniFileE :: (MonadIO m, FromJSON a) => FilePath -> m (Either ConfigSourceException a)
iniFileE path = liftIO $ I.readIniFile path >>=
    return . either (Left . IniError) (fromSource . Ini)

iniFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
iniFile = makeThrow iniFileE

optionalIniFile :: (MonadCatch m, MonadIO m, FromJSON a) => a -> FilePath -> m a
optionalIniFile = makeOptional iniFile

iniFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
iniFileReader path = makeEnvReader $ const $ iniFile path

optionalIniFileReader :: (FromJSON a, Mergeable a) => FilePath -> EnvReader a
optionalIniFileReader path = makeEnvReader $ \c -> optionalIniFile c path

envReader :: (FromJSON a, Mergeable a) => [String] -> EnvReader a
envReader prefixes = makeEnvReader $ const $ do
    envPairs <- getEnvPairs prefixes
    either (liftIO . throwIO) return $ fromSource envPairs

argsReader :: (FromJSON a, Mergeable a) => EnvReader a
argsReader = makeEnvReader $ const $ do
    argPairs <- getArgPairs
    either (liftIO . throwIO) return $ fromSource argPairs

getConfig :: (MonadIO m, FromJSON a, Mergeable a) => EnvReader a -> m a
getConfig reader = liftIO $ execStateT reader empty