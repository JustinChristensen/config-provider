{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.Reader where

import System.Environment.Config.Types
import System.Environment.Config.Base
import System.Environment (getArgs, getEnvironment)
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import System.IO.Error (isDoesNotExistError)
import Control.Monad.State (StateT(..), execStateT, liftIO)
import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Aeson (FromJSON)
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

filterEnv :: [String] -> [(String, String)] -> [(String, String)]
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
            in (normalizeKey $ stripLongestPrefix $ lcase k, v)

argToPair :: String -> (String, String)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = let (k, v) = splitAtEl '=' arg 
                in (normalizeKey k, v)

mapArgs :: [String] -> [(String, String)]
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

makeThrow :: (MonadThrow m, Exception e) => m (Either e Config) -> m Config
makeThrow source = source >>= either throwM return

makeThrowF :: (MonadThrow m, Exception e) => (FilePath -> m (Either e Config)) -> FilePath -> m Config
makeThrowF source path = makeThrow $ source path

makeOptional :: (MonadCatch m, MonadIO m) => (FilePath -> m Config) -> Config -> FilePath -> m Config
makeOptional source c1 path = catchIf isDoesNotExistError (source path) (const $ return c1)

makeEnvReader :: (Config -> IO Config) -> EnvReader Config
makeEnvReader source = StateT $ \c1 -> do
    c2 <- source c1 
    return ((), c2 <> c1)

remoteReader :: (Config -> IO Config) -> EnvReader Config
remoteReader = makeEnvReader

jsonFileE :: MonadIO m => FilePath -> m (Either JsonSourceException Config)
jsonFileE path = liftIO $ A.eitherDecodeFileStrict' @Value path >>= 
    return . either (Left . AesonError) (return . toConfig)

jsonFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
jsonFile = makeThrowF jsonFileE

optionalJsonFile :: (MonadCatch m, MonadIO m) => Config -> FilePath -> m Config
optionalJsonFile = makeOptional jsonFile

jsonFileReader :: FilePath -> EnvReader Config
jsonFileReader path = makeEnvReader $ const $ jsonFile path

optionalJsonFileReader :: FilePath -> EnvReader Config
optionalJsonFileReader path = makeEnvReader $ \c -> optionalJsonFile c path

yamlFileE :: MonadIO m => FilePath -> m (Either YamlSourceException Config)
yamlFileE path = liftIO $ YL.decodeFileEither @Value path >>= 
    return . either (Left . YamlError) (return . toConfig)

yamlFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
yamlFile = makeThrowF yamlFileE

optionalYamlFile :: (MonadCatch m, MonadIO m) => Config -> FilePath -> m Config
optionalYamlFile = makeOptional yamlFile

yamlFileReader :: FilePath -> EnvReader Config
yamlFileReader path = makeEnvReader $ const $ jsonFile path

optionalYamlFileReader :: FilePath -> EnvReader Config
optionalYamlFileReader path = makeEnvReader $ \c -> optionalYamlFile c path

xmlFileE :: MonadIO m => FilePath -> m (Either XmlSourceException Config)
xmlFileE path = liftIO $ B.readFile path >>= return . X.parse >>= 
    return . either (Left . XenoError) (return . nodeConfig)

xmlFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
xmlFile = makeThrowF xmlFileE

optionalXmlFile :: (MonadCatch m, MonadIO m) => Config -> FilePath -> m Config
optionalXmlFile = makeOptional xmlFile

xmlFileReader :: FilePath -> EnvReader Config
xmlFileReader path = makeEnvReader $ const $ xmlFile path

optionalXmlFileReader :: FilePath -> EnvReader Config
optionalXmlFileReader path = makeEnvReader $ \c -> optionalXmlFile c path

iniFileE :: MonadIO m => FilePath -> m (Either IniSourceException Config)
iniFileE path = liftIO $ I.readIniFile path >>=
    return . either (Left . IniError) (return . toConfig)

iniFile :: (MonadThrow m, MonadIO m) => FilePath -> m Config
iniFile = makeThrowF iniFileE

optionalIniFile :: (MonadCatch m, MonadIO m) => Config -> FilePath -> m Config
optionalIniFile = makeOptional iniFile

iniFileReader :: FilePath -> EnvReader Config
iniFileReader path = makeEnvReader $ const $ iniFile path

optionalIniFileReader :: FilePath -> EnvReader Config
optionalIniFileReader path = makeEnvReader $ \c -> optionalIniFile c path

envSource :: MonadIO m => [String] -> m Config
envSource prefixes = do
    envPairs <- liftIO $ getEnvPairs prefixes
    return $ toConfig envPairs

envReader :: [String] -> EnvReader Config
envReader prefixes = makeEnvReader $ const $ envSource prefixes

argsSource :: MonadIO m => m Config
argsSource = do
    argPairs <- liftIO getArgPairs 
    return $ toConfig argPairs

argsReader :: EnvReader Config
argsReader = makeEnvReader $ const argsSource

getConfig :: forall a m. (MonadIO m, FromJSON a) => EnvReader Config -> m a
getConfig reader = liftIO $ execStateT reader mempty >>= bind ""