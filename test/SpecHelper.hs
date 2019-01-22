{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module SpecHelper (
    module SpecHelper,
    module Fixtures.EnvData
) where

import Test.Hspec
import System.Environment (setEnv, unsetEnv)
import System.Environment.Config
import Control.Exception (bracket_)
import Control.Monad (unless)
import Control.Monad.State (execStateT)
import GHC.Generics (Generic)
import Fixtures.EnvData
import Data.Aeson ((.:))
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Set as S
import qualified Data.HashMap.Strict as H

jsonFixtureReader :: EnvReader ()
jsonFixtureReader = jsonFileReader "test/Fixtures/config.json"

yamlFixtureReader :: EnvReader ()
yamlFixtureReader = yamlFileReader "test/Fixtures/config.yaml"

iniFixtureReader :: EnvReader ()
iniFixtureReader = iniFileReader "test/Fixtures/config.ini"

xmlFixtureReader :: EnvReader ()
xmlFixtureReader = xmlFileReader "test/Fixtures/config.xml"

setEnvVars :: [(String, String)] -> IO ()
setEnvVars = mapM_ $ uncurry setEnv

unsetEnvVars :: [(String, String)] -> IO ()
unsetEnvVars = mapM_ $ unsetEnv . fst

withEnv :: [(String, String)] -> IO a -> IO a
withEnv env = bracket_ (setEnvVars env) (unsetEnvVars env)

withStubEnv :: IO a -> IO a
withStubEnv = withEnv stubEnv

shouldHaveKeys :: (Show k, Ord k) => H.HashMap k v -> [k] -> Expectation
actual `shouldHaveKeys` expected = let
        s1 = S.fromList expected
        s2 = S.fromList $ H.keys actual
        errMsg = show s1 ++ " should be a subset of " ++ show s2
    in unless (s1 `S.isSubsetOf` s2) $ expectationFailure errMsg

shouldNotHaveKeys :: (Show k, Ord k) => H.HashMap k v -> [k] -> Expectation
actual `shouldNotHaveKeys` expected = let
        s1 = S.fromList expected
        s2 = S.fromList $ H.keys actual
        errMsg = show s1 ++ " should be disjoint of " ++ show s2
    in unless (s1 `S.disjoint` s2) $ expectationFailure errMsg

shouldHaveExactKeys :: (Show k, Ord k) => H.HashMap k v -> [k] -> Expectation
actual `shouldHaveExactKeys` expected =
    H.keys actual `shouldMatchList` expected

checkKeys :: EnvReader () -> [String] -> Expectation
checkKeys reader keys = do
    config <- execStateT reader H.empty
    config `shouldHaveExactKeys` keys

baseFileKeys :: [String]
baseFileKeys = [
      "env"
    , "host"
    , "port"
    , "db.host"
    , "db.port"
    , "db.policies.timeout"
    , "vault.api_key"
    , "data.dirs[0]"
    , "data.dirs[1]"
    , "data.dirs[2]"
    , "key with spaces"
    ]

baseIniXmlKeys :: [String]
baseIniXmlKeys = [
      "env"
    , "host"
    , "port"
    , "db.host"
    , "db.port"
    , "db.policies.timeout"
    , "vault.api_key"
    ]

checkMerged :: [(String, Value)] -> EnvReader () -> [(String, Value)] -> Expectation
checkMerged previousConfig reader nextConfig = do
        config <- execStateT reader $ H.fromList previousConfig
        mapM_ (checkMergedValue config) nextConfig
    where
        checkMergedValue config (k, v) = H.lookup k config `shouldBe` Just v

newtype SecretData = SecretData { token :: T.Text } 
        deriving (Show, Generic)

newtype VaultData = VaultData { secretData :: SecretData } 
        deriving (Show, Generic)

data VaultResponse = VaultResponse { request_id :: T.Text, vaultData :: VaultData } 
        deriving (Show, Generic)

instance A.FromJSON SecretData where
        parseJSON = A.withObject "SecretData" $ \o -> SecretData
            <$> o .: "token"

instance A.FromJSON VaultData where
        parseJSON = A.withObject "VaultData" $ \o -> VaultData
            <$> o .: "data"

instance A.FromJSON VaultResponse where
        parseJSON = A.withObject "VaultResponse" $ \o -> VaultResponse
            <$> o .: "request_id" <*> o .: "data"

getVaultSecret :: IO (Maybe VaultResponse)
getVaultSecret = do
    mData <- A.decodeFileStrict' "test/Fixtures/vault.json"
    return (mData :: Maybe VaultResponse)
