{-# LANGUAGE OverloadedStrings #-}
module System.Environment.ConfigSpec (spec) where

import SpecHelper
import Test.Hspec
import System.Environment (withArgs)
import System.Environment.Config
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Control.Monad.State (execStateT)

-- WARNING: 
-- The tests herein are highly dependent on the state of the environment
-- In particular, they depend on the fixtures located in the Fixtures/ subdirectory of test
-- and on the environment variables and arguments passed while executing the test suite itself
-- Running the test executable with extra arguments like --env or environment variables like ENV
-- WILL lead to test failures. 

-- TODO: define exception handling behavior
-- TODO: define ADT binding API

spec :: Spec
spec = do
    describe "getEnvName" $
        it "should prefer args to env vars" $ do
            e1 <- withEnv [("ENV", "development")] getEnvName
            e2 <- withArgs ["--env", "production"] getEnvName
            e3 <- withArgs ["--ENV", "staging"] $ withEnv [("ENV", "development")] getEnvName
            e4 <- getEnvName
            e1 `shouldBe` Just (String "development")
            e2 `shouldBe` Just (String "production")
            e3 `shouldBe` Just (String "staging")
            e4 `shouldBe` Nothing

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.FileExtensions/src/FileConfigurationSource.cs
    describe "jsonFileReader" $ do
        it "should read configuration from a json file" $
            checkKeys jsonFixtureReader baseFileKeys
        it "should merge with the upstream configuration map" $ let
                previousConfig = [
                    ("host", String "0.0.0.0"),
                    ("db.host", String "db.i.foocorp.net"),
                    ("log_level", String "trace")]
                nextConfig = [
                    ("host", String "192.168.5.200"),
                    ("db.host", String "10.10.10.11"),
                    ("data.dirs[1]", String "tmp2"),
                    ("log_level", String "trace")]
            in checkMerged previousConfig jsonFixtureReader nextConfig

    describe "yamlFileReader" $ do
        it "should read configuration from a yaml file" $
            checkKeys yamlFixtureReader baseFileKeys
        it "should merge with the upstream configuration map" $ let
                previousConfig = [
                    ("host", String "0.0.0.0"),
                    ("db.host", String "db.i.foocorp.net"),
                    ("log_level", String "trace")]
                nextConfig = [
                    ("host", String "127.0.0.1"),
                    ("db.host", String "172.16.9.44"),
                    ("data.dirs[1]", String "tmp2"),
                    ("log_level", String "trace")]
            in checkMerged previousConfig yamlFixtureReader nextConfig

    describe "xmlFileReader" $ do
        it "should read configuration from an xml file" $
            checkKeys xmlFixtureReader baseIniXmlKeys
        it "should omit the root element from the path" $ do
            config <- execStateT xmlFixtureReader M.empty
            M.keys config `shouldNotSatisfy` any (isPrefixOf "application")
        it "should filter out whitespace-only text nodes" $ do
            config <- execStateT xmlFixtureReader M.empty
            M.lookup "extra" config `shouldBe` Nothing
        it "should merge with the upstream configuration map" $ let
                previousConfig = [
                    ("env", String "qa"),
                    ("db.host", String "db.i.foocorp.net"),
                    ("db.port", Integer 4747),
                    ("log_level", String "trace")]
                nextConfig = [
                    ("env", String "preprod"),
                    ("db.host", String "10.111.51.2"),
                    ("db.port", Integer 27000),
                    ("log_level", String "trace")]
            in checkMerged previousConfig xmlFixtureReader nextConfig

    describe "iniFileReader" $ do
        it "should read configuration from an ini file" $
            checkKeys iniFixtureReader ("key with spaces" : baseIniXmlKeys)
        it "should merge with the upstream configuration map" $ let
                previousConfig = [
                    ("env", String "qa"),
                    ("db.host", String "db.i.foocorp.net"),
                    ("log_level", String "trace")]
                nextConfig = [
                    ("env", String "staging6"),
                    ("db.host", String "172.7.7.7"),
                    ("log_level", String "trace")]
            in checkMerged previousConfig iniFixtureReader nextConfig

    -- describe "remoteReader" $ do
    --     it "should read configuration from a vault" pending
    --     it "should merge with the upstream configuration map" pending

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.EnvironmentVariables/src/EnvironmentVariablesConfigurationProvider.cs
    describe "envReader" $ do
        it "should read configuration from environment variables" $ do
            config <- withStubEnv $ execStateT (envReader ["HS_", "HOST", "PORT"]) M.empty
            config `shouldHaveExactKeys` [
                  "hs.vault_api_key"
                , "hs.db.host"
                , "hs.db.port"
                , "host"
                , "port"
                ]
        it "should filter out environment variables not listed in the prefix filter" $ do
            config <- withStubEnv $ execStateT (envReader ["HS_"]) M.empty
            config `shouldHaveKeys` ["hs.vault_api_key", "hs.db.host", "hs.db.port"]
            config `shouldNotHaveKeys` ["foo" , "host" , "port"]
        it "should normalize environment variables to lowercase and dot separators" $ do
            config <- withStubEnv $ execStateT (envReader ["HS__DB__", "TOO"]) M.empty
            config `shouldHaveKeys` ["hs.db.host", "hs.db.port", "too.many.underscores"]

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.CommandLine/src/CommandLineConfigurationProvider.cs
    describe "argsReader" $ do
        it "should read configuration from command-line args" $ do
            config <- withArgs stubArgs (execStateT argsReader M.empty)
            config `shouldBe` M.fromList [
                  ("api-key", String "foo-bar")
                , ("foo", Bool True)
                , ("bar", String "baz")
                , ("quux", Bool True)
                , ("arg1", String "val1")
                , ("db.host", String "127.0.0.1")
                , ("db.port", String "5432")
                ]
        it "should handle nullary args" $ do
            config <- withArgs nullaryArgs (execStateT argsReader M.empty)
            config `shouldBe` M.fromList [
                  ("foo", Bool True)
                , ("bar", String "baz")
                , ("quux", Bool True)
                ]
        it "should normalize argument keys to lowercase and dot separators" $ do
            config <- withArgs funnyArgs (execStateT argsReader M.empty)
            config `shouldBe` M.fromList [
                  ("arg1", String "val1")
                , ("db.host", String "127.0.0.1")
                , ("db.port", String "5432")
                ]

    -- describe "defaultReader" $ do
    --     it "args should override env" pending
    --     it "env should override the environment json file" pending
    --     it "the environment json file should override the base application json file" pending
    --     it "should read the environment json file only if the env was specified" pending

    -- describe "getConfigDefault" $ 
    --     it "should return the merged configuration map from the default reader" pending

    -- describe "getConfig" $ 
    --     it "should return the merged configuration map from the provided reader" pending
