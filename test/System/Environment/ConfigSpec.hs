module System.Environment.ConfigSpec (spec) where

import SpecHelper
import Test.Hspec
import System.Environment (withArgs)
import System.Environment.Config
import qualified Data.Map as M
import Control.Monad.State (execStateT)

-- TODO: define required API for file readers
-- TODO: define API for the consumer to define the type of config value
-- TODO: define type annotations for logical requirements
-- TODO: define API for deriving CLI usage string from config value type

spec :: Spec
spec = do
    -- describe "getEnvName" $ do
    --     it "should read the environment name from args" pending
    --     it "should read the environment name from env vars" pending
    --     it "should prefer args to env vars" pending

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.FileExtensions/src/FileConfigurationSource.cs
    -- describe "jsonFileReader" $ do
    --     it "should read configuration from a json file" pending
    --     it "should flatten the structure into the configuration map" pending
    --     it "should merge with the upstream configuration map" pending

    -- describe "xmlFileReader" $ do
    --     it "should read configuration from an xml file" pending
    --     it "should flatten the structure into the configuration map" pending
    --     it "should merge with the upstream configuration map" pending

    -- describe "yamlFileReader" $ do
    --     it "should read configuration from a yaml file" pending
    --     it "should flatten the structure into the configuration map" pending
    --     it "should merge with the upstream configuration map" pending

    -- describe "iniFileReader" $ do
    --     it "should read configuration from an ini file" pending
    --     it "should flatten the structure into the configuration map" pending
    --     it "should merge with the upstream configuration map" pending

    -- describe "remoteReader" $ do
    --     it "should read configuration from a vault" pending
    --     it "should merge with the upstream configuration map" pending

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.EnvironmentVariables/src/EnvironmentVariablesConfigurationProvider.cs
    around_ stubEnvVars $
        describe "envReader" $ do
            it "should read configuration from environment variables" $ do
                config <- execStateT (envReader ["HS_", "HOST", "PORT"]) M.empty
                config `shouldHaveExactKeys` [
                      "hs.vault_api_key"
                    , "hs.db.host"
                    , "hs.db.port"
                    , "host"
                    , "port"
                    ]
            it "should filter out environment variables not listed in the prefix filter" $ do
                config <- execStateT (envReader ["HS_"]) M.empty
                config `shouldHaveKeys` ["hs.vault_api_key", "hs.db.host", "hs.db.port"]
                config `shouldNotHaveKeys` ["foo" , "host" , "port"]
            it "should normalize environment variables to lowercase and dot separators" $ do
                config <- execStateT (envReader ["HS__DB__", "TOO"]) M.empty
                config `shouldHaveKeys` ["hs.db.host", "hs.db.port", "too.many.underscores"]

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.CommandLine/src/CommandLineConfigurationProvider.cs
    describe "argsReader" $ do
        it "should read configuration from command-line args" $ do 
            config <- withArgs stubArgs (execStateT argsReader M.empty)
            config `shouldBe` M.fromList [
                  ("foo", "")
                , ("bar", "baz")
                , ("quux", "")
                , ("arg1", "val1")
                , ("db.host", "127.0.0.1")
                , ("db.port", "5432")
                ]
        it "should handle nullary args" $ do
            config <- withArgs nullaryArgs (execStateT argsReader M.empty)
            config `shouldBe` M.fromList [
                  ("foo", "")
                , ("bar", "baz")
                , ("quux", "")
                ]
        it "should normalize argument keys to lowercase and dot separators" $ do
            config <- withArgs funnyArgs (execStateT argsReader M.empty)
            config `shouldBe` M.fromList [
                  ("arg1", "val1")
                , ("db.host", "127.0.0.1")
                , ("db.port", "5432")
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
