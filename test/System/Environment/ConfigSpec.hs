module System.Environment.ConfigSpec (spec) where

import Test.Hspec
import System.Environment.Config

-- TODO: define required API for file readers
-- TODO: define API for the consumer to define the type of config value
-- TODO: define type annotations for logical requirements
-- TODO: define API for deriving CLI usage string from config value type

spec :: Spec 
spec = do
    describe "getEnvName" $ do
        it "should read the environment name from args" pending
        it "should read the environment name from env vars" pending
        it "should prefer args to env vars" pending

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.FileExtensions/src/FileConfigurationSource.cs
    describe "jsonFileReader" $ 
        it "should read configuration from a json file" pending
        it "should flatten the structure into the configuration map" pending
        it "should merge with the upstream configuration map" pending

    describe "xmlFileReader" $ 
        it "should read configuration from an xml file" pending
        it "should flatten the structure into the configuration map" pending
        it "should merge with the upstream configuration map" pending

    describe "yamlFileReader" $ 
        it "should read configuration from a yaml file" pending
        it "should flatten the structure into the configuration map" pending
        it "should merge with the upstream configuration map" pending

    describe "iniFileReader" $ 
        it "should read configuration from an ini file" pending
        it "should flatten the structure into the configuration map" pending
        it "should merge with the upstream configuration map" pending

    describe "remoteReader" $ 
        it "should read configuration from a vault" pending
        it "should merge with the upstream configuration map" pending

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.EnvironmentVariables/src/EnvironmentVariablesConfigurationProvider.cs
    describe "envReader" $ 
        it "should read configuration from environment variables" pending
        it "should filter out environment variables not listed in the prefix filter" pending
        it "should normalize environment variables to lowercase and dot separators" pending

    -- https://github.com/aspnet/Extensions/blob/master/src/Configuration/Config.CommandLine/src/CommandLineConfigurationProvider.cs
    describe "argsReader" $ 
        it "should read configuration from command-line args" pending
        it "should normalize environment variables to lowercase and dot separators" pending

    describe "defaultReader" $ 
        it "args should override env" pending
        it "env should override the environment json file" pending
        it "the environment json file should override the base application json file" pending
        it "should read the environment json file only if the env was specified" pending

    describe "getConfigDefault" $ 
        it "should return the merged configuration map from the default reader" pending

    describe "getConfig" $ 
        it "should return the merged configuration map from the provided reader" pending
