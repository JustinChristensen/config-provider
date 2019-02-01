module System.Environment.Config.DefaultSpec (spec) where

import Test.Hspec
-- import System.Environment.Config.Default

spec :: Spec
spec = do
    describe "appFileReader" $ do
        it "reads the default app.json file" pending
        it "reads an environment-specific configuration if the configuration specifies the environment" pending
    describe "defaultReader" $ do
        it "prefers command-line arguments to environment variables" pending
        it "prefers environment variables to environment specific configuration from a file" pending
        it "prefers environment-specific configuration to default file configuration" pending
    describe "getConfig" $ 
        it "executes the default reader and returns the result converted to the specified type" pending
