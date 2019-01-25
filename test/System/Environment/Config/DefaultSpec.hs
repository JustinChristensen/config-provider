{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.DefaultSpec (spec) where

import SpecHelper
import Test.Hspec
import System.Environment (withArgs)
import System.Environment.Config.Default

spec :: Spec
spec = return ()

-- spec :: Spec
-- spec = 
--     describe "getEnvName" $
--         it "should prefer args to env vars" $ do
--             e1 <- withEnv [("ENV", "development")] getEnvName
--             e2 <- withArgs ["--env", "production"] getEnvName
--             e3 <- withArgs ["--ENV", "staging"] $ withEnv [("ENV", "development")] getEnvName
--             e4 <- getEnvName
--             e1 `shouldBe` Just (String "development")
--             e2 `shouldBe` Just (String "production")
--             e3 `shouldBe` Just (String "staging")
--             e4 `shouldBe` Nothing
