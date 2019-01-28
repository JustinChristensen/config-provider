{-# LANGUAGE OverloadedStrings #-}
module System.Environment.ConfigSpec (spec) where

import SpecHelper
import Test.Hspec
import System.Environment (withArgs)
import System.Environment.Config
import Data.List (isPrefixOf)
import Control.Monad.State (execStateT)
import qualified Data.HashMap.Strict as H

-- WARNING: 
-- The tests herein are highly dependent on the state of the environment
-- In particular, they depend on the fixtures located in the Fixtures/ subdirectory of test
-- and on the environment variables and arguments passed while executing the test suite itself
-- Running the test executable with extra arguments like --env or environment variables like ENV
-- WILL lead to test failures. 

spec :: Spec
spec = return ()