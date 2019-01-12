module SpecHelper (
    module SpecHelper,
    module Fixtures.EnvData
) where

import Test.Hspec
import System.Environment (setEnv, unsetEnv)
import Control.Exception (bracket_)
import Control.Monad (unless)
import qualified Data.Map as M
import qualified Data.Set as S
import Fixtures.EnvData

setEnvVars :: [(String, String)] -> IO ()
setEnvVars = mapM_ $ uncurry setEnv

unsetEnvVars :: [(String, String)] -> IO ()
unsetEnvVars = mapM_ $ unsetEnv . fst

stubEnvVars :: IO () -> IO ()
stubEnvVars = bracket_ (setEnvVars stubEnv) (unsetEnvVars stubEnv)

shouldHaveKeys :: (Show k, Ord k) => M.Map k v -> [k] -> Expectation
actual `shouldHaveKeys` expected = let
        s1 = S.fromList expected
        s2 = M.keysSet actual
        errMsg = show s1 ++ " should be a subset of " ++ show s2
    in unless (s1 `S.isSubsetOf` s2) $ expectationFailure errMsg

shouldNotHaveKeys :: (Show k, Ord k) => M.Map k v -> [k] -> Expectation
actual `shouldNotHaveKeys` expected = let
        s1 = S.fromList expected
        s2 = M.keysSet actual
        errMsg = show s1 ++ " should be disjoint of " ++ show s2
    in unless (s1 `S.disjoint` s2) $ expectationFailure errMsg

shouldHaveExactKeys :: (Show k, Ord k) => M.Map k v -> [k] -> Expectation
actual `shouldHaveExactKeys` expected = 
    M.keys actual `shouldMatchList` expected