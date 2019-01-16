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
import Fixtures.EnvData
import qualified Data.Map.Strict as M
import qualified Data.Set as S

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

checkKeys :: EnvReader () -> [String] -> Expectation
checkKeys reader keys = do
    config <- execStateT reader M.empty
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
        config <- execStateT reader $ M.fromList previousConfig
        mapM_ (checkMergedValue config) nextConfig
    where 
        checkMergedValue config (k, v) = M.lookup k config `shouldBe` Just v