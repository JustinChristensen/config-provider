module SpecHelper where

import Test.Hspec
import System.Environment (setEnv, unsetEnv)
import Control.Exception (bracket_)
import qualified Data.Map as M
import qualified Data.Set as S

stubEnv :: [(String, String)]
stubEnv = [
      ("HS_VAULT_API_KEY", "vault-api-key")
    , ("HS_DB_HOST", "127.0.0.1")
    , ("HS_DB_PORT", "3306")
    , ("HOST", "0.0.0.0")
    , ("PORT", "3000")
    , ("FOO", "bar")
    , ("BAZ", "quux")
    , ("TOO___MANY__UNDERSCORES", "111111")
    ]

setEnvVars :: [(String, String)] -> IO ()
setEnvVars = mapM_ $ uncurry setEnv

unsetEnvVars :: [(String, String)] -> IO ()
unsetEnvVars = mapM_ $ unsetEnv . fst

stubEnvVars :: IO () -> IO ()
stubEnvVars = bracket_ (setEnvVars stubEnv) (unsetEnvVars stubEnv)

shouldHaveKeys :: Ord k => M.Map k v -> [k] -> Expectation
actual `shouldHaveKeys` expected = 
    S.fromList expected `S.isSubsetOf` M.keysSet actual `shouldBe` True

shouldNotHaveKeys :: Ord k => M.Map k v -> [k] -> Expectation
actual `shouldNotHaveKeys` expected = 
    S.fromList expected `S.disjoint` M.keysSet actual `shouldBe` True

shouldHaveExactKeys :: (Show k, Eq k) => M.Map k v -> [k] -> Expectation
actual `shouldHaveExactKeys` expected = 
    M.keys actual `shouldMatchList` expected