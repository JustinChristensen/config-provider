module Fixtures.EnvData where

stubEnv :: [(String, String)]
stubEnv = [
    ("HS__VAULT_API_KEY", "vault-api-key")
  , ("HS__DB__HOST", "127.0.0.1")
  , ("HS__DB__PORT", "3306")
  , ("HOST", "0.0.0.0")
  , ("PORT", "3000")
  , ("FOO", "bar")
  , ("BAZ", "quux")
  , ("TOO___MANY_____UNDERSCORES", "111111")
  ]

nullaryArgs :: [String]
nullaryArgs = [
    "--foo"
  , "--bar=baz"
  , "quux"
  ]

funnyArgs :: [String]
funnyArgs = [
    "ARG1=val1"
  , "--db.host=127.0.0.1"
  , "db__port=5432"
  ]

stubArgs :: [String]
stubArgs = ["--API-key", "foo-bar"] ++ nullaryArgs ++ funnyArgs
