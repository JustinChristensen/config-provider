{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config.Default (
      module System.Environment.Config
    , getConfig
    -- , readAppFiles
    , defaultReader
) where

import System.Environment.Config hiding (getConfig)
import qualified System.Environment.Config as C (getConfig)
import Control.Monad.IO.Class (MonadIO)
-- import System.Environment.Config.Types
-- import Control.Monad.Reader (reader)
-- import Control.Applicative ((<|>))
-- import Control.Monad.State (gets, modify)

-- readAppFiles :: Maybe String -> EnvReader Options ConfigNode ()
-- readAppFiles env = do
--         envKey <- reader envNameKey
--         readJsonFile "app.json"
--         fileEnv <- gets $ get envKey
--         maybe (return ()) readEnvFile (env <|> fileEnv)
--     where 
--         readEnvFile e = readJsonFile $ "app." ++ e ++ ".json"

-- defaultReader :: EnvReader Options ConfigNode ()
-- defaultReader = do
--     envKey <- reader envNameKey
--     env <- envSource $ envPrefixFilter envKey
--     args <- argsSource 
--     let prev = args <> env
--     readAppFiles $ get envKey prev
--     modify (prev <>)

-- mergeAppFiles :: Maybe String -> EnvReader Options ()
-- mergeAppFiles env = do
--         envKey <- reader envNameKey
--         merge @Json `from` file "app.json"
--         fileEnv <- gets $ get envKey
--         maybe (return ()) mergeEnvFile (env <|> fileEnv)
--     where 
--         mergeEnvFile e = merge @Json `from` file "app." ++ e ++ ".json"

defaultReader :: EnvReader Options ConfigNode
defaultReader = undefined

getConfig :: forall a m. (MonadIO m, FromConfig a) => m a
getConfig = C.getConfig defaultReader
