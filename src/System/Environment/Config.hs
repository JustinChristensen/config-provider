{-# LANGUAGE ExistentialQuantification #-}
module System.Environment.Config (
      module System.Environment.Config.Types
    , defaultOptions

    , getConfig
    , merge
    , mapUsing

    , file
    , requiredFile
    , env
    , args

    , envFilter
) where

import System.Environment.Config.Source
import System.Environment.Config.Types
import System.Environment.Config.Base (envFilter)
import Control.Monad.IO.Class (MonadIO)
import qualified System.Environment.Config.Base as B (getConfig)

defaultOptions :: Options
defaultOptions = Options {
        envNameKey = "env",
        envPrefixFilter = [
              envNameKey defaultOptions
            , "~hs__"
            , "host"
            , "port"
        ]
    }

infixl 1 `merge`
merge :: (ToConfig a, ToConfig b) => EnvReader Options a -> EnvReader Options b -> EnvReader Options ConfigNode
merge = undefined

getConfig :: forall b a m. (MonadIO m, ToConfig a, FromConfig b) => EnvReader Options a -> m b
getConfig = B.getConfig defaultOptions
