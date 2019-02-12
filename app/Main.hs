{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment.Config.Default
import GHC.Generics

data LogLevel = DEBUG | INFO | WARN | ERROR 
    deriving (Show, Generic)

data Logging = Logging {
        logLevel :: LogLevel
    } deriving (Show, Generic)

data AppConfig = AppConfig {
        env :: String,
        host :: String,
        logging :: Logging
    } deriving (Show, Generic)

instance FromJSON LogLevel
instance FromJSON Logging
instance FromJSON AppConfig

main :: IO ()
main = do
    config <- getConfig @AppConfig
    print config