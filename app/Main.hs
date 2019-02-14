{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment.Config.Default
import GHC.Generics

data LogLevel = DEBUG | INFO | WARN | ERROR 
    deriving (Show, Generic, FromJSON)

newtype Logging = Logging {
        logLevel :: LogLevel
    } deriving (Show, Generic, FromJSON)

data Config = Config {
        env :: String,
        host :: String,
        port :: Int,
        logging :: Logging
    } deriving (Show, Generic, FromJSON)

main :: IO ()
main = do
    config <- getConfig @Config
    print config