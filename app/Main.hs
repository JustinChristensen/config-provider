{-# LANGUAGE TypeApplications #-}
module Main where

import System.Environment.Config.Default (getConfig, get)

main :: IO ()
main = do
    config <- getConfig
    host <- get @String "host" config
    port <- get @Integer "port" config
    env <- get @String "env" config
    putStrLn $ "Host: " ++ host ++ ", Port: " ++ show port ++ ", Env: " ++ env