module Main where

import System.Environment.Config.Default

main :: IO ()
main = do
    config <- getConfig 
    print (config :: FlatConfigMap)