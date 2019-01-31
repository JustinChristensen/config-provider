module Main where

import System.Environment.Config.Default
import System.Environment (getArgs)

main :: IO ()
main = do
    config <- getConfig 
    args <- getArgs
    print args
    print (config :: FlatConfigMap)