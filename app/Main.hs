module Main where

import System.Environment.Config.Default (getConfig)

main :: IO ()
main = getConfig >>= print