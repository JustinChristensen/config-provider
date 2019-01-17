module Main where

import System.Environment.Config (getConfigDefault)

main :: IO ()
main = do
    config <- getConfigDefault
    print config