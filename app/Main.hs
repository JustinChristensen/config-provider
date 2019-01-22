module Main where

import System.Environment.Config.Default (getConfig)

-- https://downloads.haskell.org/~ghc/8.4.4/docs/html/users_guide/glasgow_exts.html#multi-parameter-type-classes
-- https://downloads.haskell.org/~ghc/8.4.4/docs/html/users_guide/glasgow_exts.html#extension-MultiParamTypeClasses
-- https://github.com/bos/aeson/blob/master/Data/Aeson/Types/ToJSON.hs#L703 
-- https://github.com/bos/aeson/blob/master/Data/Aeson/Types/ToJSON.hs#L1089
-- https://github.com/bos/aeson/blob/master/Data/Aeson/Types/ToJSON.hs#L2814

main :: IO ()
main = getConfig >>= print