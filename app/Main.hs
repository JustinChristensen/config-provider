{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

-- import System.Environment.Config.Default

-- main :: IO ()
-- main = do
--     config <- getConfig 
--     debugConfig config

import GHC.Generics
-- import qualified Data.Aeson as A
-- import qualified Data.HashMap.Strict as H
-- import qualified Data.Vector as V
-- import Data.Vector ((!))
-- import Data.Scientific (scientific)

data Config = Config {
        host :: String,
        port :: Int
    } deriving (Show, Generic)

-- instance A.FromJSON Config

-- valPropTest :: IO () 
-- valPropTest = do
--     let pairs = [("host", A.String "123.123.123.5"), 
--                  ("port", A.Number $ scientific 3306 0),
--                  ("@@@val@@@", A.Bool True)]
--     let obj = A.Object $ H.fromList pairs
--     case A.fromJSON obj of 
--         A.Success c -> do
--             print (c :: Config)
--             print obj
--         A.Error e -> fail e

-- listTest :: IO ()
-- listTest = do
--     let obj = A.Object $ H.fromList [("host", A.String "123.123.123.5"), 
--                                      ("port", A.Number $ scientific 3306 0)]
--     let arr = A.Array $ V.fromList [A.Bool True, obj]
--     case arr of
--         A.Array v -> case A.fromJSON (v ! 1) of 
--                        A.Success c -> do
--                             print (c :: Config)
--                             print arr
--                        A.Error e -> fail e
--         _ -> fail "val is not an array"

-- main :: IO ()
-- main = do
--     valPropTest
--     listTest

main :: IO ()
main = do
