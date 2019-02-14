{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import TH
import Class

-- import System.Environment.Config.Default
-- import Data.Char (toLower)
-- import Data.Aeson.TH
-- import GHC.Generics

-- data LogLevel = DEBUG | INFO | WARN | ERROR 
--     deriving (Show, Generic)

-- newtype Logging = Logging {
--         logLevel :: LogLevel
--     } deriving (Show, Generic)

-- data Config = Config {
--         env :: String,
--         host :: String,
--         port :: Int,
--         logging :: Logging
--     } deriving (Show, Generic)

-- deriveFromJSON defaultOptions ''LogLevel
-- deriveFromJSON defaultOptions ''Logging
-- deriveFromJSON defaultOptions ''Config

-- main :: IO ()
-- main = do
--     config <- getConfig @Config
--     print config

-- TODO: allow defining configuration key equality
-- case sensitive is unambiguous
-- case insensitive is ambiguous and needs a specifier (ambiguity resolver)

-- potential to configure ToConfig?

-- derive FromConfig x
--      - generates a FromJSON instance declaration for the tyep
--      - parseJSON :: Value -> Parse a
--          - for each field in type x
--              - get all case-insensitive matches for the field name from the value (avoid discarding information when the configuration tree is initially built)
--              - apply the ambiguity resolver to select a value (return last?, return first?)
--              - [k] -> k
--              - set the field to object[k]

data Foo = Foo { name :: String, age :: Int } 
data Bar = Bar { valid :: Bool, state :: Int } 

instance Show Foo where
    show (Foo n a) = "Name: " ++ n ++ ", Age: " ++ show a

instance Show Bar where
    show (Bar v s) = "Valid: " ++ show v ++ ", State: " ++ show s

deriveFormatter [|options|] ''Foo
deriveFormatter [|options{formatter=id}|] ''Bar

main :: IO ()
main = let
        f = Foo "John Doe" 23
        b = Bar True 103
    in do
        print $ doFmt f
        print $ doFmt b
