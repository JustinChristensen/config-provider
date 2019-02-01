module System.Environment.Config.TypesSpec (spec) where

import Test.Hspec
-- import System.Environment.Config.Types

spec :: Spec
spec = do
    describe "getE" $ do
        it "finds a value by path and converts it to the desired type" pending
        it "returns a KeyNotFoundError if there are still more path segments left after finding a non-object value" pending
        it "returns a KeyNotFoundError if lookup fails and there are no path segments remaining" pending
        it "returns a ParseValueError if the type conversion fails" pending
        it "converts and returns the first non-object value when it finds an array containing objects (xml)" pending
        it "converts and returns the array if the array does not contain objects (xml)" pending
    describe "getM" $ do
        it "returns the value if the key is found and the conversion succeeds" pending
        it "returns Nothing if getE returns an exception" pending
    describe "get" $ do
        it "finds a value by path and converts it to the desired type" pending
        it "throws a KeyNotFoundError if there are still more path segments left after finding a non-object value" pending
        it "throws a KeyNotFoundError if lookup fails and there are no path segments remaining" pending
        it "throws a ParseValueError if the type conversion fails" pending
    describe "ConfigMap" $ do
        describe "fromJSON" $ do
            it "wraps an object" pending
            it "throws if the value is not an object" pending
        describe "empty" $ 
            it "returns an empty config map" pending
        describe "merge" $ do
            it "recursively merges configuration maps" pending
            it "prefers values in the left configuration map unless null" pending
        describe "getEnv" $ 
            it "returns the environment if the configuration map has it" pending
    describe "EnvPairs" $ 
        describe "toJSON" $ do
            it "converts env pairs to a hierarchy of objects, where __ creates a new level" pending
            it "lowercases keys in the object hierarchy" pending
    describe "Node" $ 
        describe "toJSON" $ do
            it "converts an xml node to an object with the node name as the key" pending
            it "lowercases the keys" pending
            it "converts an xml node to an object with an array of contents and an attribute object as the value" pending
    describe "Content" $ 
        describe "toJSON" $ do
            it "converts an element" pending
            it "attempts to parse a text node" pending
            it "returns cdata as a string" pending
    describe "Ini" $ 
        describe "toJSON" $ do
            it "prefers sections to globals with clashing keys" pending
            it "lowercases keys" pending
            it "parses values" pending