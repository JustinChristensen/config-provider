module System.Environment.Config.BaseSpec (spec) where

import Test.Hspec
-- import System.Environment.Config.Base

spec :: Spec
spec = do
    describe "splitAtEl" $ 
        it "splits the list at the specified element, and removes it from the remainder" pending
    describe "toVal" $ do
        it "parses the value" pending
        it "returns a string if parsing fails" pending
    describe "nodeList" $ do
        it "creates a list of content objects from the nodes contents" pending
        it "appends an object containing the nodes attributes to the end of the content list" pending
        it "skips whitespace-only nodes in the contents" pending