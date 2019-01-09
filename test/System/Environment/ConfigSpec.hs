module System.Environment.ConfigSpec (spec) where

import Test.Hspec
import System.Environment.Config

spec :: Spec 
spec = do
    describe "sayHi" $ do
        it "says hi" $
            greet "Justin" `shouldBe` "Hello, Justin"
