module Main ( main ) where

--import Attributes
--import Card
--import PlayGame
import Test.Hspec
import SetGame ()

main :: IO ()
main = do
  hspec $ do
    describe "my first test" $ do
      it "is 0" $ do
        "Hello" `shouldBe` "Hello"
