module SetGameTest where

--import Attributes
--import Card
--import PlayGame
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "my first test" $ do
    it "is 0" $ do
      "Hello" `shouldBe` "Hello"
