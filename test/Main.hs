module SetGameTest (main) where

import SetGame.Attributes
import SetGame.Cards
import SetGame.PlayGame
import Test.Hspec


gameState1 :: GameState
gameState1 = GameState board deck sets
  where board = [ Card {color=Blue, shade=Solid, number=One, shape=Oval} ]
        deck  = [ Card {color=Blue,   shade=Solid,   number=One, shape=Squiggle}
                , Card {color=Green,  shade=Solid,   number=One, shape=Squiggle}
                , Card {color=Purple, shade=Solid,   number=One, shape=Squiggle}
                , Card {color=Blue,   shade=Solid,   number=One, shape=Squiggle}
                , Card {color=Green,  shade=Striped, number=One, shape=Squiggle}
                , Card {color=Purple, shade=Empty,   number=One, shape=Squiggle} ]
        sets  = []

main :: IO ()
main = hspec $ do
  describe "drawCards" $ do
    it "should move n cards from the top of the deck to the board" $ do
      let (GameState finalBoard finalDeck _) = drawCards 4 gameState1

      let expectedBoard = [ Card {color=Blue,   shade=Solid, number=One, shape=Oval}
                          , Card {color=Blue,   shade=Solid, number=One, shape=Squiggle}
                          , Card {color=Green,  shade=Solid, number=One, shape=Squiggle}
                          , Card {color=Purple, shade=Solid, number=One, shape=Squiggle}
                          , Card {color=Blue,   shade=Solid, number=One, shape=Squiggle} ]

      let expectedDeck  = [ Card {color=Green,  shade=Striped, number=One, shape=Squiggle}
                          , Card {color=Purple, shade=Empty,   number=One, shape=Squiggle} ]

      finalBoard `shouldBe` expectedBoard
      finalDeck  `shouldBe` expectedDeck


  describe "playRound" $ do
    it "should do something" $ do
      "test" `shouldBe` "test"


  describe "playRounds" $ do
    it "should do something" $ do
      "test" `shouldBe` "test"


  describe "findSet" $ do
    it "should return the first set it finds" $ do
      let myBoard     = [ Card {color=Blue,   shade=Solid, number=One,   shape=Oval}
                        , Card {color=Green,  shade=Solid, number=Two,   shape=Oval}
                        , Card {color=Green,  shade=Solid, number=One,   shape=Oval}
                        , Card {color=Purple, shade=Solid, number=Three, shape=Oval} ]

          expectedSet = ( Card {color=Blue,   shade=Solid, number=One,   shape=Oval}
                        , Card {color=Green,  shade=Solid, number=Two,   shape=Oval}
                        , Card {color=Purple, shade=Solid, number=Three, shape=Oval} )

      findSet myBoard `shouldBe` Just expectedSet

    it "should return Nothing if there are no sets in the board" $ do
      let myBoard = [ Card {color=Blue,  shade=Solid, number=One, shape=Oval}
                    , Card {color=Green, shade=Solid, number=Two, shape=Oval}
                    , Card {color=Green, shade=Solid, number=One, shape=Oval} ]

      findSet myBoard `shouldBe` Nothing


  describe "isSet" $ do
    it "should correctly identify a set as such" $ do
      let mySet = ( Card {color=Blue,   shade=Solid, number=One,   shape=Oval}
                  , Card {color=Green,  shade=Solid, number=Two,   shape=Oval}
                  , Card {color=Purple, shade=Solid, number=Three, shape=Oval} )

      mySet `shouldSatisfy` isSet

    it "should reject a potential set when it fails the qualifications" $ do
      let mySet = ( Card {color=Blue,   shade=Solid, number=Two,   shape=Oval}
                  , Card {color=Green,  shade=Solid, number=Two,   shape=Oval}
                  , Card {color=Purple, shade=Solid, number=Three, shape=Oval} )

      mySet `shouldSatisfy` (not . isSet)


  describe "allUnique" $ do
    it "should be true" $ do
      ['a', 'b', 'c'] `shouldSatisfy` allUnique

    it "should be false" $ do
      ['a', 'a', 'c'] `shouldSatisfy` (not . allUnique)


  describe "allSame" $ do
    it "should be true" $ do
      ['a', 'a', 'a'] `shouldSatisfy` allSame

    it "should be false" $ do
      ['a', 'a', 'b'] `shouldSatisfy` (not . allSame)
