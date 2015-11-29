module PlayGame where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
--import System.Random
--import System.RandomShuffle

import Cards


type Deck      = [Card]
type Board     = [Card]
type CardSet   = (Card, Card, Card)
type GameState = (Board, Deck, [CardSet])


initializeBoard :: Deck -> GameState
initializeBoard deck = (board, deck \\ board, [])
  where board = take 12 deck


drawCards :: GameState -> GameState
drawCards (board, [], cs)          = (board, [], cs)
drawCards (board, [x], cs)         = (x:board, [], cs)
drawCards (board, [x1,x2], cs)     = (x1:x2:board, [], cs)
drawCards (board, x1:x2:x3:xs, cs) = (x1:x2:x3:board, xs, cs)


isSet :: (Card, Card, Card) -> Bool
isSet cards = all (propertyPasses cards) cardAccessors
  where propertyPasses (c1, c2, c3) accessor = flip elem [1,3]
                                             . length
                                             . nub
                                             . fmap accessor
                                             $ [c1, c2, c3]


allUnique :: (Card, Card, Card) -> Bool
allUnique (c1, c2, c3) = (== 3)
                       . length
                       . nub
                       $ [c1, c2, c3]


findSet :: Board -> Maybe CardSet
findSet board | length board < 3 = Nothing
              | otherwise        = maybeFirst
                                 . filter isSet
                                 . filter allUnique
                                 $ allCombinations
  where allCombinations  = [(,,)] <*> board <*> board <*> board
        maybeFirst []    = Nothing
        maybeFirst (x:_) = Just x


playRound :: GameState -> GameState
playRound (board, deck, sets) = (drawCards . removeSet . findSet) board
  where removeSet Nothing             = (board, deck, sets)
        removeSet (Just (c1, c2, c3)) = (updatedBoard, deck, updatedSets)
          where updatedBoard = board \\ [c1, c2, c3]
                updatedSets  = sets ++ [(c1, c2, c3)]


playRounds :: GameState -> GameState
playRounds (board, deck, cardSets)
  | shouldQuit = (board, deck, cardSets)
  | otherwise  = (playRounds . playRound) (board, deck, cardSets)
    where shouldQuit = (isNothing . findSet) board && deck == mempty


play :: IO ()
play = do
  let (board, deck, sets) = initializeBoard allCards
      (_, _, finalSets)  = playRounds (board, deck, sets)
  print finalSets
  print $ length finalSets
  return ()
