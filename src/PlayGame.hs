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
data GameState = GameState Board Deck [CardSet]


initializeBoard :: Deck -> GameState
initializeBoard deck = GameState board (deck \\ board) []
  where board = take 12 deck


drawCards :: GameState -> GameState
drawCards (GameState board [] cs)            = GameState board [] cs
drawCards (GameState board [x] cs)           = GameState (x:board) [] cs
drawCards (GameState board [x1,x2] cs)       = GameState (x1:x2:board) [] cs
drawCards (GameState board (x1:x2:x3:xs) cs) = GameState (x1:x2:x3:board) xs cs


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
playRound (GameState board deck sets) = (drawCards . removeSet . findSet) board
  where removeSet Nothing             = GameState board deck sets
        removeSet (Just (c1, c2, c3)) = GameState updatedBoard deck updatedSets
          where updatedBoard = board \\ [c1, c2, c3]
                updatedSets  = sets ++ [(c1, c2, c3)]


playRounds :: GameState -> GameState
playRounds gameState
  | shouldQuit gameState = gameState
  | otherwise            = (playRounds . playRound) gameState
    where shouldQuit (GameState board deck _) = (isNothing . findSet) board && deck == mempty


play :: IO ()
play = do
  let initialState              = initializeBoard allCards
      GameState _ _ finalSets  = playRounds initialState
  print finalSets
  print $ length finalSets
  return ()
