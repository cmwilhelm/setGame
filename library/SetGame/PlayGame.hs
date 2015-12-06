module SetGame.PlayGame where

import SetGame.Cards
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import System.Random
import System.Random.Shuffle


type Deck      = [Card]
type Board     = [Card]
type CardSet   = (Card, Card, Card)
data GameState = GameState Board Deck [CardSet]


drawCards :: Int -> GameState -> GameState
drawCards n (GameState board deck sets) = GameState updatedBoard updatedDeck sets
  where drawn        = take n deck
        updatedBoard = board ++ drawn
        updatedDeck  = deck \\ drawn


playRound :: GameState -> GameState
playRound (GameState board deck sets) = (drawCards 3 . removeSet . findSet) board
  where removeSet Nothing             = GameState board deck sets
        removeSet (Just (c1, c2, c3)) = GameState updatedBoard deck updatedSets
          where updatedBoard = board \\ [c1, c2, c3]
                updatedSets  = sets ++ [(c1, c2, c3)]


playRounds :: GameState -> GameState
playRounds gameState
  | shouldQuit gameState = gameState
  | otherwise            = (playRounds . playRound) gameState
  where shouldQuit (GameState board deck _) = hasNoSets board && deckIsEmpty deck
        hasNoSets board                     = isNothing $ findSet board
        deckIsEmpty deck                    = deck == mempty



shuffleCards :: Deck -> StdGen -> Deck
shuffleCards deck generator = shuffle' deck (length deck) generator


getShuffledDeck :: StdGen -> Deck
getShuffledDeck generator = shuffleCards allCards generator


findSet :: Board -> Maybe CardSet
findSet board | length board < 3 = Nothing
              | otherwise        = maybeFirst
                                 . filter isSet
                                 . filter (\(c1, c2, c3) -> allUnique [c1, c2, c3])
                                 $ allCombinations
  where allCombinations  = [(,,)] <*> board <*> board <*> board
        maybeFirst []    = Nothing
        maybeFirst (x:_) = Just x


isSet :: (Card, Card, Card) -> Bool
isSet cards = all (attrPasses cards) cardAccessors
  where attrPasses (c1, c2, c3) accessor = (\attrs -> allUnique attrs
                                                   || allSame attrs)
                                         . fmap accessor
                                         $ [c1, c2, c3]


allUnique :: (Eq a) => [a] -> Bool
allUnique xs = (== expectedLength)
             . length
             . nub
             $ xs
  where expectedLength = length xs


allSame :: (Eq a) => [a] -> Bool
allSame = (== 1) . length . nub
