module SetGame where

import SetGame.GameState
import SetGame.PlayGame
import System.Random


initializeGame :: IO GameState
initializeGame = do
  generator <- getStdGen

  let shuffledDeck = getShuffledDeck generator
      initialState = drawCards 12 (GameState [] shuffledDeck [])

  let (_, newGenerator) = split generator

  setStdGen newGenerator

  return initialState


runGame :: IO [CardSet]
runGame = do
  gameState <- initializeGame

  let (GameState _ _ finalSets) = playRounds gameState

  return finalSets
