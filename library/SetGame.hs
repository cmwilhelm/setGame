module SetGame where

import SetGame.PlayGame
import System.Random


runGame :: IO [CardSet]
runGame = do
  generator <- getStdGen

  let initialState              = initializeGame generator
      (GameState _ _ finalSets) = playRounds initialState

  let (_, newGenerator) = split generator

  setStdGen newGenerator

  return finalSets
