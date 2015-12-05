module SetGame where

import SetGame.PlayGame
import System.Random


runGame :: IO String
runGame = do
  generator <- getStdGen

  let initialState              = initializeGame generator
      (GameState _ _ finalSets) = playRounds initialState

  return $ show finalSets
