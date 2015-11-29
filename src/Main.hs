module Main where

import PlayGame
import System.Random


main :: IO ()
main = do
  generator <- getStdGen

  let initialState              = initializeBoard generator
      (GameState _ _ finalSets) = playRounds initialState

  mapM_ print finalSets
  print $ length finalSets

  return ()
