module Main where

import PlayGame
import System.Random

main :: IO ()
main = do
  generator <- getStdGen

  let shuffledDeck              = getShuffledDeck generator
      initialState              = initializeBoard shuffledDeck
      (GameState _ _ finalSets) = playRounds initialState

  mapM_ print finalSets
  print $ length finalSets

  return ()
