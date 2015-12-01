module Main where

import PlayGame
import System.Random


main :: IO ()
main = do
  generator <- getStdGen

  let initialState              = initializeGame generator
      (GameState _ _ finalSets) = playRounds initialState

  mapM_ print finalSets
  print $ "Found: " ++ (show . length) finalSets ++ " sets"

  return ()
