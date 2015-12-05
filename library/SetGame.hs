module SetGame where

import SetGame.PlayGame
import System.Random


main :: IO ()
main = do
  generator <- getStdGen
  runApplication generator

runApplication :: StdGen -> IO ()
runApplication generator = do
  let initialState              = initializeGame generator
      (GameState _ _ finalSets) = playRounds initialState

  mapM_ print finalSets
  print $ "Found: " ++ (show . length) finalSets ++ " sets"

  return ()
