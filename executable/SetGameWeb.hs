{-# LANGUAGE OverloadedStrings #-}
module SetGameWeb where

import           Control.Monad.IO.Class
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as CharB
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Time.Clock
import           Snap.Core
import           Snap (SnapletInit, Handler, addRoutes,
                 nestSnaplet, makeSnaplet)
import           Snap.Snaplet.AcidState (update, query, acidInit)

import SetGame (runGame, initializeGame)
import SetGame.GameState
import SetGameWeb.Data


app :: SnapletInit App App
app = makeSnaplet "setGame" "A web implementaiton of the Set game" Nothing $ do
    a <- nestSnaplet "acid" acid $ acidInit (GameStore Map.empty)
    addRoutes routes
    return $ App a


routes :: [(CharB.ByteString, Handler App App ())]
routes = [ ("/sp", singlePlayerHandler)
         , ("/all", allGamesHandler)
         , ("/full-game", fullGameHandler)
         ]


-- Plays an entire game and outputs the resultant sets

fullGameHandler :: Handler App App ()
fullGameHandler = do
  results <- liftIO runGame
  writeBS $ CharB.pack
          . L.intercalate "\n"
          . fmap show
          $ results


-- Counts the current number of games in progress

allGamesHandler :: Handler App App ()
allGamesHandler = do
  allGames <- query CountAllGames
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ allGames


-- Starts a new single player game and returns the game board

singlePlayerHandler :: Handler App App ()
singlePlayerHandler = do
  (GameState board deck sets) <- liftIO initializeGame
  now <- liftIO getCurrentTime
  update $ InsertGame now (SinglePlayer (GameState board deck sets))

  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ board
