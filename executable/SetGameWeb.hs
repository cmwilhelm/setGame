{-# LANGUAGE OverloadedStrings #-}
module SetGameWeb where

import           Control.Monad.IO.Class
import           Data.Aeson (encode)
import qualified Data.ByteString.Char8 as CharB
import           Data.Functor
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Time.Clock.POSIX
import           Snap.Core
import           Snap.Util.FileServe
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
routes = [ ("/sp",                   serveFile       "views/index.html")
         , ("/spa",                  serveDirectory  "static/build")
         , ("/vendor",               serveDirectory  "node_modules")
         , ("/sp/new",               method GET      newSinglePlayerGameHandler)
         , ("/sp/:gameId",           method GET      retrieveSinglePlayerGameHandler)
         , ("/all",                  method GET      allGamesHandler)
         , ("/full-game",            method GET      fullGameHandler)
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

newSinglePlayerGameHandler :: Handler App App ()
newSinglePlayerGameHandler = do
  (GameState board deck sets) <- liftIO initializeGame
  now <- liftIO $ (round . (* 1000)) <$> getPOSIXTime
  update $ InsertGame now (SinglePlayer (GameState board deck sets))

  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (now, board)


-- Looks up an existing game by its id and returns the board

retrieveSinglePlayerGameHandler :: Handler App App ()
retrieveSinglePlayerGameHandler = do
  Just gameIdParam <- getParam "gameId"
  let (Just (gameId, _)) = CharB.readInteger gameIdParam
  Just (SinglePlayer (GameState board _ _)) <- query $ LookupGame gameId
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (gameId, board)


--matchAttempt :: Handler App App ()
--matchAttempt = do
--  Just gameIdParam <- getParam "gameId"
--  Just cardIds     <- getPostParam "cardIds"
--  let (Just (gameId, _)) = CharB.readInteger gameIdParam
--  Just (SinglePlayer gameState) <- query $ LookupGame gameId
