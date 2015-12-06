{-# LANGUAGE OverloadedStrings #-}
module SetGameWeb where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as CharB
import qualified Data.List as L
import Snap.Core
import Snap.Util.FileServe

import SetGame (runGame, initializeGame)
import SetGame.GameState
import SetGameWeb.Data()


site :: Snap ()
site =
    ifTop top <|>
    route [ ("sp", singlePlayerHandler) ] <|>
    dir "static" (serveDirectory ".")

top :: Snap ()
top = do
  results <- liftIO runGame
  writeBS $ CharB.pack
          . L.intercalate "\n"
          . fmap show
          $ results

singlePlayerHandler :: Snap ()
singlePlayerHandler = do
  (GameState board _ _) <- liftIO initializeGame
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ board
