{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.Char8
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server
import SetGame (runGame)


main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop top <|>
    dir "static" (serveDirectory ".")

top :: Snap ()
top = do
  results <- liftIO runGame
  writeBS $ pack results
