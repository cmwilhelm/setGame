{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as CharB
import qualified Data.List as L
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
  writeBS $ CharB.pack
          . L.intercalate "\n"
          . fmap show
          $ results
