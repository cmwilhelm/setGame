module Main where

import           Snap (serveSnaplet, defaultConfig)
import           SetGameWeb (app)


main :: IO ()
main = serveSnaplet defaultConfig app
