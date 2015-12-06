module Main where

import Snap.Http.Server
import SetGameWeb (site)


main :: IO ()
main = quickHttpServe site
