{-# LANGUAGE TemplateHaskell    #-}
module SetGame.GameState where

import Data.SafeCopy
import SetGame.Cards


type Deck      = [Card]
type Board     = [Card]
type CardSet   = (Card, Card, Card)
data GameState = GameState Board Deck [CardSet]

$(deriveSafeCopy 0 'base ''GameState)
