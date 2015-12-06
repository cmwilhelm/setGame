{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
module SetGameWeb.Data where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import qualified Data.Map as Map
import           Data.SafeCopy
import           Data.Typeable
import           SetGame.GameState


-- data persistence

type GameID = String
data WebGameState = SinglePlayer GameState
                  | MultiPlayer GameState
  deriving (Typeable)

data GameStore = GameStore !(Map.Map GameID WebGameState)
  deriving (Typeable)

$(deriveSafeCopy 0 'base ''WebGameState)
$(deriveSafeCopy 0 'base ''GameStore)

insertGame :: GameID -> WebGameState -> Update GameStore ()
insertGame key value
    = do GameStore m <- get
         put (GameStore (Map.insert key value m))

lookupGame :: GameID -> Query GameStore (Maybe WebGameState)
lookupGame key
    = do GameStore m <- ask
         return (Map.lookup key m)

$(makeAcidic ''GameStore ['insertGame, 'lookupGame])
