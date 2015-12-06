{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module SetGameWeb.Data where

import           Control.Lens (makeLenses, view)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map as Map
import           Data.SafeCopy
import           Data.Typeable
import           SetGame.GameState
import           Snap (Snaplet, snapletValue)
import           Snap.Snaplet.AcidState

-- data persistence

type GameID = Integer
data WebGameState = SinglePlayer GameState
  deriving (Typeable)

type GameStateMap = Map.Map GameID WebGameState

data GameStore = GameStore GameStateMap
  deriving (Typeable)

makeLenses ''WebGameState

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

countAllGames :: Query GameStore Int
countAllGames = do
  GameStore m <- ask
  return (length . Map.toList $ m)

$(makeAcidic ''GameStore ['insertGame, 'lookupGame, 'countAllGames])

data App = App
    { _acid :: Snaplet (Acid GameStore)
    }

makeLenses ''App

instance HasAcid App GameStore where
  getAcidStore = view (acid.snapletValue)
