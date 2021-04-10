{-# LANGUAGE TemplateHaskell #-}
module UI.TUI.State where

import           Control.Brogalik               ( generateBrogalik )
import           Data.Brogalik                  ( Brogalik )
import           Data.Geom                      ( Size )
import           Data.Text                      ( Text )
import           Lens.Micro.TH                  ( makeLenses )

data AppState = AppState
  { _stateTitle    :: Text
  , _stateStatus   :: Text
  , _stateBrogalik :: Brogalik
  }

makeLenses ''AppState

-- | create the initial AppState
initialState :: Size -> AppState
initialState size = AppState
  { _stateTitle    = "Brogalik - Rogue-alike game build with Brick"
  , _stateStatus   = "New game started"
  , _stateBrogalik = generateBrogalik size
  }
