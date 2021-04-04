module UI.TUI.State where

import           Control.Brogalik
import           Data.Brogalik
import           Data.Geom
import           Data.Text

data AppState = AppState
  { stateTitle    :: Text
  , stateStatus   :: Text
  , stateBrogalik :: Brogalik
  }

-- | create the initial AppState
initialState :: Width -> Height -> AppState
initialState w h = AppState
  { stateTitle    = "Brogalik - Rogue-alike game build with Brick"
  , stateStatus   = "New game started"
  , stateBrogalik = generateBrogalik w h
  }
