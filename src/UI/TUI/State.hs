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
initialState :: Size -> AppState
initialState size = AppState
  { stateTitle    = "Brogalik - Rogue-alike game build with Brick"
  , stateStatus   = "New game started"
  , stateBrogalik = generateBrogalik size
  }
