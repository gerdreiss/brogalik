module UI.TUI.State where

import           Control.Brogalik
import           Data.Brogalik

data AppState = AppState
  { stateTitle    :: String
  , stateStatus   :: String
  , stateBrogalik :: Brogalik
  }


-- | create the initial AppState
initialState :: AppState
initialState = AppState
  { stateTitle    = "Brogalik - Rogue-alike game build with Brick"
  , stateStatus   = "New game started"
  , stateBrogalik = generateBrogalik
  }
