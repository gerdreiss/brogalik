module UI.TUI.State where


data AppState = AppState
  { stateTitle  :: String
  , stateStatus :: String
  }


-- | create the initial AppState
initialState :: AppState
initialState = AppState
  { stateTitle  = "Brogalik - Rogue-alike game build with Brick"
  , stateStatus = "New game started"
  }
