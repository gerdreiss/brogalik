module UI.TUI.State where


data AppState = AppState
  { stateTitle  :: String
  , stateStatus :: String
  }


-- | create the initial AppState
initialState :: IO AppState
initialState = return AppState
  { stateTitle  = "Brogalik - Rogue-alike game build with Brick"
  , stateStatus = "New game started"
  }
