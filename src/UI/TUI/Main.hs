module UI.TUI.Main
  ( tui
  ) where

import qualified UI.TUI.Widgets                as W

import           Brick
import           Graphics.Vty.Input.Events
import           UI.TUI.State


type NewState = EventM () (Next AppState)

-- | the module's public function
tui :: IO ()
tui = defaultMain appState initialState >>= printExitStatus
  where printExitStatus = print . stateStatus


-- | create the application state
appState :: App AppState e ()
appState = App { appDraw         = drawTui
               , appChooseCursor = showFirstCursor
               , appStartEvent   = return
               , appHandleEvent  = handleTuiEvent
               , appAttrMap      = const $ attrMap mempty mempty
               }

-- | draw the TUI
drawTui :: AppState -> [Widget ()]
drawTui state =
  [ vBox
      [ W.title (stateTitle state)
      , hBox [W.inventory state, W.gameField state]
      , hBox [W.help, W.status state]
      ]
  ]

-- | handle TUI events
handleTuiEvent :: AppState -> BrickEvent n e -> NewState
handleTuiEvent s (VtyEvent (EvKey KLeft _)) = moveWest s
handleTuiEvent s (VtyEvent (EvKey KRight _)) = moveEast s
handleTuiEvent s (VtyEvent (EvKey KUp _)) = moveNorth s
handleTuiEvent s (VtyEvent (EvKey KDown _)) = moveSouth s
handleTuiEvent s (VtyEvent (EvKey (KChar 'a') _)) = moveWest s
handleTuiEvent s (VtyEvent (EvKey (KChar 'd') _)) = moveEast s
handleTuiEvent s (VtyEvent (EvKey (KChar 'w') _)) = moveNorth s
handleTuiEvent s (VtyEvent (EvKey (KChar 's') _)) = moveSouth s
handleTuiEvent s (VtyEvent (EvKey (KChar 'n') _)) = newGame s
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleTuiEvent s _ = continue s


moveWest :: AppState -> NewState
moveWest s = continue s { stateStatus = "Moving west..." }

moveEast :: AppState -> NewState
moveEast s = continue s { stateStatus = "Moving east..." }

moveNorth :: AppState -> NewState
moveNorth s = continue s { stateStatus = "Moving north..." }

moveSouth :: AppState -> NewState
moveSouth s = continue s { stateStatus = "Moving south..." }

newGame :: AppState -> NewState
newGame s = continue initialState
