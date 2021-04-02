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
tui = initialState >>= defaultMain appState >>= printExitStatus
  where printExitStatus = putStrLn . stateStatus


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
      [ W.title state
      , hBox [W.inventory state, W.gameField state]
      , hBox [W.help, W.status state]
      ]
  ]

-- | handle TUI events
handleTuiEvent :: AppState -> BrickEvent n e -> NewState
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleTuiEvent s _ = continue s
