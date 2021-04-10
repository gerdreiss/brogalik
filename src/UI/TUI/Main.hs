module UI.TUI.Main
  ( tui
  ) where

import qualified UI.TUI.Widgets.Main           as W

import           Brick                   hiding ( Direction )
import           Data.Array
import           Data.Brogalik
import           Data.Geom
import           Data.Text
import           Graphics.Vty.Input.Events
import           Lens.Micro
import           UI.TUI.State

type NewState = EventM () (Next AppState)

-- | the module's public function
tui :: Width -> Height -> IO ()
tui w h = defaultMain appState (initialState $ Size w h) >>= printExitStatus
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
      , hBox
        [ W.inventory $ state & stateBrogalik & brogalikPlayer
        , W.gameField $ stateBrogalik state
        ]
      , hBox [W.help, W.status state]
      ]
  ]

-- | handle TUI events
handleTuiEvent :: AppState -> BrickEvent n e -> NewState
handleTuiEvent s (VtyEvent (EvResize w h)) = resizeBrogalik w h s
handleTuiEvent s (VtyEvent (EvKey (KChar 'n') _)) = newGame s
handleTuiEvent s (VtyEvent (EvKey (KChar 'a') _)) = movePlayer West s
handleTuiEvent s (VtyEvent (EvKey (KChar 'd') _)) = movePlayer East s
handleTuiEvent s (VtyEvent (EvKey (KChar 'w') _)) = movePlayer North s
handleTuiEvent s (VtyEvent (EvKey (KChar 's') _)) = movePlayer South s
handleTuiEvent s (VtyEvent (EvKey KLeft _)) = movePlayer West s
handleTuiEvent s (VtyEvent (EvKey KRight _)) = movePlayer East s
handleTuiEvent s (VtyEvent (EvKey KUp _)) = movePlayer North s
handleTuiEvent s (VtyEvent (EvKey KDown _)) = movePlayer South s
handleTuiEvent s (VtyEvent (EvKey (KChar 'q') _)) = halt s
handleTuiEvent s _ = continue s

resizeBrogalik :: Width -> Height -> AppState -> NewState
resizeBrogalik w h s = continue s
  { stateStatus   = pack $ "Terminal size : " <> show w <> "x" <> show h
  , stateBrogalik = doResize w h (stateBrogalik s)
  }
  where doResize w h brogalik = brogalik { brogalikSize = Size w h }

newGame :: AppState -> NewState
newGame = continue . initialState . brogalikSize . stateBrogalik

movePlayer :: Direction -> AppState -> NewState
movePlayer d s = continue s { stateStatus = pack $ "Moving " <> show d <> "..."
                            , stateBrogalik = brogalikMove d (stateBrogalik s)
                            }

brogalikMove :: Direction -> Brogalik -> Brogalik
brogalikMove direction brogalik = brogalik
  { brogalikPlayer = player { playerPos  = newPos
                            , playerGold = playerGold player + goldFound
                            }
  }
 where
  player    = brogalikPlayer brogalik
  rect      = brogalikRooms brogalik ! playerRoom player & roomRect
  newPos    = clampPos rect $ playerPos player |--> directionChanges direction
  goldFound = 0

clampPos :: Rect -> Pos -> Pos
clampPos (Rect (Pos rectX rectY) (Size w h)) (Pos x y) = Pos newX newY
 where
  newX = clamp (rectX - 1) (rectX + w - 2) x
  newY = clamp (rectY - 1) (rectY + h - 2) y
