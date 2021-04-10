module UI.TUI.Main
  ( tui
  ) where

import qualified UI.TUI.Widgets.Main           as W

import           Brick                   hiding ( Direction )
import           Data.Array
import           Data.Brogalik
import           Data.Geom
import           Data.Maybe
import           Data.Text
import           Graphics.Vty.Input.Events
import           Lens.Micro.GHC
import           UI.TUI.State

type NewState = EventM () (Next AppState)

-- | the module's public function
tui :: Width -> Height -> IO ()
tui width height = defaultMain app state >>= printExitStatus
 where
  state           = initialState $ Size width height
  printExitStatus = putStrLn . unpack . _stateStatus

-- | create the application state
app :: App AppState e ()
app = App { appDraw         = drawTui
          , appChooseCursor = showFirstCursor
          , appStartEvent   = return
          , appHandleEvent  = handleTuiEvent
          , appAttrMap      = const $ attrMap mempty mempty
          }

-- | draw the TUI
drawTui :: AppState -> [Widget ()]
drawTui state =
  [ vBox
      [ W.title (state ^. stateTitle)
      , hBox
        [ W.inventory (state ^. stateBrogalik . brogalikPlayer)
        , W.gameField (state ^. stateBrogalik)
        ]
      , hBox [W.help, W.status (state ^. stateStatus)]
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
resizeBrogalik width height state =
  continue $ state & updateSize & updateStatus
 where
  updateSize = stateBrogalik . brogalikSize .~ Size width height
  updateStatus =
    stateStatus .~ pack ("Terminal size : " <> show width <> "x" <> show height)

newGame :: AppState -> NewState
newGame = continue . initialState . _brogalikSize . _stateBrogalik

movePlayer :: Direction -> AppState -> NewState
movePlayer direction state = continue $ state & moveBrogalik & updateStatus
 where
  moveBrogalik = stateBrogalik %~ brogalikMove direction
  updateStatus = stateStatus .~ pack ("Moving " <> show direction <> "...")

brogalikMove :: Direction -> Brogalik -> Brogalik
brogalikMove direction brogalik = brogalik & updatePos & updateGold
 where
  updatePos  = brogalikPlayer . playerPos .~ clampPos rect newPos
  rect       = brogalik ^?! brogalikRooms . ix roomIndex . roomRect
  roomIndex  = brogalik ^. brogalikPlayer . playerRoom
  newPos     = oldPos |--> directionChanges direction
  oldPos     = brogalik ^. brogalikPlayer . playerPos
  updateGold = brogalikPlayer . playerGold %~ (+) goldFound
  goldFound  = 0 -- TODO implement goldFound

clampPos :: Rect -> Pos -> Pos
clampPos (Rect (Pos rectX rectY) (Size w h)) (Pos x y) = Pos newX newY
 where
  newX = clamp (rectX - 1) (rectX + w - 2) x
  newY = clamp (rectY - 1) (rectY + h - 2) y
