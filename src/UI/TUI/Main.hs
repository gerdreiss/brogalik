module UI.TUI.Main
  ( tui
  ) where

import qualified UI.TUI.Widgets.Main           as W

import           Brick                   hiding ( Direction
                                                , Size
                                                )
import           Control.Brogalik               ( generateBrogalik )
import           Data.Brogalik
import           Data.Geom
import           Data.Text
import           Graphics.Vty.Input.Events
import           Lens.Micro.GHC
import           Lens.Micro.TH                  ( makeLenses )

type NewState = EventM () (Next AppState)

data AppState = AppState
  { _stateTitle    :: Text
  , _stateStatus   :: Text
  , _stateBrogalik :: Brogalik
  }

makeLenses ''AppState


-- | the module's public function
tui :: Width -> Height -> IO ()
tui width height = defaultMain app initialState >>= printExitStatus
 where
  initialState    = initialAppState (Size width height)
  printExitStatus = putStrLn . unpack . _stateStatus


-- | create the application state
app :: App AppState e ()
app = App { appDraw         = drawTui
          , appChooseCursor = showFirstCursor
          , appStartEvent   = return
          , appHandleEvent  = handleTuiEvent
          , appAttrMap      = const $ attrMap mempty mempty
          }

-- | create the initial AppState
initialAppState :: Size -> AppState
initialAppState size = AppState
  { _stateTitle    = "Brogalik - Roguelike game build with Brick"
  , _stateStatus   = "New game started"
  , _stateBrogalik = generateBrogalik size
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
resizeBrogalik width height appState =
  continue $ appState & updateSize & updateStatus
 where
  updateSize = stateBrogalik . brogalikSize .~ Size width height
  updateStatus =
    stateStatus .~ pack ("Terminal size : " <> show width <> "x" <> show height)

newGame :: AppState -> NewState
newGame = continue . initialAppState . _brogalikSize . _stateBrogalik

movePlayer :: Direction -> AppState -> NewState
movePlayer direction state = continue $ state & moveBrogalik & updateStatus
 where
  moveBrogalik = stateBrogalik %~ brogalikMove direction
  updateStatus = stateStatus .~ pack ("Moving " <> show direction <> "...")

brogalikMove :: Direction -> Brogalik -> Brogalik
brogalikMove direction brogalik = brogalik & updatePos & updateGold
 where
  -- update position
  updatePos = brogalikPlayer . playerPos .~ clampPos (location place) newPos
  location (PlaceRoom index) = brogalik ^?! brogalikRooms . ix index . roomRect
  location (PlacePassage index) =
    toRect $ brogalik ^?! brogalikPassages . ix index . passageLine
  place = brogalik ^. brogalikPlayer . playerPlace
  newPos =
    (brogalik ^. brogalikPlayer . playerPos) |--> directionChanges direction
  -- TODO update gold
  updateGold = brogalikPlayer . playerGold %~ (+) goldFound
  goldFound  = 0

clampPos :: Rect -> Pos -> Pos
clampPos (Rect (Pos rectX rectY) (Size w h)) (Pos x y) = Pos newX newY
 where
  newX = clamp (rectX - 1) (rectX + w - 2) x
  newY = clamp (rectY - 1) (rectY + h - 2) y
