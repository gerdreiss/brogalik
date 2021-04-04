module UI.TUI.Widgets
  ( title
  , inventory
  , gameField
  , help
  , status
  ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Data.Text
import           UI.TUI.State


title :: Text -> Widget ()
title = border . vLimit 1 . vCenter . hCenter . str . unpack


inventory :: AppState -> Widget ()
inventory s =
  border
    . hLimit _leftWidth
    . padLeft (Pad 3)
    . padTop (Pad 1)
    . padBottom Max
    . strWrap
    $ "Inventory"


gameField :: AppState -> Widget ()
gameField s =
  border
    . hBox
    $ [ padLeft (Pad 3)
        . padTop (Pad 1)
        . padRight Max
        . padBottom Max
        . strWrap
        $ "Game"
      ]

help :: Widget ()
help =
  border
    . hLimit _leftWidth
    . vLimit _bottomHeight
    . hBox
    $ [ padLeft (Pad 3)
        . padRight Max
        . vBox
        $ [ str "a  Move west"
          , str "d  Move east"
          , str "w  Move north"
          , str "s  Move south"
          , str "n  New Game"
          , str "q  Exit"
          ]
      ]

status :: AppState -> Widget ()
status =
  border
    . vLimit _bottomHeight
    . padLeft (Pad 3)
    . padRight Max
    . padBottom Max
    . strWrap
    . unpack
    . stateStatus


_leftWidth :: Int
_leftWidth = 20

_bottomHeight :: Int
_bottomHeight = 6
