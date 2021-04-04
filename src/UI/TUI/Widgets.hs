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
import           Data.Brogalik
import           Data.Text
import           UI.TUI.State


title :: Text -> Widget ()
title = border . vLimit 1 . vCenter . hCenter . str . unpack


inventory :: Player -> Widget ()
inventory player =
  border
    . hLimit _leftWidth
    . padLeft _padL
    . padTop _padT
    . padBottom Max
    . padRight Max
    $ vBox
        [ str ("Gold: " <> gold)
        , str "Weapons:"
        , padLeft (Pad 1) . vBox $ fmap (str . ("- " <>)) weapons
        ]
 where
  gold    = show . playerGold $ player
  weapons = fmap show . playerWeapons $ player


gameField :: AppState -> Widget ()
gameField s =
  border
    . hBox
    $ [ padLeft _padL
        . padTop _padT
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
    $ [ padLeft _padL
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
    . padLeft _padL
    . padRight Max
    . padBottom Max
    . strWrap
    . unpack
    . stateStatus


_leftWidth :: Int
_leftWidth = 20

_bottomHeight :: Int
_bottomHeight = 6

_padL :: Padding
_padL = Pad 3

_padT :: Padding
_padT = Pad 1
