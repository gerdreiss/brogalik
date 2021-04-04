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
import           Control.Brogalik
import           Control.Display
import           Data.Brogalik
import           Data.Geom
import           Data.Text
import           UI.TUI.State

import           Control.Geom
import qualified Data.Text                     as T

title :: Text -> Widget ()
title = border . vLimit 1 . vCenter . hCenter . str . unpack

inventory :: Player -> Widget ()
inventory player =
  border
    . hLimit _leftWidth
    . padLeft _leftPadding
    . padTop _topPadding
    . padBottom Max
    . padRight Max
    $ vBox
        [ str . ("Gold: " <>) . show . playerGold $ player
        , str "Weapons:"
        , padLeft (Pad 1)
        . vBox
        . fmap (str . ("* " <>) . show)
        . playerWeapons
        $ player
        ]


gameField :: Brogalik -> Widget ()
gameField brogalik =
  border
    . hBox
    $ [ padLeft _leftPadding
        . padTop _topPadding
        . padRight Max
        . padBottom Max
        . strWrap
        . renderDisplay
        . renderBrogalik brogalik
        $ mkDisplay (reducedSize . brogalikSize $ brogalik) ' '
      ]
 where
  reducedSize size = size ^-^ Size (_leftWidth + _leftPaddingV * 4)
                                   (_bottomHeight + _topPaddingV * 4 + 4)

help :: Widget ()
help =
  border
    . hLimit _leftWidth
    . vLimit _bottomHeight
    . hBox
    $ [ padLeft _leftPadding
        .   padRight Max
        $   str "a  Move west"
        <=> str "d  Move east"
        <=> str "w  Move north"
        <=> str "s  Move south"
        <=> str "n  New Game"
        <=> str "q  Exit"
      ]

status :: AppState -> Widget ()
status =
  border
    . vLimit _bottomHeight
    . padLeft _leftPadding
    . padRight Max
    . padBottom Max
    . strWrap
    . unpack
    . stateStatus

_leftWidth :: Int
_leftWidth = 20

_bottomHeight :: Int
_bottomHeight = 6

_leftPadding :: Padding
_leftPadding = Pad _leftPaddingV

_topPadding :: Padding
_topPadding = Pad _topPaddingV

_leftPaddingV :: Int
_leftPaddingV = 2

_topPaddingV :: Int
_topPaddingV = 1
