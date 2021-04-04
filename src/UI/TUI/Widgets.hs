module UI.TUI.Widgets
  ( title
  , inventory
  , gameField
  , help
  , status
  ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Control.Brogalik
import           Control.Display
import           Data.Brogalik
import           Data.Geom
import           Data.Text
import           UI.TUI.State

-- | The title widget (top)
title :: Text -> Widget ()
title =
  withBorderStyle unicodeBold
    . border
    . vLimit 1
    . vCenter
    . hCenter
    . str
    . unpack

-- | The inventory widget (center left)
inventory :: Player -> Widget ()
inventory player =
  withBorderStyle unicodeBold
    . border
    . hLimit _leftWidth
    . padLeft _leftPadding
    . padTop _topPadding
    . padBottom Max
    . padRight Max
    $ vBox [gold, str "Weapons:", weaponList]
 where
  gold = str . ("Gold: " <>) . show . playerGold $ player
  weaponList =
    padLeft (Pad 1)
      . vBox
      . fmap (str . ("* " <>) . show)
      . playerWeapons
      $ player

-- | The game field (center right)
gameField :: Brogalik -> Widget ()
gameField brogalik =
  withBorderStyle unicodeBold
    . border
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
  reducedSize size = size - Size (_leftWidth + _leftPaddingV * 2 + 4)
                                 (_bottomHeight + _topPaddingV * 2 + 7)

-- | The help widget (bottom left)
help :: Widget ()
help =
  withBorderStyle unicodeBold
    . border
    . hLimit _leftWidth
    . vLimit _bottomHeight
    . padLeft _leftPadding
    . padRight Max
    $ vBox
        [ str "a  Move west"
        , str "d  Move east"
        , str "w  Move north"
        , str "s  Move south"
        , str "n  New Game"
        , str "q  Exit"
        ]

-- | The status widget (bottom right)
status :: AppState -> Widget ()
status =
  withBorderStyle unicodeBold
    . border
    . vLimit _bottomHeight
    . padLeft _leftPadding
    . padRight Max
    . padBottom Max
    . strWrap
    . unpack
    . stateStatus

-- | Constants

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
