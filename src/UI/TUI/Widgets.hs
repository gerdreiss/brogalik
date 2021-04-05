module UI.TUI.Widgets (title, inventory, gameField, help, status) where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center (hCenter, vCenter)
import Control.Display (renderBrogalik)
import Data.Brogalik
import Data.Geom (Size (Size), minSize)
import qualified Data.List as L
import Data.Text (Text, unpack)
import UI.TUI.State (AppState (stateStatus))

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
    $ vBox [gold, weapons, weaponList]
  where
    gold = str . ("Gold: " <>) . show . playerGold $ player
    weapons = str $ if L.null . playerWeapons $ player then "Weapons: none" else "Weapons: "
    weaponList = padLeft (Pad 1) . vBox . toList . playerWeapons $ player
    toList = fmap $ str . ("* " <>) . show

-- | The game field (center right)
gameField :: Brogalik -> Widget ()
gameField brogalik = do
  let widthReduct = _leftWidth + _leftPaddingV * 2 + 4 -- 4 = number of vertical borders
      heightReduct = _bottomHeight + _topPaddingV * 2 + 7 -- 7 = number of horizontal borders
      sizeReduct = Size widthReduct heightReduct
      gameFieldSize = brogalikSize brogalik - sizeReduct
   in withBorderStyle unicodeBold
        . border
        . padLeft _leftPadding
        . padTop _topPadding
        . padRight Max
        . padBottom Max
        $ if gameFieldSize < minSize
          then strWrap "The terminal size is too small. Please increase."
          else str $ renderBrogalik brogalik gameFieldSize

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
      [ str "a  Move west",
        str "d  Move east",
        str "w  Move north",
        str "s  Move south",
        str "n  New Game",
        str "q  Exit"
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
