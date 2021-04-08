module UI.TUI.Widgets
  ( title
  , inventory
  , gameField
  , help
  , status
  ) where

import qualified Data.List                     as L

import           Brick                   hiding ( Size )
import           Brick.Widgets.Border           ( border )
import           Brick.Widgets.Border.Style     ( unicodeBold )
import           Brick.Widgets.Center           ( hCenter
                                                , vCenter
                                                )
import           Control.Display                ( renderBrogalik )
import           Data.Brogalik                  ( Brogalik(..)
                                                , Player(..)
                                                )
import           Data.Geom                      ( Height
                                                , Size(..)
                                                , Width
                                                )
import           Data.Text                      ( Text
                                                , unpack
                                                )
import           UI.TUI.State                   ( AppState(..) )

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
    $ vBox [gold, weaponsLabel, weaponsList]
 where
  gold         = str . ("Gold: " <>) . show . playerGold $ player
  weaponsLabel = str
    $ if L.null . playerWeapons $ player then "Weapons: none" else "Weapons: "
  weaponsList = padLeft (Pad 1) . vBox . toList . playerWeapons $ player
  toList      = fmap $ str . ("* " <>) . show

-- | The game field (center right)
gameField :: Brogalik -> Widget ()
gameField brogalik =
  let terminalSize  = brogalikSize brogalik
      gameFieldSize = _gameFieldSize (brogalikSize brogalik)
  in  withBorderStyle unicodeBold
        . border
        . padLeft _leftPadding
        . padTop _topPadding
        . padRight Max
        . padBottom Max
        $ if minRequired gameFieldSize
            then renderGame gameFieldSize
            else renderWarning terminalSize gameFieldSize
 where
  minRequired (Size w h) = w >= _minGameFieldWidth && h >= _minGameFieldHeight
  renderGame size = str $ renderBrogalik brogalik size
  renderWarning terminalSize gameFieldSize =
    str
      . L.concat
      $ [ "Terminal too small!"
        , "\n"
        , "Terminal size = "
        , show terminalSize
        , "\n"
        , "Game field size = "
        , show gameFieldSize
        , "\n"
        , "Minimal game field size = 80x30"
        ]

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

_minGameFieldWidth :: Width
_minGameFieldWidth = 80

_minGameFieldHeight :: Height
_minGameFieldHeight = 30

_gameFieldSize :: Size -> Size
_gameFieldSize (Size w h) = Size
  (w - (_leftWidth + _leftPaddingV + _leftPaddingV + 4)) -- 4 = number of vertical borders
  (h - (_bottomHeight + _topPaddingV + _topPaddingV + 7)) -- 7 = number of horizontal borders
