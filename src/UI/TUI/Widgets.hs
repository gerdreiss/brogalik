module UI.TUI.Widgets (title, inventory, gameField, help, status) where

import qualified Data.List as L
import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Control.Display
import           Data.Brogalik
import           Data.Geom
import           Data.Text
import           UI.TUI.State

-- | The title widget (top)
title :: Text -> Widget ()
title = withBorderStyle unicodeBold
  . border
  . vLimit 1
  . vCenter
  . hCenter
  . str
  . unpack

-- | The inventory widget (center left)
inventory :: Player -> Widget ()
inventory player = withBorderStyle unicodeBold
  . border
  . hLimit _leftWidth
  . padLeft _leftPadding
  . padTop _topPadding
  . padBottom Max
  . padRight Max
  $ vBox [gold, weaponsLabel, weaponList]
  where
    gold = str . ("Gold: " <>) . show . playerGold $ player

    weaponsLabel = str
      $ if L.null . playerWeapons $ player
        then "Weapons: none"
        else "Weapons: "

    weaponList = padLeft (Pad 1) . vBox . toList . playerWeapons $ player

    toList = fmap $ str . ("* " <>) . show

-- | The game field (center right)
gameField :: Brogalik -> Widget ()
gameField brogalik = do
  let Size w h = brogalikSize brogalik
      gameFieldW = _gameFieldWidth w
      gameFieldH = _gameFieldHeight h
    in withBorderStyle unicodeBold
       . border
       . padLeft _leftPadding
       . padTop _topPadding
       . padRight Max
       . padBottom Max
       $ if _gameFieldMinWidth <= gameFieldW
           && _gameFieldMinHeight <= gameFieldH
         then str $ renderBrogalik brogalik (Size gameFieldW gameFieldH)
         else str . L.concat
           $ [ "Terminal too small!"
             , "\nTerminal size = "
             , show w
             , "x"
             , show h
             , "\nGame field = "
             , show gameFieldW
             , "x"
             , show gameFieldH
             , "\nGame field min size = 80x30"]

-- | The help widget (bottom left)
help :: Widget ()
help = withBorderStyle unicodeBold
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
    , str "q  Exit"]

-- | The status widget (bottom right)
status :: AppState -> Widget ()
status = withBorderStyle unicodeBold
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

_gameFieldMinWidth :: Int
_gameFieldMinWidth = 80

_gameFieldMinHeight :: Int
_gameFieldMinHeight = 30

_gameFieldWidth :: Width -> Width
_gameFieldWidth w = w - (_leftWidth + _leftPaddingV + _leftPaddingV + 4) -- 4 = number of vertical borders

_gameFieldHeight :: Height -> Height
_gameFieldHeight h = h - (_bottomHeight + _topPaddingV + _topPaddingV + 7) -- 7 = number of horizontal borders
