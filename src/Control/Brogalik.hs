module Control.Brogalik
  ( generateBrogalik
  , brogalikMove
  ) where

import qualified Data.Map                      as M

import           Data.Array                     ( Ix(range)
                                                , array
                                                )
import           Data.Brogalik
import           Data.Geom

generateBrogalik :: Size -> Brogalik
generateBrogalik size = Brogalik
  { brogalikSize   = size
  , brogalikRooms  = array indexRange $ zip (range indexRange) rooms
  , brogalikPlayer = Player { playerRoom    = Index 0
                            , playerPos     = Pos 0 0
                            , playerGold    = 0
                            , playerWeapons = []
                            }
  }
 where
  indexRange = (Index 0, Index (length rooms - 1))
  rooms =
    [ addItem (Pos 4 3) (GoldItem 69) $ mkRoom (Pos 1 1) (Size 16 5)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 48 10) (Size 12 6)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 4 16) (Size 12 6)
    , addItem (Pos 2 2) (WeaponItem Sword) $ mkRoom (Pos 54 30) (Size 10 5)
    ]

mkRoom :: Pos -> Size -> Room
mkRoom pos size = Room (Rect pos size) mempty

addItem :: Pos -> Item -> Room -> Room
addItem pos item room = room { roomItems = M.insert pos item (roomItems room) }


brogalikMove :: Direction -> Brogalik -> Brogalik
brogalikMove direction brogalik =
  brogalik { brogalikPlayer = playerMove direction (brogalikPlayer brogalik) }

playerMove :: Direction -> Player -> Player
playerMove direction player = player
  { playerPos  = newPos (playerPos player) (directionChanges direction)
  , playerGold = playerGold player + goldFound
  }
 where
  newPos (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)
  goldFound = 0
