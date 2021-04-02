module Control.Brogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Brogalik
import           Data.Geom

generateBrogalik :: Brogalik
generateBrogalik = Brogalik
  { brogalikRooms  = array roomsIndexRange $ zip (range roomsIndexRange) rooms
  , brogalikPlayer = Player { playerRoom    = Index 0
                            , playerPos     = Pos 0 0
                            , playerGold    = 0
                            , playerWeapons = []
                            }
  }
 where
  roomsIndexRange = (Index 0, Index (roomsCount - 1))
  rooms =
    [ addItem (Pos 4 4) (GoldItem 69) $ mkRoom $ Rect (Pos 0 0) (Size 10 5)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom $ Rect (Pos 0 20) (Size 5 7)
    , addItem (Pos 2 2) (WeaponItem Sword) $ mkRoom $ Rect (Pos 20 20)
                                                           (Size 10 10)
    ]
  roomsCount = length rooms


mkRoom :: Rect -> Room
mkRoom rect = Room rect mempty

addItem :: Pos -> Item -> Room -> Room
addItem pos item room = room { roomItems = M.insert pos item items }
  where items = roomItems room
