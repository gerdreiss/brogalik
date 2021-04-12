module Control.Brogalik
  ( generateBrogalik
  ) where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Brogalik
import           Data.Geom
import           Lens.Micro.GHC


generateBrogalik :: Size -> Brogalik
generateBrogalik size = Brogalik
  { _brogalikSize     = size
  , _brogalikRooms    = roomsArray
  , _brogalikPassages = passagesArray
  , _brogalikPlayer   = Player { _playerPlace   = PlaceRoom (Index 0)
                               , _playerPos     = Pos 0 0
                               , _playerGold    = 0
                               , _playerWeapons = []
                               }
  }
 where
  -- rooms 
  roomsArray     = array roomIndexRange $ zip (range roomIndexRange) rooms
  roomIndexRange = (Index 0, Index (length rooms - 1))
  rooms =
    [ addItem 4 3 (GoldItem 69) $ room 1 1 16 5
    , addItem 2 2 (WeaponItem Axe) $ room 12 12 12 6
    , addItem 2 2 (GoldItem 1) $ room 4 24 12 6
    , addItem 2 2 (WeaponItem Axe) $ room 56 2 12 5
    , addItem 2 2 (WeaponItem Sword) $ room 54 20 10 5
    ]
  -- passages  
  passagesArray =
    array passageIndexRange $ zip (range passageIndexRange) passages
  passageIndexRange = (Index 0, Index (length passages - 1))
  passages =
    [ Passage (line 17 3 39 Horizontal)  (placeRoom 0) (placeRoom 3)
    , Passage (line 57 7 13 Vertical)    (placeRoom 0) (placeRoom 3)
    , Passage (line 16 24 38 Horizontal) (placeRoom 0) (placeRoom 3)
    , Passage (line 14 18 6 Vertical)    (placeRoom 0) (placeRoom 3)
    ]

addItem :: X -> Y -> Item -> Room -> Room
addItem x y item = over roomItems (M.insert (Pos x y) item)

