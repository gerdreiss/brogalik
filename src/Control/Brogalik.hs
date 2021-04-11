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
    [ addItem (Pos 4 3) (GoldItem 69) $ mkRoom (Pos 1 1) (Size 16 5)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 12 12) (Size 12 6)
    , addItem (Pos 2 2) (GoldItem 1) $ mkRoom (Pos 4 24) (Size 12 6)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 56 2) (Size 12 5)
    , addItem (Pos 2 2) (WeaponItem Sword) $ mkRoom (Pos 54 20) (Size 10 5)
    ]
  -- passages  
  passagesArray =
    array passageIndexRange $ zip (range passageIndexRange) passages
  passageIndexRange = (Index 0, Index (length passages - 1))
  passages =
    [ mkPassage (PlaceRoom (Index 0))
                (PlaceRoom (Index 3))
                (head rooms)
                (rooms !! 3)
    ]

mkRoom :: Pos -> Size -> Room
mkRoom pos size = Room { _roomRect = Rect pos size, _roomItems = mempty }

addItem :: Pos -> Item -> Room -> Room
addItem pos item = over roomItems (M.insert pos item)


-- | make a passage  
-- * from room to room
-- * from room to passage
-- * from passage to room
-- * from passage to passage
mkPassage :: Place -> Place -> Room -> Room -> Passage
mkPassage leftPlace rightPlace leftRoom rightRoom = Passage
  { _passageLine       = Line { _lineStart       = Pos 17 3
                              , _lineLength      = 39
                              , _lineOrientation = Horizontal
                              }
  , _passageLeftPlace  = leftPlace
  , _passageRightPlace = rightPlace
  }
