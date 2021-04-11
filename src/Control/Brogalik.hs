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
  { _brogalikSize   = size
  , _brogalikRooms  = array indexRange $ zip (range indexRange) rooms
  , _brogalikPlayer = Player { _playerRoom    = Index 0
                             , _playerPos     = Pos 0 0
                             , _playerGold    = 0
                             , _playerWeapons = []
                             }
  }
 where
  indexRange = (Index 0, Index (length rooms - 1))
  rooms =
    [ addItem (Pos 4 3) (GoldItem 69) $ mkRoom (Pos 1 1) (Size 16 5)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 48 10) (Size 12 6)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 4 16) (Size 12 6)
    , addItem (Pos 2 2) (WeaponItem Sword) $ mkRoom (Pos 64 22) (Size 10 5)
    ]

mkRoom :: Pos -> Size -> Room
mkRoom pos size = Room (Rect pos size) mempty

addItem :: Pos -> Item -> Room -> Room
addItem pos item = over roomItems (M.insert pos item)
