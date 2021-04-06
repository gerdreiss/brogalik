module Control.Brogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom
import           Data.StateT

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
    [ addItem (Pos 4 3) (GoldItem 69) $ mkRoom (Pos 0 0) (Size 10 5)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom (Pos 0 20) (Size 12 7)
    , addItem (Pos 2 2) (WeaponItem Sword) $ mkRoom (Pos 20 20) (Size 10 5)
    ]

mkRoom :: Pos -> Size -> Room
mkRoom pos size = Room (Rect pos size) mempty

addItemT :: Pos -> Item -> StateT Room ()
addItemT pos item = StateT
  $ \room -> (room { roomItems = M.insert pos item (roomItems room) }, ())

addItem :: Pos -> Item -> Room -> Room
addItem pos item room = room { roomItems = M.insert pos item (roomItems room) }

brogalikMoveT :: Direction -> StateT Brogalik ()
brogalikMoveT direction = StateT $ \brogalik ->
  ( brogalik
    { brogalikPlayer = fst $ runStateT (playerMoveT direction)
                                       (brogalikPlayer brogalik)
    }
  , ()
  )

brogalikMove :: Direction -> Brogalik -> Brogalik
brogalikMove direction brogalik =
  brogalik { brogalikPlayer = playerMove direction (brogalikPlayer brogalik) }

playerMoveT :: Direction -> StateT Player ()
playerMoveT direction = StateT $ \player -> (playerMove direction player, ())

playerMove :: Direction -> Player -> Player
playerMove direction player = player
  { playerPos = newPos (playerPos player) (directionChanges direction)
  }
  where newPos (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)
