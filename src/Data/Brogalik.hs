module Data.Brogalik where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Geom


data Weapon
  = Sword
  | Axe
  deriving (Show)


data Item
  = GoldItem Int
  | WeaponItem Weapon
  deriving (Show)


data Room = Room
  { roomRect  :: Rect
  , roomItems :: M.Map Pos Item
  }
  deriving Show


newtype Index a =
  Index Int
  deriving (Eq, Ord, Ix, Show)


data Player = Player
  { playerRoom    :: Index Room
  , playerPos     :: Pos
  , playerGold    :: Int
  , playerWeapons :: [Weapon]
  }
  deriving Show


data Brogalik = Brogalik
  { brogalikRooms  :: Array (Index Room) Room
  , brogalikPlayer :: Player
  }
  deriving Show


data Display = Display
  { displaySize   :: Size
  , displayPixels :: Array Pos Pixel
  }
  deriving Show
