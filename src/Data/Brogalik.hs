module Data.Brogalik where

import qualified Data.Map                      as M

import           Data.Array                     ( Array
                                                , Ix
                                                )
import           Data.Geom                      ( Pixel
                                                , Pos
                                                , Rect
                                                , Size
                                                )


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
  , brogalikSize   :: Size
  }
  deriving Show


data Display = Display
  { displaySize   :: Size
  , displayPixels :: Array Pos Pixel
  }
  deriving Show


itemChar :: Item -> Char
itemChar (GoldItem   _    ) = '$'
itemChar (WeaponItem Axe  ) = 'T'
itemChar (WeaponItem Sword) = '!'


roomFloor :: Pixel
roomFloor = '.'
