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
  , brogalikSize   :: Size
  }
  deriving Show


data Display = Display
  { displaySize   :: Size
  , displayPixels :: Array Pos Pixel
  }
  deriving Show


--instance Semigroup Display where
--  l <> r = Display newSize pixels
--   where
--    newSize   = displaySize l <> displaySize r
--    pixels    = array cellRange $ (, pixel) <$> range cellRange
--    cellRange = (Pos 0 0, Pos (w - 1) (h - 1))
--    pixel     = displayPixels l ! Pos 0 0
--    Size w h  = newSize

--instance Monoid Display where
--  mempty = Display (Size 0 0) pixels
--   where
--    pixels    = array cellRange [(Pos 0 0, displayBg)]
--    cellRange = (Pos 0 0, Pos 0 0)



displayBg :: Char
displayBg = ' '


roomFloor :: Pixel
roomFloor = '.'


itemChar :: Item -> Char
itemChar (GoldItem   _    ) = '$'
itemChar (WeaponItem Axe  ) = 'T'
itemChar (WeaponItem Sword) = '!'

