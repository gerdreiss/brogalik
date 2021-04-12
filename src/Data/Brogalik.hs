module Data.Brogalik where

import qualified Data.Map                      as M

import           Data.Array                     ( Array
                                                , Ix
                                                )
import           Data.Geom
import           Lens.Micro.TH                  ( makeLenses )

data Weapon
  = Sword
  | Axe
  deriving (Show)

data Item
  = GoldItem Int
  | WeaponItem Weapon
  deriving (Show)

newtype Index a =
  Index Int
  deriving (Eq, Ord, Ix, Show)

data Place
  = PlaceRoom (Index Room)
  | PlacePassage (Index Passage)
  deriving (Show)

data Passage = Passage
  { _passageLine  :: Line
  , _passageNbors :: M.Map Pos Place
  }
  deriving Show

data Room = Room
  { _roomRect  :: Rect
  , _roomItems :: M.Map Pos Item
  }
  deriving Show

data Player = Player
  { _playerPlace   :: Place
  , _playerPos     :: Pos
  , _playerGold    :: Int
  , _playerWeapons :: [Weapon]
  }
  deriving Show

data Brogalik = Brogalik
  { _brogalikRooms    :: Array (Index Room) Room
  , _brogalikPassages :: Array (Index Passage) Passage
  , _brogalikPlayer   :: Player
  , _brogalikSize     :: Size
  }
  deriving Show

data Display = Display
  { _displaySize   :: Size
  , _displayPixels :: Array Pos Pixel
  }
  deriving Show

makeLenses ''Passage
makeLenses ''Room
makeLenses ''Player
makeLenses ''Brogalik
makeLenses ''Display


displayBg :: Pixel
displayBg = ' '

roomFloor :: Pixel
roomFloor = '.'

itemPixel :: Item -> Pixel
itemPixel (GoldItem   _    ) = '$'
itemPixel (WeaponItem Axe  ) = 'T'
itemPixel (WeaponItem Sword) = '!'

line :: X -> Y -> Length -> Orientation -> Line
line x y = Line (Pos x y)

placeRoom :: Int -> Place
placeRoom index = PlaceRoom (Index index)

room :: X -> Y -> Width -> Height -> Room
room x y w h =
  Room { _roomRect = Rect (Pos x y) (Size w h), _roomItems = mempty }


--instance Semigroup Display where
--  l <> r = Display newSize pixels
--   where
--    newSize   = _displaySize l <> _displaySize r
--    pixels    = array cellRange $ (, pixel) <$> range cellRange
--    cellRange = (Pos 0 0, Pos (w - 1) (h - 1))
--    pixel     = _displayPixels l ! Pos 0 0
--    Size w h  = newSize
--instance Monoid Display where
--  mempty = Display (Size 0 0) pixels
--   where
--    pixels    = array cellRange [(Pos 0 0, displayBg)]
--    cellRange = (Pos 0 0, Pos 0 0)
