module Data.Geom where

import           Data.Array                     ( Ix )

-- | Types
type Pixel = Char
type Length = Int

data Orientation = Vertical | Horizontal

type X = Int
type Y = Int

-- | position in the game field, starting in the top left corner
data Pos = Pos X Y
  deriving (Eq, Ord, Ix)

type Width = Int
type Height = Int

data Size = Size Width Height

data Rect = Rect Pos Size
  deriving Show

data Direction = West
               | East
               | North
               | South
  deriving (Eq, Ord, Ix, Show)

data PosDelta = PosDelta X Y

-- | Instances
instance Show Pos where
  show (Pos x y) = concat ["(", show x, ",", show y, ")"]

instance Eq Size where
  (Size wl hl) == (Size wr hr) = wl == wr && hl == hr

instance Ord Size where
  (Size wl hl) <= (Size wr hr) = wl <= wr && hl <= hr

instance Show Size where
  show (Size w h) = show w <> "x" <> show h

instance Semigroup Size where
  (Size w1 h1) <> (Size w2 h2) = Size newW h1
   where
    newW = round $ _w1 + _w2 * _h2 / _h1
    _w1  = fromIntegral w1 :: Double
    _h1  = fromIntegral h1 :: Double
    _w2  = fromIntegral w2 :: Double
    _h2  = fromIntegral h2 :: Double

instance Monoid Size where
  mempty = Size 0 0

instance Semigroup Pos where
  (Pos xl rl) <> (Pos xr rr) = Pos (xl + xr) (rl + rr)

instance Monoid Pos where
  mempty = Pos 0 0

-- | Helper functions
directionChanges :: Direction -> PosDelta
directionChanges West  = PosDelta (-1) 0
directionChanges East  = PosDelta 1 0
directionChanges North = PosDelta 0 (-1)
directionChanges South = PosDelta 0 1

(|-->) :: Pos -> PosDelta -> Pos
(|-->) (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)
