module Data.Geom where

import           Data.Array                     ( Ix )

-- | Types
type Pixel = Char

type X = Int
type Y = Int

-- | position in the game field, starting in the top left corner
data Pos = Pos X Y
  deriving (Eq, Ord, Ix, Show)

type Width = Int
type Height = Int

data Size = Size Width Height
  deriving Show

data Rect = Rect Pos Size
  deriving Show

data Direction = West
               | East
               | North
               | South
  deriving (Eq, Ord, Ix, Show)

data PosDelta = PosDelta X Y

-- | Instances
instance Eq Size where
  (Size wl hl) == (Size wr hr) = wl == wr && hl == hr

instance Ord Size where
  (Size wl hl) <= (Size wr hr) = wl <= wr && hl <= hr

instance Semigroup Size where
  (Size w1 h1) <> (Size w2 h2) = Size w2 newH
   where
    newH = round $ fromIntegral h2 + (fromIntegral (w1 * h1) / fromIntegral w2)

instance Monoid Size where
  mempty = Size 0 0

instance Semigroup Pos where
  (Pos col1 row1) <> (Pos col2 row2) = Pos (col1 + col2) (row1 + row2)

instance Monoid Pos where
  mempty = Pos 0 0

-- | Helper functions
directionChanges :: Direction -> PosDelta
directionChanges West  = PosDelta (-1) 0
directionChanges East  = PosDelta 1 0
directionChanges North = PosDelta 0 (-1)
directionChanges South = PosDelta 0 1

