module Data.Geom where

import           Data.Array

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
  deriving (Eq, Ord, Show)

data Rect = Rect Pos Size
  deriving (Eq, Ord, Show)

data Direction
  = West
  | East
  | North
  | South
  deriving (Eq, Ord, Ix, Show)

data PosDelta = PosDelta X Y


-- | Instances

instance Semigroup Size where
  (<>) (Size w1 h1) (Size w2 h2) = Size (w1 + w2) (h1 + h2)

instance Monoid Size where
  mempty = Size 0 0

instance Semigroup Pos where
  (<>) (Pos col1 row1) (Pos col2 row2) = Pos (col1 + col2) (row1 + row2)

instance Monoid Pos where
  mempty = Pos 0 0
