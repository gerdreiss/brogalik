module Data.Geom where

import           Data.Array                     ( Ix )
import           Lens.Micro.GHC                 ( (^.) )
import           Lens.Micro.TH                  ( makeLenses )

-- | Types
type Pixel = Char

type X = Int
type Y = Int

type Length = Int
type Width = Int
type Height = Int

-- | position in the game field, starting in the top left corner
data Pos = Pos
  { _x :: X
  , _y :: Y
  }
  deriving (Eq, Ord, Ix)

instance Show Pos where
  show (Pos x y) = concat ["(", show x, ",", show y, ")"]

data Size = Size
  { _width  :: Width
  , _height :: Height
  }

instance Show Size where
  show (Size w h) = show w <> "x" <> show h

data Orientation
  = Vertical
  | Horizontal
  deriving (Show)

data Line = Line
  { _lineStart       :: Pos
  , _lineLength      :: Length
  , _lineOrientation :: Orientation
  }
  deriving Show

data Rect = Rect
  { _rectPos  :: Pos
  , _rectSize :: Size
  }
  deriving Show

data Direction
  = West
  | East
  | North
  | South
  deriving (Eq, Ord, Ix, Show)

data PosDelta = PosDelta X Y

makeLenses ''Pos
makeLenses ''Size
makeLenses ''Line
makeLenses ''Rect

-- | Instances
instance Eq Size where
  (Size wl hl) == (Size wr hr) = wl == wr && hl == hr

instance Ord Size where
  (Size wl hl) <= (Size wr hr) = wl <= wr && hl <= hr

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

instance Semigroup Rect where
  (Rect pos1 size1) <> (Rect pos2 size2) = Rect (pos1 <> pos2) (size1 <> size2)

instance Monoid Rect where
  mempty = Rect (Pos 0 0) (Size 0 0)


-- | Helper functions
directionChanges :: Direction -> PosDelta
directionChanges West  = PosDelta (-1) 0
directionChanges East  = PosDelta 1 0
directionChanges North = PosDelta 0 (-1)
directionChanges South = PosDelta 0 1

(|-->) :: Pos -> PosDelta -> Pos
(|-->) (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)

toRect :: Line -> Rect
toRect line = Rect pos size
 where
  pos  = line ^. lineStart
  size = case line ^. lineOrientation of
    Horizontal -> Size (line ^. lineLength) 1
    Vertical   -> Size 1 (line ^. lineLength)
