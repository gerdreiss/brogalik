{-# LANGUAGE TupleSections #-}

module Control.Display where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom

-- | Make a display of given size filled with the given pixel character
mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  Size width height = size
  cellRange         = (Pos 0 0, Pos (width - 1) (height - 1))
  pixels            = array cellRange $ (, pixel) <$> range cellRange

-- | Fill an existing display with the given pixel character
fillDisplay :: Pixel -> Display -> Display
fillDisplay pixel display = _fillRect rect pixel display
 where
  Display size _ = display
  rect           = Rect (Pos 0 0) size

-- | Render the display to a string
renderDisplay :: Display -> String
renderDisplay (Display size@(Size width height) pixels) = unlines
  [ [ pixels ! Pos x y | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

-- | Place the given room in the given display
displayRoom :: Room -> Display -> Display
displayRoom room display = foldl' foldFunc display' items
 where
  foldFunc dspl (Pos itemX itemY, item) =
    displayPixel (Pos (roomX + itemX) (roomY + itemY)) (itemChar item) dspl
  Rect (Pos roomX roomY) _ = roomRect room
  display'                 = _fillRect (roomRect room) roomFloor display
  items                    = M.toList (roomItems room)

-- | Place the given pixel character in the given display
displayPixel :: Pos -> Pixel -> Display -> Display
displayPixel (Pos x y) = _fillRect $ Rect (Pos x y) (Size 1 1)

-- | Fill the given rectangle with the pixel character
_fillRect :: Rect -> Pixel -> Display -> Display
_fillRect rect pixel display = display
  { displayPixels = pixels // do
                      x <- [rectX .. (rectX + rectW - 1)]
                      y <- [rectY .. (rectY + rectH - 1)]
                      return (Pos (x `mod` width) (y `mod` height), pixel)
  }
 where
  Rect    (Pos  rectX rectY ) (Size rectW rectH) = rect
  Display (Size width height) pixels             = display


-- | helper functions, currently unused
_drawVLine :: Pos -> Height -> Pixel -> Display -> Display
_drawVLine (Pos x y) height =
  _fillRect $ Rect (Pos x y) (Size 1 (height - y + 1))

_drawHLine :: Pos -> Width -> Pixel -> Display -> Display
_drawHLine (Pos x y) width =
  _fillRect $ Rect (Pos x y) (Size (width - x + 1) 1)

_drawRect :: Rect -> Pixel -> Display -> Display
_drawRect rect pixel =
  _drawHLine (Pos x y) width pixel
    . _drawHLine (Pos x height) width pixel
    . _drawVLine (Pos x y)     height pixel
    . _drawVLine (Pos width y) height pixel
 where
  (Rect (Pos x y) (Size w h)) = rect
  width                       = x + w - 1
  height                      = y + h - 1
