{-# LANGUAGE TupleSections #-}

module Control.Display where

import qualified Data.Map                      as M

import           Control.Brogalik
import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom

mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  Size width height = size
  cellRange         = (Pos 0 0, Pos (width - 1) (height - 1))
  pixels            = array cellRange $ (, pixel) <$> range cellRange

renderDisplay :: Display -> String
renderDisplay (Display size@(Size width height) pixels) = unlines
  [ [ pixels ! Pos x y | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

fillDisplay :: Pixel -> Display -> Display
fillDisplay pixel display = fillRect rect pixel display
 where
  Display size _ = display
  rect           = Rect (Pos 0 0) size

fillRect :: Rect -> Pixel -> Display -> Display
fillRect rect pixel display = display
  { displayPixels = pixels // do
                      x <- [rectX .. (rectX + rectW - 1)]
                      y <- [rectY .. (rectY + rectH - 1)]
                      return (Pos (x `mod` width) (y `mod` height), pixel)
  }
 where
  Rect    (Pos  rectX rectY ) (Size rectW rectH) = rect
  Display (Size width height) pixels             = display

drawPixel :: Pos -> Pixel -> Display -> Display
drawPixel (Pos x y) = fillRect $ Rect (Pos x y) (Size 1 1)

drawVLine :: Pos -> Height -> Pixel -> Display -> Display
drawVLine (Pos x y) height =
  fillRect $ Rect (Pos x y) (Size 1 (height - y + 1))

drawHLine :: Pos -> Width -> Pixel -> Display -> Display
drawHLine (Pos x y) width = fillRect $ Rect (Pos x y) (Size (width - x + 1) 1)

drawRect :: Rect -> Pixel -> Display -> Display
drawRect rect pixel =
  drawHLine (Pos x y) width pixel
    . drawHLine (Pos x height) width pixel
    . drawVLine (Pos x y)     height pixel
    . drawVLine (Pos width y) height pixel
 where
  (Rect (Pos x y) (Size w h)) = rect
  width                       = x + w - 1
  height                      = y + h - 1

drawRoom :: Room -> Display -> Display
drawRoom room display =
  let items    = M.toList (roomItems room)
      display' = fillRect rect roomFloor display
  in  foldl' folderF display' items
 where
  rect                     = roomRect room
  Rect (Pos roomX roomY) _ = rect
  folderF dspl (Pos itemX itemY, item) =
    drawPixel (Pos (roomX + itemX) (roomY + itemY)) (itemChar item) dspl
