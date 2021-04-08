{-# LANGUAGE TupleSections #-}

module Control.Display where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Brogalik
import           Data.Foldable                  ( Foldable(foldl') )
import           Data.Geom

-- | Make a display of given size filled with the given pixel character
mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  pixels    = array cellRange $ (, pixel) <$> range cellRange
  cellRange = (Pos 0 0, Pos (w - 1) (h - 1))
  Size w h  = size

-- | Fill an existing display with the given pixel character
fillDisplay :: Pixel -> Display -> Display
fillDisplay pixel display = _fillRect rect pixel display
  where rect = Rect (Pos 0 0) (displaySize display)

-- | Render the brogalik into a string
renderBrogalik :: Brogalik -> Size -> String
renderBrogalik brogalik size =
  renderDisplay . displayBrogalik brogalik $ mkDisplay size displayBg

-- | Render the display into a string
renderDisplay :: Display -> String
renderDisplay (Display (Size width height) pixels) = unlines
  [ [ pixels ! Pos x y | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

-- | Display the Brogalik
displayBrogalik :: Brogalik -> Display -> Display
displayBrogalik brogalik = displayPlayer brogalik . displayRooms brogalik

-- | Display the player within the Brogalik
displayPlayer :: Brogalik -> Display -> Display
displayPlayer brogalik = displayPixel playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> playerPos player
  Rect playerRoomPos _ = roomRect (brogalikRooms brogalik ! playerRoom player)
  player               = brogalikPlayer brogalik

-- | Display the rooms within the Brogalik
displayRooms :: Brogalik -> Display -> Display
displayRooms brogalik display = foldl' (flip displayRoom) display roomList
  where roomList = elems (brogalikRooms brogalik)

-- | Place the given room in the given display
displayRoom :: Room -> Display -> Display
displayRoom room display = displayItems room display''
 where
  display'  = _fillRect (roomRect room) roomFloor display
  display'' = _frameRect (roomRect room) '─' '│' display'

-- | Place the items of the given room
displayItems :: Room -> Display -> Display
displayItems room display = foldl' (flip displayItemInRoom) display items
 where
  displayItemInRoom (pos, item) = displayItem item pos room
  items = M.toList $ roomItems room

-- | Place the given item at the given position in the room
displayItem :: Item -> Pos -> Room -> Display -> Display
displayItem item (Pos itemX itemY) room = displayPixel relItemPos
                                                       (itemPixel item)
 where
  relItemPos               = Pos (roomX + itemX) (roomY + itemY)
  Rect (Pos roomX roomY) _ = roomRect room

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

-- | The corner positions of a rectangle
_corners :: Rect -> [Pos]
_corners (Rect (Pos x y) (Size w h)) =
  [Pos x y, Pos (x + w - 1) y, Pos x (y + h - 1), Pos (x + w - 1) (y + h - 1)]

-- |
-- |
-- | helper functions, currently unused
_drawVLine :: Pos -> Height -> Pixel -> Display -> Display
_drawVLine (Pos x y) height =
  _fillRect $ Rect (Pos x y) (Size 1 (height - y + 1))

_drawHLine :: Pos -> Width -> Pixel -> Display -> Display
_drawHLine (Pos x y) width =
  _fillRect $ Rect (Pos x y) (Size (width - x + 1) 1)

_frameRect :: Rect -> Pixel -> Pixel -> Display -> Display
_frameRect rect horizPixel vertPixel =
  _drawHLine (Pos (x - 1) (y - 1)) horizLength horizPixel
    . _drawHLine (Pos (x - 1) vertLength) horizLength horizPixel
    . _drawVLine (Pos (x - 1) (y - 1))     vertLength vertPixel
    . _drawVLine (Pos horizLength (y - 1)) vertLength vertPixel
 where
  (Rect (Pos x y) (Size w h)) = rect
  horizLength                 = x + w
  vertLength                  = y + h
