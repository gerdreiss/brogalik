{-# LANGUAGE TupleSections #-}

module UI.TUI.Widgets.Game
  ( game
  ) where

import qualified Data.Map                      as M

import           Brick                   hiding ( Horizontal
                                                , Size
                                                , Vertical
                                                )
import           Brick.Widgets.Border.Style
import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom
import           Lens.Micro.GHC

-- | The game field (center right)
game :: Brogalik -> Size -> Widget ()
game brogalik size = str $ renderBrogalik brogalik size

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
displayPlayer brogalik = drawPixel playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> brogalik ^. brogalikPlayer . playerPos
  Rect playerRoomPos _ = brogalik ^?! brogalikRooms . ix roomIndex . roomRect
  roomIndex            = brogalik ^. brogalikPlayer . playerRoom

-- | Display the rooms within the Brogalik
displayRooms :: Brogalik -> Display -> Display
displayRooms brogalik display =
  foldl' (flip displayRoom) display (brogalik ^. brogalikRooms)

-- | Place the given room in the given display
displayRoom :: Room -> Display -> Display
displayRoom room =
  displayItems room
    . fillRect (room ^. roomRect) roomFloor
    . frameRect (room ^. roomRect) unicode

-- | Place the items of the given room
displayItems :: Room -> Display -> Display
displayItems room display = foldl' doDisplay display items
 where
  doDisplay = flip . uncurry . displayItem $ room
  items     = M.toList $ _roomItems room

-- | Place the given item at the given position in the room
displayItem :: Room -> Pos -> Item -> Display -> Display
displayItem (Room (Rect roomPos _) _) itemPos item =
  drawPixel (roomPos <> itemPos) (itemPixel item)

-- | Draw borders around the given rectangle
frameRect :: Rect -> BorderStyle -> Display -> Display
frameRect (Rect (Pos x y) (Size w h)) borderStyle =
  drawPixel (Pos (x - 1) (y - 1)) (bsCornerTL borderStyle)
    . drawPixel (Pos (x + w) (y - 1)) (bsCornerTR borderStyle)
    . drawPixel (Pos (x - 1) (y + h)) (bsCornerBL borderStyle)
    . drawPixel (Pos (x + w) (y + h)) (bsCornerBR borderStyle)
    . drawLine (Pos x (y - 1)) (x + w - 1) Horizontal (bsHorizontal borderStyle)
    . drawLine (Pos x (y + h)) (x + w - 1) Horizontal (bsHorizontal borderStyle)
    . drawLine (Pos (x + w) y) (y + h - 1) Vertical   (bsVertical borderStyle)
    . drawLine (Pos (x - 1) y) (y + h - 1) Vertical   (bsVertical borderStyle)

-- | draw a pixel
drawPixel :: Pos -> Pixel -> Display -> Display
drawPixel (Pos x y) = fillRect $ Rect (Pos x y) (Size 1 1)

-- | draw a line
drawLine :: Pos -> Length -> Orientation -> Pixel -> Display -> Display
drawLine (Pos x y) length Horizontal =
  fillRect $ Rect (Pos x y) (Size (length - x + 1) 1)
drawLine (Pos x y) length Vertical =
  fillRect $ Rect (Pos x y) (Size 1 (length - y + 1))

-- | Fill the given rectangle with the pixel character
fillRect :: Rect -> Pixel -> Display -> Display
fillRect rect pixel display = display
  { _displayPixels = pixels // do
                       x <- [rectX .. (rectX + rectW - 1)]
                       y <- [rectY .. (rectY + rectH - 1)]
                       return (Pos (x `mod` width) (y `mod` height), pixel)
  }
 where
  Rect    (Pos  rectX rectY ) (Size rectW rectH) = rect
  Display (Size width height) pixels             = display

-- | Make a display of given size filled with the given pixel character
mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  pixels    = array cellRange $ (, pixel) <$> range cellRange
  cellRange = (Pos 0 0, Pos (w - 1) (h - 1))
  Size w h  = size


