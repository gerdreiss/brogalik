{-# LANGUAGE TupleSections #-}

module UI.TUI.Widgets.Game
  ( game
  ) where

import qualified Data.Map                      as M

import           Brick                          ( Widget
                                                , str
                                                )
import           Brick.Widgets.Border.Style
import           Control.Monad.State
import           Data.Array
import           Data.Brogalik
import           Data.Geom
import           Lens.Micro.GHC

-- | The game field (center right)
game :: Brogalik -> Size -> Widget ()
game brogalik size =
  str $ evalState (renderBrogalik brogalik) (mkDisplay size displayBg)

-- | Render the brogalik into a string
renderBrogalik :: Brogalik -> State Display String
renderBrogalik brogalik = do
  displayBrogalik brogalik
  renderDisplay

-- | Render the display into a string
renderDisplay :: State Display String
renderDisplay = do
  (Display (Size width height) pixels) <- get
  return $ unlines
    [ [ pixels ! Pos x y | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

-- | Display the Brogalik
displayBrogalik :: Brogalik -> State Display ()
displayBrogalik brogalik = do
  displayRooms brogalik
  displayPlayer brogalik

-- | Display the player within the Brogalik
displayPlayer :: Brogalik -> State Display ()
displayPlayer brogalik = drawPixel playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> brogalik ^. brogalikPlayer . playerPos
  Rect playerRoomPos _ = brogalik ^?! brogalikRooms . ix roomIndex . roomRect
  roomIndex            = brogalik ^. brogalikPlayer . playerRoom

-- | Display the rooms within the Brogalik
displayRooms :: Brogalik -> State Display ()
displayRooms brogalik = mapM_ displayRoom (brogalik ^. brogalikRooms)

-- | Place the given room in the given display
displayRoom :: Room -> State Display ()
displayRoom room = do
  frameRect (room ^. roomRect) unicode
  fillRect (room ^. roomRect) roomFloor
  displayItems room

-- | Place the items of the given room
displayItems :: Room -> State Display ()
displayItems room = mapM_ doDisplay items
 where
  doDisplay = uncurry (displayItem room)
  items     = M.toList $ _roomItems room

-- | Place the given item at the given position in the room
displayItem :: Room -> Pos -> Item -> State Display ()
displayItem room itemPos item = drawPixel (roomPos <> itemPos) (itemPixel item)
  where Rect roomPos _ = room ^. roomRect

-- | Draw borders around the given rectangle
frameRect :: Rect -> BorderStyle -> State Display ()
frameRect (Rect (Pos x y) (Size w h)) borderStyle = do
  drawPixel (Pos (x - 1) (y - 1)) (bsCornerTL borderStyle)
  drawPixel (Pos (x + w) (y - 1)) (bsCornerTR borderStyle)
  drawPixel (Pos (x - 1) (y + h)) (bsCornerBL borderStyle)
  drawPixel (Pos (x + w) (y + h)) (bsCornerBR borderStyle)
  drawLine (Pos x (y - 1)) (x + w - 1) Horizontal (bsHorizontal borderStyle)
  drawLine (Pos x (y + h)) (x + w - 1) Horizontal (bsHorizontal borderStyle)
  drawLine (Pos (x + w) y) (y + h - 1) Vertical   (bsVertical borderStyle)
  drawLine (Pos (x - 1) y) (y + h - 1) Vertical   (bsVertical borderStyle)

-- | draw a pixel
drawPixel :: Pos -> Pixel -> State Display ()
drawPixel (Pos x y) = fillRect $ Rect (Pos x y) (Size 1 1)

-- | draw a line
drawLine :: Pos -> Length -> Orientation -> Pixel -> State Display ()
drawLine (Pos x y) length Horizontal =
  fillRect $ Rect (Pos x y) (Size (length - x + 1) 1)
drawLine (Pos x y) length Vertical =
  fillRect $ Rect (Pos x y) (Size 1 (length - y + 1))

-- | Fill the given rectangle with the pixel character
fillRect :: Rect -> Pixel -> State Display ()
fillRect (Rect (Pos rectX rectY) (Size rectW rectH)) pixel = do
  Size displayW displayH <- gets (^. displaySize)
  modify $ over
    displayPixels
    (// do
      x <- [rectX .. (rectX + rectW - 1)]
      y <- [rectY .. (rectY + rectH - 1)]
      return (Pos (x `mod` displayW) (y `mod` displayH), pixel)
    )

-- | Make a display of given size filled with the given pixel character
mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  pixels    = array cellRange $ (, pixel) <$> range cellRange
  cellRange = (Pos 0 0, Pos (w - 1) (h - 1))
  Size w h  = size
