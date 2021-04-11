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
import           Data.Foldable                  ( for_ )
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
  Display (Size w h) pixels <- get
  return
    $ unlines [ [ pixels ! Pos x y | x <- [0 .. w - 1] ] | y <- [0 .. h - 1] ]

-- | Display the Brogalik
displayBrogalik :: Brogalik -> State Display ()
displayBrogalik brogalik = do
  displayRooms brogalik
  displayPassages brogalik
  displayPlayer brogalik

-- | Display the player within the Brogalik
displayPlayer :: Brogalik -> State Display ()
displayPlayer brogalik = drawPixel playerScreenPos '@'
 where
  playerScreenPos =
    playerLocation currentPlace <> brogalik ^. brogalikPlayer . playerPos
  playerLocation (PlaceRoom index) =
    brogalik ^?! brogalikRooms . ix index . roomRect . rectPos
  playerLocation (PlacePassage index) =
    brogalik ^?! brogalikPassages . ix index . passageLine . lineStart
  currentPlace = brogalik ^. brogalikPlayer . playerPlace

-- | Display the rooms within the Brogalik
displayRooms :: Brogalik -> State Display ()
displayRooms brogalik = for_ (brogalik ^. brogalikRooms) displayRoom

-- | Place the given room within the Brogalik
displayRoom :: Room -> State Display ()
displayRoom room = do
  let rect = room ^. roomRect
  frameRect rect unicode
  fillRect rect roomFloor
  displayItems room

-- | Place the items of the given room
displayItems :: Room -> State Display ()
displayItems room = mapM_ doDisplay items
 where
  doDisplay = uncurry (displayItem room)
  items     = M.toList $ _roomItems room

-- | Place the given item at the given position in the room
displayItem :: Room -> Pos -> Item -> State Display ()
displayItem room itemPos item =
  drawPixel (room ^. roomRect . rectPos <> itemPos) (itemPixel item)

-- | Place the given passages within the Brogalik
displayPassages :: Brogalik -> State Display ()
displayPassages brogalik = mapM_ displayPassage (brogalik ^. brogalikPassages)

-- | Place the given passage within the Brogalik
displayPassage :: Passage -> State Display ()
displayPassage passage = drawLine (passage ^. passageLine) '#'

-- | Draw borders around the given rectangle
frameRect :: Rect -> BorderStyle -> State Display ()
frameRect rect borderStyle = do
  let Rect (Pos x y) (Size w h) = rect
  -- draw corners
  drawPixel (Pos (x - 1) (y - 1)) (bsCornerTL borderStyle)
  drawPixel (Pos (x + w) (y - 1)) (bsCornerTR borderStyle)
  drawPixel (Pos (x - 1) (y + h)) (bsCornerBL borderStyle)
  drawPixel (Pos (x + w) (y + h)) (bsCornerBR borderStyle)
  -- draw walls
  drawLine (Line (Pos x (y - 1)) w Horizontal) (bsHorizontal borderStyle)
  drawLine (Line (Pos x (y + h)) w Horizontal) (bsHorizontal borderStyle)
  drawLine (Line (Pos (x + w) y) h Vertical)   (bsVertical borderStyle)
  drawLine (Line (Pos (x - 1) y) h Vertical)   (bsVertical borderStyle)

-- | draw a pixel
drawPixel :: Pos -> Pixel -> State Display ()
drawPixel pos = fillRect $ Rect pos (Size 1 1)

-- | draw a line
drawLine :: Line -> Pixel -> State Display ()
drawLine (Line pos length Horizontal) = fillRect $ Rect pos (Size length 1)
drawLine (Line pos length Vertical  ) = fillRect $ Rect pos (Size 1 length)

-- | Fill the given rectangle with the pixel character
fillRect :: Rect -> Pixel -> State Display ()
fillRect rect pixel = do
  Size displayW displayH <- gets (^. displaySize)
  modify $ over
    displayPixels
    (// do
      x <- [rectX .. (rectX + rectW - 1)]
      y <- [rectY .. (rectY + rectH - 1)]
      return (Pos (x `mod` displayW) (y `mod` displayH), pixel)
    )
  where Rect (Pos rectX rectY) (Size rectW rectH) = rect

-- | Make a display of given size filled with the given pixel character
mkDisplay :: Size -> Pixel -> Display
mkDisplay size pixel = Display size pixels
 where
  pixels    = array cellRange $ (, pixel) <$> range cellRange
  cellRange = (Pos 0 0, Pos (size ^. width - 1) (size ^. height - 1))
