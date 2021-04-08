{-# LANGUAGE TupleSections #-}

module Experimental.BrogalikStateT where

import qualified Data.Map                      as M

import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom
import           Experimental.StateT

transformP2B :: Monad m => StateT Player m () -> StateT Brogalik m ()
transformP2B s = StateT $ \br -> runState br >>= updateState br
 where
  runState br = snd <$> runStateT s (brogalikPlayer br)
  updateState br p = return . ((), ) $ br { brogalikPlayer = p }

renderBrogalik :: Monad m => StateT Brogalik m String
renderBrogalik = do
  brogalik <- getState
  let size@(Size w h) = brogalikSize brogalik
      cellRange       = (Pos 0 0, Pos (w - 1) (h - 1))
      pixels          = array cellRange $ (, displayBg) <$> range cellRange
  renderDisplay (Display size pixels)

renderDisplay :: Monad m => Display -> StateT Brogalik m String
renderDisplay (Display (Size width height) pixels) = StateT
  $ \s -> return (render, s)
 where
  render = unlines
    [ [ pixels ! Pos x y | x <- [0 .. width - 1] ] | y <- [0 .. height - 1] ]

addItem :: Monad m => Pos -> Item -> StateT Room m ()
addItem pos item = StateT $ \room ->
  return . ((), ) $ room { roomItems = M.insert pos item (roomItems room) }

playerMove :: Monad m => Direction -> StateT Player m ()
playerMove direction =
  StateT $ \player -> return . ((), ) $ _movePlayer direction player

brogalikMove :: Monad m => Direction -> StateT Brogalik m ()
brogalikMove direction = StateT $ \brogalik -> return . ((), ) $ brogalik
  { brogalikPlayer = _movePlayer direction $ brogalikPlayer brogalik
  }

fillDisplay :: Monad m => Pixel -> StateT Display m ()
fillDisplay pixel = do
  size <- displaySize <$> getState
  _fillRect (Rect (Pos 0 0) size) pixel

displayBrogalik :: Monad m => Brogalik -> StateT Display m ()
displayBrogalik brogalik = displayPlayer brogalik >> displayRooms brogalik

displayPlayer :: Monad m => Brogalik -> StateT Display m ()
displayPlayer brogalik = displayPixel playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> playerPos player
  Rect playerRoomPos _ = roomRect (brogalikRooms brogalik ! playerRoom player)
  player               = brogalikPlayer brogalik

displayRoom :: Monad m => Room -> StateT Display m ()
displayRoom room =
  _fillRect (roomRect room) roomFloor
    >> let Rect roomPos _ = roomRect room
       in  for_ (M.toList . roomItems $ room) (uncurry (displayItem roomPos))
 where
  displayItem (Pos roomX roomY) (Pos itemX itemY) item =
    displayPixel (Pos (roomX + itemX) (roomY + itemY)) (itemPixel item)

displayRooms :: Monad m => Brogalik -> StateT Display m ()
displayRooms brogalik = for_ (elems (brogalikRooms brogalik)) displayRoom

displayPixel :: Monad m => Pos -> Pixel -> StateT Display m ()
displayPixel (Pos x y) = _fillRect (Rect (Pos x y) (Size 1 1))

_movePlayer :: Direction -> Player -> Player
_movePlayer direction player = player
  { playerPos = newPos (playerPos player) (directionChanges direction)
  }
  where newPos (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)

_fillRect :: Monad m => Rect -> Pixel -> StateT Display m ()
_fillRect rectangle pixel = StateT $ \display ->
  let Rect    (Pos  rectX rectY ) (Size rectW rectH) = rectangle
      Display (Size width height) pixels             = display
  in  return . ((), ) $ display
        { displayPixels = pixels // do
                            x <- [rectX .. (rectX + rectW - 1)]
                            y <- [rectY .. (rectY + rectH - 1)]
                            return (Pos (x `mod` width) (y `mod` height), pixel)
        }
