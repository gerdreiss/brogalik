{-# LANGUAGE TupleSections #-}

module Control.BrogalikStateT where

import qualified Data.Map                      as M

import           Control.Display
import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom
import           Data.StateT


transformP2B :: Monad m => StateT Player m () -> StateT Brogalik m ()
transformP2B s = StateT $ \br ->
  runStateT s (brogalikPlayer br)
    >>= (\p -> return . (, ()) $ br { brogalikPlayer = p }) . fst

renderBrogalikT :: Monad m => StateT Brogalik m String
renderBrogalikT = do
  brogalik <- getState
  let size      = brogalikSize brogalik
      Size w h  = size
      cellRange = (Pos 0 0, Pos (w - 1) (h - 1))
      pixels    = array cellRange $ (, displayBg) <$> range cellRange
  return
    . show
    . renderDisplay
    . fst
    . runStateT (displayBrogalikT brogalik)
    $ Display size pixels

addItemT :: Monad m => Pos -> Item -> StateT Room m ()
addItemT pos item = StateT $ \room ->
  return . (, ()) $ room { roomItems = M.insert pos item (roomItems room) }

playerMoveT :: Monad m => Direction -> StateT Player m ()
playerMoveT direction =
  StateT $ \player -> return . (, ()) $ _movePlayer direction player

brogalikMoveT :: Monad m => Direction -> StateT Brogalik m ()
brogalikMoveT direction = StateT $ \brogalik -> return . (, ()) $ brogalik
  { brogalikPlayer = _movePlayer direction $ brogalikPlayer brogalik
  }

fillDisplayT :: Monad m => Pixel -> StateT Display m ()
fillDisplayT pixel = do
  size <- displaySize <$> getState
  _fillRectT (rect size) pixel
  where rect = Rect (Pos 0 0)

displayBrogalikT :: Monad m => Brogalik -> StateT Display m ()
displayBrogalikT brogalik = do
  displayPlayerT brogalik
  displayRoomsT brogalik

displayPlayerT :: Monad m => Brogalik -> StateT Display m ()
displayPlayerT brogalik = displayPixelT playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> playerPos player
  Rect playerRoomPos _ = roomRect (brogalikRooms brogalik ! playerRoom player)
  player               = brogalikPlayer brogalik

displayRoomT :: Monad m => Room -> StateT Display m ()
displayRoomT room = StateT $ \display ->
  let Rect (Pos roomX roomY) _ = roomRect room
      foldFunc (Pos itemX itemY, item) =
        _fillRect (mkRect (roomX + itemX) (roomY + itemY) 1 1) (itemChar item)
      display' = _fillRect (roomRect room) roomFloor display
      items    = M.toList (roomItems room)
  in  return (foldl' (flip foldFunc) display' items, ())
  where mkRect x y w h = Rect (Pos x y) (Size w h)

displayRoomsT :: Monad m => Brogalik -> StateT Display m ()
displayRoomsT brogalik = StateT $ \display ->
  let roomList = elems (brogalikRooms brogalik)
  in  return (foldl' (flip displayRoom) display roomList, ())

displayPixelT :: Monad m => Pos -> Pixel -> StateT Display m ()
displayPixelT (Pos x y) pixel = StateT $ \display ->
  return (_fillRect (Rect (Pos x y) (Size 1 1)) pixel display, ())



_movePlayer :: Direction -> Player -> Player
_movePlayer direction player = player
  { playerPos = newPos (playerPos player) (directionChanges direction)
  }
  where newPos (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)

_fillRectT :: Monad m => Rect -> Pixel -> StateT Display m ()
_fillRectT rectangle pixel = StateT $ \display ->
  let Rect    (Pos  rectX rectY ) (Size rectW rectH) = rectangle
      Display (Size width height) pixels             = display
  in  return . (, ()) $ display
        { displayPixels = pixels // do
                            x <- [rectX .. (rectX + rectW - 1)]
                            y <- [rectY .. (rectY + rectH - 1)]
                            return (Pos (x `mod` width) (y `mod` height), pixel)
        }
