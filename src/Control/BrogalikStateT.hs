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
transformP2B s = StateT $ \br -> runState br >>= updateState br
 where
  runState br = snd <$> runStateT s (brogalikPlayer br)
  updateState br p = return . ((), ) $ br { brogalikPlayer = p }


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
  return . ((), ) $ room { roomItems = M.insert pos item (roomItems room) }


playerMoveT :: Monad m => Direction -> StateT Player m ()
playerMoveT direction =
  StateT $ \player -> return . ((), ) $ _movePlayer direction player


brogalikMoveT :: Monad m => Direction -> StateT Brogalik m ()
brogalikMoveT direction = StateT $ \brogalik -> return . ((), ) $ brogalik
  { brogalikPlayer = _movePlayer direction $ brogalikPlayer brogalik
  }


fillDisplayT :: Monad m => Pixel -> StateT Display m ()
fillDisplayT pixel = do
  size <- displaySize <$> getState
  _fillRectT (Rect (Pos 0 0) size) pixel


displayBrogalikT :: Monad m => Brogalik -> StateT Display m ()
displayBrogalikT brogalik = displayPlayerT brogalik >> displayRoomsT brogalik


displayPlayerT :: Monad m => Brogalik -> StateT Display m ()
displayPlayerT brogalik = displayPixelT playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> playerPos player
  Rect playerRoomPos _ = roomRect (brogalikRooms brogalik ! playerRoom player)
  player               = brogalikPlayer brogalik


displayRoomT :: Monad m => Room -> StateT Display m ()
displayRoomT room =
  _fillRectT (roomRect room) roomFloor
    >> let Rect roomPos _ = roomRect room
       in  for_ (M.toList . roomItems $ room) (uncurry (displayItem roomPos))
 where
  displayItem (Pos roomX roomY) (Pos itemX itemY) item =
    displayPixelT (Pos (roomX + itemX) (roomY + itemY)) (itemChar item)


displayRoomsT :: Monad m => Brogalik -> StateT Display m ()
displayRoomsT brogalik = for_ (elems (brogalikRooms brogalik)) displayRoomT


displayPixelT :: Monad m => Pos -> Pixel -> StateT Display m ()
displayPixelT (Pos x y) = _fillRectT (Rect (Pos x y) (Size 1 1))



_movePlayer :: Direction -> Player -> Player
_movePlayer direction player = player
  { playerPos = newPos (playerPos player) (directionChanges direction)
  }
  where newPos (Pos x y) (PosDelta dx dy) = Pos (x + dx) (y + dy)

_fillRectT :: Monad m => Rect -> Pixel -> StateT Display m ()
_fillRectT rectangle pixel = StateT $ \display ->
  let Rect    (Pos  rectX rectY ) (Size rectW rectH) = rectangle
      Display (Size width height) pixels             = display
  in  return . ((), ) $ display
        { displayPixels = pixels // do
                            x <- [rectX .. (rectX + rectW - 1)]
                            y <- [rectY .. (rectY + rectH - 1)]
                            return (Pos (x `mod` width) (y `mod` height), pixel)
        }
