module Control.Brogalik where

import qualified Data.Map                      as M

import           Control.Display
import           Control.Geom
import           Data.Array
import           Data.Brogalik
import           Data.Foldable
import           Data.Geom

generateBrogalik :: Width -> Height -> Brogalik
generateBrogalik w h = Brogalik
  { brogalikRooms  = array roomsIndexRange $ zip (range roomsIndexRange) rooms
  , brogalikPlayer = Player { playerRoom    = Index 0
                            , playerPos     = Pos 0 0
                            , playerGold    = 0
                            , playerWeapons = [Sword]
                            }
  , brogalikSize   = Size w h
  }
 where
  roomsIndexRange = (Index 0, Index (roomsCount - 1))
  rooms =
    [ addItem (Pos 4 4) (GoldItem 69) $ mkRoom $ Rect (Pos 0 0) (Size 10 5)
    , addItem (Pos 2 2) (WeaponItem Axe) $ mkRoom $ Rect (Pos 0 20) (Size 5 7)
    , addItem (Pos 2 2) (WeaponItem Sword) $ mkRoom $ Rect (Pos 20 20)
                                                           (Size 10 10)
    ]
  roomsCount = length rooms

mkRoom :: Rect -> Room
mkRoom rect = Room rect mempty

addItem :: Pos -> Item -> Room -> Room
addItem pos item room = room { roomItems = M.insert pos item items }
  where items = roomItems room

brogalikResize :: Size -> Brogalik -> Brogalik
brogalikResize size brogalik = brogalik { brogalikSize = size }

brogalikMove :: Direction -> Brogalik -> Brogalik
brogalikMove direction rogalik = rogalik
  { brogalikPlayer = playerMove direction player
  }
  where player = brogalikPlayer rogalik

renderBrogalik :: Brogalik -> Display -> Display
renderBrogalik brogalik = renderPlayer brogalik . renderRooms brogalik


playerMove :: Direction -> Player -> Player
playerMove direction player = player
  { playerPos = newPos (playerPos player) (directionChanges direction)
  }
 where
  newPos (Pos x y) (PosDelta changeX changeY) = Pos (x + changeX) (y + changeY)

renderPlayer :: Brogalik -> Display -> Display
renderPlayer brogalik = drawPixel playerScreenPos '@'
 where
  playerScreenPos      = playerRoomPos <> playerPos player
  Rect playerRoomPos _ = roomRect (rooms ! playerRoom player)
  player               = brogalikPlayer brogalik
  rooms                = brogalikRooms brogalik


renderRooms :: Brogalik -> Display -> Display
renderRooms rogalik display = foldl' draw display rooms
 where
  draw  = flip drawRoom
  rooms = elems (brogalikRooms rogalik)

