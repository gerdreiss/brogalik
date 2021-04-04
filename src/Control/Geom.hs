module Control.Geom where

import           Data.Geom


directionChanges :: Direction -> PosDelta
directionChanges West  = PosDelta (-1) 0
directionChanges East  = PosDelta 1 0
directionChanges North = PosDelta 0 (-1)
directionChanges South = PosDelta 0 1


posDelta :: Pos -> Pos -> PosDelta
posDelta (Pos x1 y1) (Pos x2 y2) = PosDelta (x1 - x2) (y1 - y2)


move :: Pos -> PosDelta -> Pos
move (Pos px py) (PosDelta dx dy) = Pos (px + dx) (py + dy)


(^+^) :: Size -> Size -> Size
(^+^) s1 s2 = s1 <> s2

(^-^) :: Size -> Size -> Size
(^-^) (Size w1 h1) (Size w2 h2) = Size (w1 - w2) (h1 - h2)


reduceSizeByPercent :: Int -> Size -> Size
reduceSizeByPercent p size = size ^-^ (p `percentOfSize` size)

percentOfSize :: Int -> Size -> Size
percentOfSize p (Size w h) = Size (p %% w) (p %% h)

(%%) :: Int -> Int -> Int
(%%) percent value = round $ v / 100 * p
 where
  v = fromIntegral value :: Float
  p = fromIntegral percent :: Float
