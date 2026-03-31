module Caminos (caminos) where

type Tablero = [Int]

caminos :: Tablero -> Int -> Int -> [[Int]]
caminos tablero pos fin
  | pos == fin = [[pos]]
  | pos > fin  = []
  | otherwise =
      concat [ map (pos:) (caminos tablero (pos + salto) fin)
             | salto <- [1,2,3], pos + salto <= fin ]