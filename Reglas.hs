module Reglas (
  puntajeBruto,
  penalizacionZafiro,
  bonoEter,
  ajustarFinal
) where

type Tablero = [Int]

puntajeBruto :: Tablero -> [Int] -> Int
puntajeBruto tablero camino = sum [tablero !! i | i <- camino]

penalizacionZafiro :: Tablero -> [Int] -> Int
penalizacionZafiro _ [] = 0
penalizacionZafiro _ [_] = 0
penalizacionZafiro tablero (x:y:xs)
  | tablero !! x < 0 && tablero !! y < 0 = 5 + penalizacionZafiro tablero (y:xs)
  | otherwise = penalizacionZafiro tablero (y:xs)

bonoEter :: Int -> Int -> Int
bonoEter total _ =
  if even total then 10 else 0

ajustarFinal :: Tablero -> Int
ajustarFinal tablero
  | last tablero == 0 = length tablero - 2
  | otherwise = length tablero - 1