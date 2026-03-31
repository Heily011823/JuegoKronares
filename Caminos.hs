module Caminos (caminos) where

type Tablero = [Int]

-- Ajustamos a la firma requerida: Tablero -> Posición Actual -> Caminos
caminos :: Tablero -> Int -> [[Int]]
caminos tablero posActual
  | posActual == fin = [[fin]]
  | posActual > fin  = []
  | otherwise =
      concat [ map (posActual:) (caminos tablero (posActual + salto))
             | salto <- [1, 2, 3] ]
  where
    -- Calculamos el objetivo final una sola vez
    -- Usamos la lógica de la "Regla del Vacío" aquí o en una función aparte
    fin = if last tablero == 0 then length tablero - 2 else length tablero - 1