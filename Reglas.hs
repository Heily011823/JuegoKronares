module Reglas where

type Tablero = [Int]

-- Calcula el puntaje sin bonos ni penalizaciones
puntajeBruto :: Tablero -> [Int] -> Int
puntajeBruto tablero indices = sum [tablero !! i | i <- indices]

-- Regla del Zafiro: -5 si hay dos negativos consecutivos EN EL CAMINO
penalizacionZafiro :: Tablero -> [Int] -> Int
penalizacionZafiro t (i1:i2:is)
  | (t !! i1) < 0 && (t !! i2) < 0 = 5 + penalizacionZafiro t (i2:is)
  | otherwise = penalizacionZafiro t (i2:is)
penalizacionZafiro _ _ = 0

-- Regla del Éter: +10 si el puntaje es par
bonoEter :: Int -> Int -> Int
bonoEter puntaje _ = if even puntaje then 10 else 0

-- Regla del Vacío: Si la última es 0, termina en n-1
ajustarFinal :: Tablero -> Int
ajustarFinal t = if last t == 0 then length t - 2 else length t - 1