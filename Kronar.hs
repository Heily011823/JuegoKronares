module Kronar where

-- Representa el tablero como lista de enteros
type Tablero = [Int]

-- 1. Calcula todos los caminos posibles (listas de índices)
-- Usa recursión obligatoria y saltos de 1, 2 o 3 casillas 
caminos :: Tablero -> Int -> [[Int]]
caminos tablero posActual
    | posActual == ajustarFinal tablero = [[posActual]] -- Caso base: llegó al final 
    | posActual > ajustarFinal tablero  = []            -- Se pasó del tablero
    | otherwise = 
        let salto1 = caminos tablero (posActual + 1)
            salto2 = caminos tablero (posActual + 2)
            salto3 = caminos tablero (posActual + 3)
            proximosPasos = salto1 ++ salto2 ++ salto3 -- 
        in map (posActual:) proximosPasos

-- 2. Calcula el puntaje bruto (suma simple de valores)
puntajeBruto :: Tablero -> [Int] -> Int
puntajeBruto tablero camino = sum [tablero !! i | i <- camino]

-- 3. Regla del Zafiro: -5 puntos por dos negativos consecutivos
penalizacionZafiro :: Tablero -> [Int] -> Int
penalizacionZafiro _ [] = 0
penalizacionZafiro _ [_] = 0
penalizacionZafiro tablero (x:y:xs)
    | (tablero !! x) < 0 && (tablero !! y) < 0 = 5 + penalizacionZafiro tablero (y:xs)
    | otherwise = penalizacionZafiro tablero (y:xs)

-- 4. Regla del Éter: +10 si el total es par al llegar al final
bonoEter :: Int -> Int -> Int
bonoEter puntajeFinal _ 
    | even puntajeFinal = 10
    | otherwise = 0

-- 5. Regla del Vacío: Si la última casilla es 0, termina en n-1
ajustarFinal :: Tablero -> Int
ajustarFinal tablero
    | last tablero == 0 = length tablero - 2 -- Índice n-1 (base 0) 
    | otherwise = length tablero - 1         -- Índice n (base 0)

-- 6. Función propia para encontrar el máximo 
maximo :: [Int] -> Int
maximo [] = error "Lista vacía"
maximo [x] = x
maximo (x:xs)
    | x > maxResto = x
    | otherwise = maxResto
    where maxResto = maximo xs