module Pruebas where

import Kronar

-- Tableros de prueba

-- t1

-- Explicación: No hay valores negativos seguidos en el camino, por lo tanto, no se aplica la penalización por zafiro, el resultado es un número par, entonces de da el bono de éter +10 y no se aplica la regla del vacío porque la última casilla no es 0.

-- Camino óptimo: [0,2,3,4] 
-- 3 + (-1) + 4 + 2 = 8
-- Par -> bono +10
-- Total = 8 + 10 = 18

t1 :: Tablero
t1 = [3, -2, -1, 4, 2]


-- t2

-- Explicación: No hay valores negativos seguidos (se evita el -4) en el camino, por lo tanto, no se aplica la penalización por zafiro, el resultado es un número par, entonces de da el bono de éter +10 y se aplica la regla del vacío porque la última casilla es 0, entonces el camino termina en el índice 3.

-- Camino óptimo: [0,1,3]
-- 1 + (-3) + 2 = 0
-- Par -> bono +10
-- Total = 0 + 10 = 10

t2 :: Tablero
t2 = [1, -3, -4, 2, 0]



-- t3

-- Explicación: Todos los valores son positivos, entonces el mejor camino es recorrer todas las casillas en orden y posición por posición, no hay valores negativos seguidos en el camino, por lo tanto, no se aplica la penalización por zafiro y no se aplica la regla del vacío porque la última casilla no es 0.

-- Camino óptimo: [0,1,2,3,4,5]
-- 5 + 5 + 5 + 5 + 5 + 5 = 30
-- Par -> bono +10
-- Total = 30 + 10 = 40

t3 :: Tablero
t3 = [5, 5, 5, 5, 5, 5]


-- Función principal de pruebas

main :: IO ()
main = do
    putStrLn "Prueba #1"
    print (kronar t1)
    exportarResultado t1 "resultado.json"

    putStrLn "Prueba #2"
    print (kronar t2)
    -- exportarResultado t2 "resultado1.json"

    putStrLn "Prueba #3"
    print (kronar t3)
    -- exportarResultado t3 "resultado2.json"