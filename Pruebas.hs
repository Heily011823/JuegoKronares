module Pruebas where

import Kronar

-- ========================
-- PRUEBA 1
-- ========================

t1 :: Tablero
t1 = [3, -2, -1, 4, 2]

-- Explicación:
-- Se evalúan varios caminos posibles.
-- Se evita pasar por dos negativos consecutivos (-2, -1).
-- Camino óptimo: [0,2,3,4]
-- 3 + (-1) + 4 + 2 = 8
-- No hay penalización
-- 8 es par → bono +10
-- Resultado esperado: 18


-- ========================
-- PRUEBA 2 (REGLA VACÍO)
-- ========================

t2 :: Tablero
t2 = [1, -3, -4, 2, 0]

-- Explicación:
-- La última casilla es 0 → se aplica regla del vacío
-- Se termina en índice 3
-- Se evita el par (-3, -4)
-- Camino óptimo: [0,1,3]
-- 1 + (-3) + 2 = 0
-- No hay penalización
-- 0 es par → bono +10
-- Resultado esperado: 10


-- ========================
-- PRUEBA 3 (POSITIVOS)
-- ========================

t3 :: Tablero
t3 = [5, 5, 5, 5, 5, 5]

-- Explicación:
-- Todos son positivos → conviene recorrer todo
-- Camino óptimo: [0,1,2,3,4,5]
-- Suma: 30
-- Sin penalización
-- 30 es par → bono +10
-- Resultado esperado: 40


-- ========================
-- PRUEBA 4 (INVENTADA)
-- ========================

t4 :: Tablero
t4 = [2, -5, 10, -1]

-- Explicación:
-- Se evita pasar por -5 y -1 juntos
-- Mejor camino: [0,2,3]
-- 2 + 10 + (-1) = 11
-- No hay penalización
-- 11 impar → sin bono
-- Resultado esperado: 11


-- ========================
-- MAIN
-- ========================

main :: IO ()
main = do
    putStrLn "Prueba #1"
    print (kronar t1)
    exportarResultado t1 "resultado.json"

    putStrLn "Prueba #2"
    print (kronar t2)

    putStrLn "Prueba #3"
    print (kronar t3)

    putStrLn "Prueba #4"
    print (kronar t4)