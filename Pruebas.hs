module Pruebas where

import Kronar
import Reglas (Tablero)

-- ========================
-- PRUEBA 1
-- ========================
t1 :: Tablero
t1 = [3, -2, -1, 4, 2]
-- Explicación:
-- Camino óptimo: [0, 2, 3, 4] (Valores: 3, -1, 4, 2)
-- Bruto: 8. Bono Éter: +10 (por ser par). Zafiro: 0.
-- Resultado: 18

-- ========================
-- PRUEBA 2 (REGLA VACÍO)
-- ========================
t2 :: Tablero
t2 = [1, -3, -4, 2, 0]
-- Explicación:
-- Casilla final es 0, termina en índice 3 (valor 2).
-- Camino óptimo: [0, 1, 3] (Valores: 1, -3, 2)
-- Bruto: 0. Bono Éter: +10 (0 es par). Zafiro: 0.
-- Resultado: 10

-- ========================
-- PRUEBA 3 (POSITIVOS)
-- ========================
t3 :: Tablero
t3 = [5, 5, 5, 5, 5, 5]
-- Explicación: Todo positivo, se recorre completo.
-- Bruto: 30. Bono Éter: +10.
-- Resultado: 40

-- ========================
-- PRUEBA 4 (INVENTADA)
-- ========================
t4 :: Tablero
t4 = [2, -5, 10, -1]
-- Explicación:
-- Camino óptimo: [0, 2, 3] (Valores: 2, 10, -1)
-- Bruto: 11. Bono Éter: 0 (impar). Zafiro: 0.
-- Resultado: 11

-- ========================
-- MAIN
-- ========================
main :: IO ()
main = do
    putStrLn "=== EJECUTANDO PRUEBAS DEL KRONAR ==="

    putStrLn "\nPrueba #1 (Exportando a JSON):"
    print (kronar t1)
    -- Usamos 'ejecutar' que definimos en Kronar para generar el JSON
    ejecutar t1

    putStrLn "\nPrueba #2 (Regla del Vacío):"
    print (kronar t2)

    putStrLn "\nPrueba #3 (Todo Positivo):"
    print (kronar t3)

    putStrLn "\nPrueba #4 (Inventada):"
    print (kronar t4)

    putStrLn "\nProceso completado. Revise 'resultado.json'."