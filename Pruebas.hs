module Pruebas where

import Kronar

-- Tableros de prueba

t1 :: Tablero
t1 = [3, -2, -1, 4, 2]

t2 :: Tablero
t2 = [1, -3, -4, 2, 0]

t3 :: Tablero
t3 = [5, 5, 5, 5, 5, 5]


-- Función principal de pruebas

probar :: IO ()
probar = do
    putStrLn "Prueba #1"
    print (kronar t1)
    exportarResultado t1 "resultado.json"

    -- putStrLn "Prueba #2"
    -- print (kronar t2)
    -- exportarResultado t2 "resultado1.json"

    -- putStrLn "Prueba #3"
    -- print (kronar t3)
    -- exportarResultado t3 "resultado2.json"