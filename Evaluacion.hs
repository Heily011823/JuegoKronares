module Evaluacion where

import Reglas

-- Retorna (Total, Bono, Penalización)
evaluar :: Tablero -> [Int] -> (Int, Int, Int)
evaluar t camino =
    let bruto = puntajeBruto t camino
        penal = penalizacionZafiro t camino
        -- Calculamos el bono sobre el bruto (o subtotal según tu regla)
        bono  = bonoEter bruto (last camino)
        total = bruto - penal + bono
    in (total, bono, penal)

-- RESTRICCIÓN: Implementación propia de máximo para tuplas planas
mejorCamino :: Tablero -> [[Int]] -> ([Int], Int, Int, Int)
mejorCamino _ [] = error "Sin caminos"
mejorCamino t (c:cs) =
    let (cI, tI, bI, pI) = inicial -- Aplanamos el primer caso
    in foldl comparar (cI, tI, bI, pI) cs
  where
    -- Valor inicial aplanado
    (tot, bon, pen) = evaluar t c
    inicial = (c, tot, bon, pen)

    -- La función de comparar ahora recibe y devuelve una tupla de 4 elementos
    comparar (bestC, bestT, bestB, bestP) nuevoC =
        let (nT, nB, nP) = evaluar t nuevoC
        in if nT >= bestT
           then (nuevoC, nT, nB, nP)
           else (bestC, bestT, bestB, bestP)