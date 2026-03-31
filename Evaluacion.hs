module Evaluacion (evaluar, mejorCamino) where

import Reglas

evaluar :: Tablero -> [Int] -> (Int, Int, Int)
evaluar tablero camino =
  let bruto = puntajeBruto tablero camino
      penal = penalizacionZafiro tablero camino
      subtotal = bruto - penal
      bono = bonoEter subtotal (last camino)
      total = subtotal + bono
  in (total, bono, penal)

mejorCamino :: Tablero -> [[Int]] -> ([Int], Int, Int, Int)
mejorCamino _ [] = error "No hay caminos"
mejorCamino tablero (c:cs) = foldl comparar (c, evaluar tablero c) cs
  where
    comparar (mc, (mt, mb, mp)) c =
      let (t, b, p) = evaluar tablero c
      in if t > mt then (c, (t, b, p)) else (mc, (mt, mb, mp))