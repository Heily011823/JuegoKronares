module Kronar where

import Caminos
import Reglas
import Evaluacion
import Exportar

-- RESTRICCIÓN: Implementación propia de máximo
maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = let m = maximo xs in if x > m then x else m

kronar :: Tablero -> Int
kronar tablero =
  let todos = caminos tablero 0   -- <--- QUITAMOS 'fin'
      -- evaluamos cada camino y extraemos solo el puntaje final
      puntajes = [ t | c <- todos, let (t,_,_) = evaluar tablero c ]
  in maximo puntajes

ejecutar :: Tablero -> IO ()
ejecutar tablero = do
  let todos = caminos tablero 0   
  -- Obtenemos el mejor camino y sus datos
  let resultado = mejorCamino tablero todos
  -- Exportamos el resultado a JSON
  exportarResultado tablero "resultado.json" resultado