module Kronar where

import Caminos
import Reglas
import Evaluacion
import Exportar

type Tablero = [Int]

maximo :: [Int] -> Int
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

kronar :: Tablero -> Int
kronar tablero =
  let fin = ajustarFinal tablero
      todos = caminos tablero 0 fin
      validos = filter (\c -> last c == fin) todos
      puntajes = [ t | c <- validos, let (t,_,_) = evaluar tablero c ]
  in maximo puntajes

ejecutar :: Tablero -> IO ()
ejecutar tablero = do
  let fin = ajustarFinal tablero
  let todos = caminos tablero 0 fin
  let validos = filter (\c -> last c == fin) todos
  let resultado = mejorCamino tablero validos
  exportarResultado tablero "resultado.json" resultado