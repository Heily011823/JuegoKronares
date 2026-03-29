module Kronar where

import Data.Char (toLower)

type Tablero = [Int]


--
-- Kronar el resultado.json
--


--
-- Evalúa un camino aplicando reglas
--

evaluar :: Tablero -> [Int] -> (Int, Int, Int)
evaluar tablero camino =
    let bruto = puntajeBruto tablero camino
        penalizacion = penalizacionZafiro tablero camino
        subtotal = bruto - penalizacion
        bono = bonoEter subtotal (if null camino then 0 else last camino)
        total = subtotal + bono
    in (total, bono, penalizacion)



--
-- Encuentra el mejor camino
--

mejorCamino :: Tablero -> [[Int]] -> ([Int], Int, Int, Int)
mejorCamino _ [] = error "No hay caminos disponibles"
mejorCamino tablero (c:cs) = mejorAux tablero cs (c, evaluar tablero c)


mejorAux :: Tablero -> [[Int]] -> ([Int], (Int, Int, Int)) -> ([Int], Int, Int, Int)
mejorAux _ [] (cam, (total, bono, penalizacion)) = (cam, total, bono, penalizacion)

mejorAux tablero (c:cs) (mejorCam, (mejorTotal, mejorBono, mejorPenal)) =
    let (total, bono, penalizacion) = evaluar tablero c
    in if total > mejorTotal
       then mejorAux tablero cs (c, (total, bono, penalizacion))
       else mejorAux tablero cs (mejorCam, (mejorTotal, mejorBono, mejorPenal))



--
-- Función principal
--

kronar :: Tablero -> Int
kronar tablero =
    let todos = caminos tablero 0
        (_, total, _, _) = mejorCamino tablero todos
    in total



--
-- Exportar resultado en JSON
--

exportarResultado :: Tablero -> FilePath -> IO ()
exportarResultado tablero ruta = do
    let todos = caminos tablero 0
    let (camino, total, bono, penalizacion) = mejorCamino tablero todos
    let reglaVacio = not (null tablero) && last tablero == 0

    writeFile ruta $
        "{\n" ++
        "  \"tablero\": " ++ show tablero ++ ",\n" ++
        "  \"camino_optimo\": " ++ show camino ++ ",\n" ++
        "  \"puntaje_final\": " ++ show total ++ ",\n" ++
        "  \"bono_eter\": " ++ show bono ++ ",\n" ++
        "  \"penalizacion_zafiro\": " ++ show penalizacion ++ ",\n" ++
        "  \"regla_vacio_aplicada\": " ++ map toLower (show reglaVacio) ++ "\n" ++ 
        "}"