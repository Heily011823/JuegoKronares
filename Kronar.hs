module Kronar where

import Data.Char (toLower)

type Tablero = [Int]

--
-- GENERACIÓN DE CAMINOS
--

caminos :: Tablero -> Int -> [[Int]]
caminos tablero posActual
    | posActual == ajustarFinal tablero = [[posActual]]
    | posActual > ajustarFinal tablero  = []
    | otherwise =
        let salto1 = caminos tablero (posActual + 1)
            salto2 = caminos tablero (posActual + 2)
            salto3 = caminos tablero (posActual + 3)
        in map (posActual:) (salto1 ++ salto2 ++ salto3)

--
-- PUNTAJE BASE
--

puntajeBruto :: Tablero -> [Int] -> Int
puntajeBruto tablero camino = sum [tablero !! i | i <- camino]

--
-- REGLA ZAFIRO
--

penalizacionZafiro :: Tablero -> [Int] -> Int
penalizacionZafiro _ [] = 0
penalizacionZafiro _ [_] = 0
penalizacionZafiro tablero (x:y:xs)
    | (tablero !! x) < 0 && (tablero !! y) < 0 = 5 + penalizacionZafiro tablero (y:xs)
    | otherwise = penalizacionZafiro tablero (y:xs)

--
-- REGLA ÉTER
--

bonoEter :: Int -> Int -> Int
bonoEter puntajeFinal _
    | even puntajeFinal = 10
    | otherwise = 0

--
-- REGLA VACÍO
--

ajustarFinal :: Tablero -> Int
ajustarFinal tablero
    | last tablero == 0 = length tablero - 2
    | otherwise = length tablero - 1

--
-- EVALUAR CAMINO
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
-- MEJOR CAMINO
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
-- MAXIMO
--

maximo :: [Int] -> Int
maximo [] = error "Lista vacía"
maximo [x] = x
maximo (x:xs)
    | x > m     = x
    | otherwise = m
    where m = maximo xs

--
-- FUNCIÓN PRINCIPAL
--

kronar :: Tablero -> Int
kronar tablero =
    let todos = caminos tablero 0
        puntajes = [ total | c <- todos, let (total, _, _) = evaluar tablero c ]
    in maximo puntajes

--
-- EXPORTAR JSON
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