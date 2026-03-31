module Exportar (exportarResultado) where

import Data.Char (toLower)

exportarResultado :: [Int] -> FilePath -> ([Int], Int, Int, Int) -> IO ()
exportarResultado tablero ruta (camino, total, bono, penalizacion) =
  let reglaVacio = last tablero == 0
  in writeFile ruta $
    "{\n" ++
    "  \"tablero\": " ++ show tablero ++ ",\n" ++
    "  \"camino_optimo\": " ++ show camino ++ ",\n" ++
    "  \"puntaje_final\": " ++ show total ++ ",\n" ++
    "  \"bono_eter\": " ++ show bono ++ ",\n" ++
    "  \"penalizacion_zafiro\": " ++ show penalizacion ++ ",\n" ++
    "  \"regla_vacio_aplicada\": " ++ map toLower (show reglaVacio) ++ "\n" ++
    "}"