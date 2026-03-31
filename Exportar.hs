module Exportar where

import System.IO

exportarResultado :: [Int] -> FilePath -> ([Int], Int, Int, Int) -> IO ()
exportarResultado tablero ruta (camino, total, bono, penal) = do
    let vacio = last tablero == 0
    let json = "{\n" ++
               "  \"tablero\": " ++ show tablero ++ ",\n" ++
               "  \"camino_optimo\": " ++ show camino ++ ",\n" ++
               "  \"puntaje_final\": " ++ show total ++ ",\n" ++
               "  \"bono_eter\": " ++ show bono ++ ",\n" ++
               "  \"penalizacion_zafiro\": " ++ show penal ++ ",\n" ++
               "  \"regla_vacio_aplicada\": " ++ (if vacio then "true" else "false") ++ "\n" ++
               "}"
    writeFile ruta json
    putStrLn $ "Archivo " ++ ruta ++ " generado."