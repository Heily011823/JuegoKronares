(* 1. TAREA ADICIONAL: Tipo algebraico para estados *)
type estado = Visitada | Omitida

(* 2. LECTOR DE ARCHIVO: Robusto para Windows *)
let leer_archivo ruta =
  let ic = open_in_bin ruta in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* 3. PARSER MANUAL: Extrae listas del JSON sin usar librerías externas *)
let extraer_lista clave texto =
  try
    let rec buscar_pos s sub i =
      if i + String.length sub > String.length s then -1
      else if String.sub s i (String.length sub) = sub then i
      else buscar_pos s sub (i + 1)
    in
    let pos_clave = buscar_pos texto ("\"" ^ clave ^ "\"") 0 in
    if pos_clave = -1 then []
    else
      let inicio = String.index_from texto pos_clave '[' in
      let fin = String.index_from texto inicio ']' in
      let sub = String.sub texto (inicio + 1) (fin - inicio - 1) in
      sub |> String.split_on_char ','
          |> List.filter (fun x -> String.trim x <> "")
          |> List.map (fun x -> int_of_string (String.trim x))
  with _ -> []

(* 4. LÓGICA DE VISUALIZACIÓN: Dibuja el tablero lineal *)
let dibujar_tablero tablero camino =
  print_endline "\n========================================";
  print_endline "       RECONSTRUCCIÓN VISUAL KRONAR";
  print_endline "========================================";

  (* Fila de marcas [X] o [ ] *)
  List.iteri (fun i _ ->
    let est = if List.mem i camino then Visitada else Omitida in
    print_string (match est with Visitada -> "[X] " | Omitida -> "[ ] ")
  ) tablero;

  print_newline ();

  (* Fila de valores de energía *)
  List.iter (Printf.printf "%3d ") tablero;
  print_endline "\n----------------------------------------"

(* 5. TAREA ADICIONAL: Análisis con Pattern Matching *)
let analizar tablero camino =
  let estados = List.mapi (fun i _ -> if List.mem i camino then Visitada else Omitida) tablero in
  let (v, o) = List.fold_left (fun (vis, omi) e ->
    match e with Visitada -> (vis + 1, omi) | Omitida -> (vis, omi + 1)
  ) (0, 0) estados in

  let hay_neg = List.exists2 (fun e valor -> e = Visitada && valor < 0) estados tablero in

  Printf.printf "➤ Estadísticas del Viaje:\n";
  Printf.printf "   - Casillas Visitadas: %d\n" v;
  Printf.printf "   - Casillas Omitidas:  %d\n" o;
  Printf.printf "   - ¿Pisó energía negativa?: %s\n" (if hay_neg then "SÍ" else "NO")

(* 6. MAIN: Orquestación del programa *)
let () =
  (* Localizamos el archivo en la carpeta actual de trabajo *)
  let nombre_archivo = "resultado.json" in
  let ruta_completa = Filename.concat (Sys.getcwd()) nombre_archivo in

  try
    if not (Sys.file_exists ruta_completa) then
      failwith ("No se encuentra el archivo: " ^ ruta_completa)
    else
      let json = leer_archivo ruta_completa in
      let tablero = extraer_lista "tablero" json in
      let camino = extraer_lista "camino_optimo" json in

      if tablero = [] then failwith "El tablero está vacío o el JSON es inválido";

      dibujar_tablero tablero camino;
      analizar tablero camino;
      print_endline "========================================\n"
  with e ->
    print_endline "\n--- ERROR EN LA INTEGRACIÓN ---";
    print_endline (Printexc.to_string e);
    print_endline "Asegúrese de que Haskell generó 'resultado.json' en esta carpeta."