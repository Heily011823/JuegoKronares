(* Tipo algebraico requerido *)
type estado = Visitada | Omitida

(* Leer archivo JSON como string *)
let leer_archivo ruta =
  let ic = open_in ruta in
  let buffer = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string buffer (input_line ic);
       Buffer.add_char buffer '\n'
     done
   with End_of_file -> ());
  close_in ic;
  Buffer.contents buffer

(* Extraer lista de enteros desde JSON (simple parsing) *)
let extraer_lista clave texto =
  try
    let inicio = String.index_from texto 0 '[' in
    let fin = String.index_from texto inicio ']' in
    let sub = String.sub texto (inicio + 1) (fin - inicio - 1) in
    sub
    |> String.split_on_char ','
    |> List.map (fun x -> int_of_string (String.trim x))
  with _ -> []

(* Verifica si un índice está en el camino *)
let esta_en_camino i camino =
  List.exists (fun x -> x = i) camino

(* Construir estados del tablero *)
let construir_estados tablero camino =
  List.mapi (fun i _ ->
    if esta_en_camino i camino then Visitada else Omitida
  ) tablero

(* Mostrar tablero visual *)
let mostrar_tablero estados tablero =
  List.iter2 (fun est valor ->
    match est with
    | Visitada -> print_string "[X] "
    | Omitida  -> print_string "[ ] "
  ) estados tablero;
  print_newline ();

  List.iter (fun v ->
    print_int v;
    print_string " "
  ) tablero;
  print_newline ()

(* Contar visitadas y omitidas *)
let contar estados =
  List.fold_left (fun (v, o) est ->
    match est with
    | Visitada -> (v + 1, o)
    | Omitida  -> (v, o + 1)
  ) (0, 0) estados

(* Detectar si hay negativos visitados *)
let hay_negativos_visitados estados tablero =
  List.exists2 (fun est valor ->
    match est with
    | Visitada -> valor < 0
    | Omitida  -> false
  ) estados tablero

(* MAIN *)
let () =
  let json = leer_archivo "resultado.json" in

  (* OJO: aquí asumimos orden del JSON *)
  let tablero = extraer_lista "tablero" json in
  let camino = extraer_lista "camino_optimo" json in

  let estados = construir_estados tablero camino in

  print_endline "TABLERO:";
  mostrar_tablero estados tablero;

  let (visitadas, omitidas) = contar estados in
  Printf.printf "Visitadas: %d\n" visitadas;
  Printf.printf "Omitidas: %d\n" omitidas;

  let hayNeg = hay_negativos_visitados estados tablero in
  Printf.printf "Hay negativos visitados: %b\n" hayNeg;