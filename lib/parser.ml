open Stdlib.Scanf

open Parsed_struct
open Util

let int_of_string' s =
  try
    int_of_string s
  with
    Failure(_) as e ->
      Printf.printf "int_of_string failed for: %s\n" s;
      raise e

let dimacs_from_string (input : string) : (parsed_instance_data, string) result =
  let lines = String.split_on_char '\n' input in
  let lines = List.filter (comp not (String.starts_with ~prefix:"c")) lines in
  match lines with
  | [] -> Error "unexpected input"
  | header::clauses ->
    try
      let n_vars, n_clauses = sscanf header "p cnf %d %d" (fun x y -> (x, y)) in
      let clauses = take n_clauses clauses in
      let clauses =
        String.concat " " clauses      |>
        String.split_on_char ' '       |>
        List.filter (fun s -> s <> "") |>
        split_list_on_elem "0"         |>
        List.map (List.map int_of_string')
      in
      Ok
        { form   = Array.of_list (List.map Array.of_list clauses)
        ; n_vars = n_vars
        } 
    with UnexpectedInput -> Error "unexpected input"
