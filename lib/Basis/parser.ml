open Stdlib.Scanf
open Types
open Util

let int_of_string' s =
  try
    int_of_string s
  with
    Failure(_) as e ->
      Printf.printf "failed for: %s\n" s;
      raise e


let dimacs_from_string (input : string) : (dimacs, string) result =
  let lines = String.split_on_char '\n' input in
  let lines =
    drop_while (comp not (String.starts_with ~prefix:"p cnf")) lines in
  match lines with
  | [] -> Error "unexpected input"
  | header::clauses ->
    try
      let (n_vars, n_clauses) = sscanf header "p cnf %d %d" (fun x y -> x, y) in
      Printf.printf "input length : %d\n" (List.length clauses);
      let clauses =
        String.concat " " clauses      |>
        String.split_on_char ' '       |>
        List.filter (fun s -> s <> "") |>
        split_list_on_elem "0"         |>
        List.map (List.map int_of_string')
      in
      Printf.printf "clauses length: %d\n" (List.length clauses);
      Ok
        { form       = Array.of_list (List.map Array.of_list clauses)
        ; n_vars     = n_vars
        ; _n_clauses = n_clauses
        } 
    with UnexpectedInput -> Error "unexpected input"
