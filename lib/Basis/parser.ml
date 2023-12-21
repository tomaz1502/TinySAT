open Stdlib.Scanf
open Types
open Util

let dimacs_from_string (input : string) : (dimacs, string) result =
    let lines = String.split_on_char '\n' input in
    let lines =
        drop_while (comp not (String.starts_with ~prefix:"p cnf")) lines in
    match lines with
    | [] -> Error "unexpected input"
    | header::clauses ->
        try
            let (n_vars, n_clauses) =
                sscanf header "p cnf %d %d" (fun x y -> x, y) in
            let clauses =
                String.concat " " clauses |>
                String.split_on_char '0' in
            let join curr acc =
                let clause = scan_int_list curr in
                if is_empty_list clause then
                    acc
                else (Array.of_list clause) :: acc in
            let clauses_list = List.fold_right join clauses [] in
            Ok { form       = Array.of_list clauses_list
               ; n_vars     = n_vars
               ; _n_clauses = n_clauses
               } 
        with UnexpectedInput -> Error "unexpected input"
