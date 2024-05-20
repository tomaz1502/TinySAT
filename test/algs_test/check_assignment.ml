open Lib.Parsed_struct;;
open Algs.Certificate;;

let check_assignment (input: parsed_instance) (assignment: assignment): bool =
  let check_lit lit =
    if lit < 0 then
      assignment.(-lit) = false
    else assignment.(lit) = true
  in
  let check_clause = Array.exists check_lit in
  Array.for_all check_clause input.form
