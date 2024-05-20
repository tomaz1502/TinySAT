open Lib.Parsed_struct;;
open Algs.Certificate;;

(* Trusted in the tests *)
let check_assignment (input: parsed_instance) (assignment: assignment): bool =
  let check_lit lit =
    if lit < 0 then
      assignment.(-lit) = false
    else assignment.(lit) = true
  in
  let check_clause = Array.exists check_lit in
  Array.for_all check_clause input.form

(* Trusted in the tests *)
let check_unsat_proof (input: parsed_instance) (cert: proof_cert): bool =
  let fold_fun acc step =
    match step with
      | InputClause _ -> acc
      | Resolution { resolvant; c1_idx; c2_idx; _ } ->
          let resolved = resolve resolvant acc.(c1_idx - 1) acc.(c2_idx - 1) in
          Array.append acc [|resolved|]
  in
  let input_form_list = Array.map (Array.to_list) input.form in
  let final_clauses = List.fold_left fold_fun input_form_list cert.proof in
  final_clauses.(Array.length final_clauses - 1) = []
