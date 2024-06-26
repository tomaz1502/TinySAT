open Lib.Parsed_struct
open Certificate

let pp_sat = function
  | Ok _    -> "SAT"
  | Error _ -> "UNSAT"

let print_sat fmt res = 
  Format.fprintf fmt "%s\n" (if Result.is_ok res then "SAT" else "UNSAT")

let pp_clause clause =
  Array.to_list clause   |>
  List.map string_of_int |>
  String.concat " v "

let pp_form form =
  Array.to_list form                            |>
  List.map (fun c -> "( " ^ pp_clause c ^ " )") |>
  String.concat " ^ "

let pp_input { form; n_vars } =
  String.concat "\n"
  [ "n_vars = " ^ (string_of_int n_vars)
  ; "form = " ^ (pp_form form)
  ]

let print_clause fmt c =
  if Array.length c > 0 then begin
    Format.fprintf fmt "%d" c.(0);
    for i = 1 to Array.length c - 1 do
      Format.fprintf fmt " v %d" c.(i)
    done
  end
  else
    Format.fprintf fmt "<empty>"

let print_form fmt form =
  if Array.length form > 0 then
    Format.fprintf fmt "(%a)" print_clause form.(0);
  for i = 1 to Array.length form - 1 do
    Format.fprintf fmt " ^ (%a)" print_clause form.(i);
  done

let print_input fmt { form; n_vars } =
  Format.fprintf fmt "n_vars = %d\n" n_vars;
  Format.fprintf fmt "form = %a\n" print_form form

let print_assignment fmt assignment =
  Format.fprintf fmt "SAT\nAssignment:\n";
  for i = 1 to Array.length assignment - 1 do
    Format.fprintf fmt "%d -> %s\n" i (if assignment.(i) then "T" else "F");
  done

let print_proof_step fmt = function
  | Resolution r ->
      Format.fprintf fmt
        "%d: %a (Resolution of clauses %d and %d with %d as pivot)\n"
        r.new_clause_idx
        print_clause (Array.of_list r.new_clause)
        r.c1_idx
        r.c2_idx
        r.resolvant
  | InputClause _ -> ()

let print_proof_cert fmt cert =
  Format.fprintf fmt "UNSAT\nProof certificate:\n";
  List.iter (fun step -> Format.fprintf fmt "%a" print_proof_step step) cert.proof;
  Format.fprintf fmt "\n"

let print_cert fmt = function
  | Ok assignment -> print_assignment fmt assignment
  | Error cert    -> print_proof_cert fmt cert
