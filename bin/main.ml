open Algs.Brute_force
(* open Basis.Types *)
(* open Basis.Angstrom_parser *)
open Basis.Util
open Basis.Parser
(* open Algs.Dpll *)

(* let _p1 = *)
(*     { form = [||]; *)
(*       n_vars = 0; *)
(*       _n_clauses = 0; *)
(*     } *)

(* let _p2 = *)
(*     { form = [| [|1|]; [|-1|] |]; *)
(*       n_vars = 1; *)
(*       _n_clauses = 2; *)
(*     } *)

(* let _p3 = *)
(*     { form = [| [|1|]; [|-2|] |]; *)
(*       n_vars = 2; *)
(*       _n_clauses = 2; *)
(*     } *)

(* let _p4 = *)
(*     { form = [| [|1; 2; -3|]; [|-2; 3|] |]; *)
(*       n_vars = 3; *)
(*       _n_clauses = 2; *)
(*     } *)

let () =
    let input = read_file "./data/ex_02.dimacs" in
    let d = dimacs_from_string input in
    match d with
    | Ok d ->
        brute_force d |>
        pp_sat |>
        print_endline
    | Error e -> failwith ("[Parser]: " ^ e)
