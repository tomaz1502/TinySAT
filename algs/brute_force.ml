open Lib.Parsed_struct
open Common

type clause = parsed_clause

type formula = parsed_formula

type _instance_data = parsed_instance_data

let eval (tbl: bool array) (l: literal): bool =
  assert (l <> 0);
  if l < 0 then not (tbl.(-l)) else tbl.(l)

let check_clause (c: clause) (tbl: bool array): output =
  let aux acc curr =
    if eval tbl curr then Ok () else acc
  in
  Array.fold_left aux (Error ()) c

let check_assignment (f: formula) (tbl: bool array): output =
  let aux acc curr =
    match acc with
      | Error _ -> Error ()
      | Ok _    -> check_clause curr tbl
  in
  Array.fold_left aux (Ok ()) f

let solve (data: parsed_instance_data): output =
  let n_vars = data.n_vars in
  let formula = data.formula in
  let rec loop i tbl =
    if i > n_vars then
      check_assignment formula tbl
    else begin
      tbl.(i) <- false;
      let s1 = loop (i + 1) tbl in
      match s1 with
        | Ok _ -> Ok ()
        | Error _ ->
            tbl.(i) <- true;
            loop (i + 1) tbl
    end
  in
  let tbl = Array.make (n_vars + 1) false in
  loop 1 tbl
