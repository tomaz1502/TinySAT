open Lib.Parsed_struct

type clause = parsed_clause

type formula = parsed_formula

type _instance_data = parsed_instance_data

let eval (tbl: bool array) (l: literal): bool =
  assert (l <> 0);
  if l < 0 then not (tbl.(-l)) else tbl.(l)

let check_clause (c: clause) (tbl: bool array): bool =
  Array.fold_left (fun acc curr -> acc || eval tbl curr) false c

let check_assignment (f: formula) (tbl: bool array): bool =
  Array.fold_left (fun acc curr -> acc && check_clause curr tbl) true f

let solve (data: parsed_instance_data): bool =
  let n_vars = data.n_vars in
  let formula = data.formula in
  let rec loop i tbl =
    if i > n_vars then
      check_assignment formula tbl
    else begin
      tbl.(i) <- false;
      let s1 = loop (i + 1) tbl in
      if s1 then true
      else begin
        tbl.(i) <- true;
        let s2 = loop (i + 1) tbl in
        s2
      end
    end
  in
  let tbl = Array.make (n_vars + 1) false in
  loop 1 tbl
