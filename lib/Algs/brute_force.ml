open Basis.Types
open Basis.Util

let eval (tbl: assignment) (l: lit) =
    assert (l <> 0);
    if l < 0 then not (tbl.(-l)) else tbl.(l)

let check_clause (c: clause) (tbl: assignment): bool =
    Array.fold_left (fun acc curr -> acc || eval tbl curr) false c

let check_assignment (f: formula) (tbl: assignment) : bool =
    Array.fold_left (fun acc curr -> acc && check_clause curr tbl) true f

let brute_force (p: dimacs): bool =
    Printf.printf "%s\n" (pp_form p.form);
    let rec loop (i: int) (tbl: assignment) =
        if i > p.n_vars then
            check_assignment p.form tbl
        else begin
            tbl.(i) <- false;
            let s1 = loop (i + 1) tbl in
            tbl.(i) <- true;
            let s2 = loop (i + 1) tbl in
            s1 || s2
        end
    in
    let tbl = Array.make (p.n_vars + 1) false in
    loop 1 tbl
