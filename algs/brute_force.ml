open Lib.Defs

(* TODO: is it possible to make a module type for this? *)
module Make(I : Input_format): Solver(I).M =
  struct
    let eval tbl l =
      assert (l <> 0);
      if l < 0 then not (tbl.(-l)) else tbl.(l)

    let check_clause c tbl =
      I.fold_clause (fun acc curr -> acc || eval tbl curr) false c

    let check_assignment f tbl =
      I.fold_formula (fun acc curr -> acc && check_clause curr tbl) true f

    let solve formula n_vars =
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
  end
