open Lib.Defs

module Make(I : Input_format with type literal := int): Solver =
  struct
    let eval tbl l =
      assert (l <> 0);
      if l < 0 then not (tbl.(-l)) else tbl.(l)

    let check_clause c tbl =
      I.fold_clause (fun acc curr -> acc || eval tbl curr) false c

    let check_assignment f tbl =
      I.fold_formula (fun acc curr -> acc && check_clause curr tbl) true f

    let solve inp =
      let formula = I.cast inp in
      let rec loop i tbl =
        if i > inp.n_vars then
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
      let tbl = Array.make (inp.n_vars + 1) false in
      loop 1 tbl
  end
