type parsed_input = int array array

module type Input_format =
  sig
    type literal (* usually will be just int, but who knows *)
    type clause
    type t (* formula *)

    val get_n_vars : t -> int
    (* val get_formula : unit -> t -> NOTE: I think the module should not carry a value *)
    val cast : parsed_input -> t

    val get_clause : t -> int -> clause
    val get_lit : clause -> int -> literal
    val fold_formula : ('a -> clause -> 'a) -> 'a -> t -> 'a
    val fold_clause : ('a -> literal -> 'a) -> 'a -> clause -> 'a
  end

module type Algorithm =
  sig
    val solve : parsed_input -> bool
  end

module Brute_force(I : Input_format with type literal := int): Algorithm =
  struct
    type assignment = bool array;;

    let eval tbl l =
      assert (l <> 0);
      if l < 0 then not (tbl.(-l)) else tbl.(l)

    let check_clause c tbl =
      I.fold_clause (fun acc curr -> acc || eval tbl curr) false c

    let check_assignment f tbl =
      I.fold_formula (fun acc curr -> acc && check_clause curr tbl) true f

    let solve (p: parsed_input): bool =
      let formula = I.cast p in
      let n_vars = I.get_n_vars formula in
      let rec loop (i: int) (tbl: assignment) =
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
      let tbl = Array.make (I.n_vars + 1) false in
      loop 1 tbl
  end
