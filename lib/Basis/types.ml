(* We assume CNF initially *)

(* -x -> x negated *)
type lit = int;;

type clause = lit array;;

type formula = clause array;;

type dimacs =
    { form: formula;
      n_vars: int;
      _n_clauses: int
    }

type assignment = bool array;;
module type Format = sig
    type formula
end;;

module Make (F : Format) = struct
    type input = {
        form:   F.formula
    ;   n_vars: int
    }
end
