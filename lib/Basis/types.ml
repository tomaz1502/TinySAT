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
