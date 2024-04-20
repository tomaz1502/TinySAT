(* Either Ok with the satisfying assignment or
   Err with the proof certificate of unsatisfiability *)
(* type output = (bool array, unsat_certificate) result *)

type assignment = bool array

type drat_clause = int list

type drat_cert = drat_clause list

type certificate = (assignment, drat_cert) result
