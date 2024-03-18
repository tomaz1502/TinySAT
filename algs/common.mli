(* Either Ok with the satisfying assignment or
   Err with the proof certificate of unsatisfiability *)
(* type output = (bool array, unsat_certificate) result *)
type output = (bool array, unit) result
