(* Either Ok with the satisfying assignment or
   Err with the proof certificate of unsatisfiability *)
(* type output = (bool array, unsat_certificate) result *)
type assignment = bool array

type output = (assignment, unit) result
