(* Either Ok with the satisfying assignment or
   Err with the proof certificate of unsatisfiability *)

type assignment = bool array

type proof_step =
  | Resolution of
      { new_clause: int list
      ; new_clause_idx: int
      ; c1_idx: int
      ; c2_idx: int
      ; resolvant: int
      }
  | InputClause of int list * int

type proof_cert =
  { proof: proof_step list
  ; added_clauses: int
  }

type certificate = (assignment, proof_cert) result

val clause_of_proof_step : proof_step -> (int * int list)

val mkResolution : int list -> int -> int -> int -> int -> proof_step

val mkProofCert : proof_step list -> int -> proof_cert

val resolve : int -> int list -> int list -> int list
