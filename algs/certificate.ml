open Lib.Util;;

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

let clause_of_proof_step = function
  | Resolution r -> (r.new_clause_idx, r.new_clause)
  | InputClause (c, i) -> (i, c)

let mkResolution new_clause new_clause_idx c1_idx c2_idx resolvant =
  Resolution
    { new_clause; new_clause_idx; c1_idx; c2_idx; resolvant }

let mkProofCert proof added_clauses = { proof; added_clauses }

let resolve r c1 c2 =
  assert (List.mem r c1);
  assert (List.mem (-r) c2);
  let c1' = List.filter (fun l -> l <> r) c1 in
  let c2' = List.filter (fun l -> l <> -r) c2 in
  rem_dups (List.append c1' c2')
