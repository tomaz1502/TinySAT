open Lib.Parsed_struct
open Lib.Util
open Certificate

type clause = literal list

type formula = clause list

type _instance_data = parsed_instance_data

type var_val = True | False | Unassigned

let clause_of_proof_step = function
  | Resolution r -> r.new_clause
  | InputClause (c, _) -> c

let clause_index_of_proof_step = function
  | Resolution r -> r.new_clause_idx
  | InputClause (_, i) -> i

let mkResolution new_clause new_clause_idx c1_idx c2_idx resolvant =
  Resolution
    { new_clause; new_clause_idx; c1_idx; c2_idx; resolvant }

let mkProofCert proof added_clauses = { proof; added_clauses }

let cast_var_val: var_val -> bool = function
  | True       -> true
  | False      -> false
  | Unassigned -> failwith "[cast_var_val]: unreachable"

let eval (tbl: var_val array) (l: literal): bool =
  assert (l <> 0);
  match tbl.(Int.abs l) with
    | True       -> l > 0
    | False      -> l < 0
    | Unassigned -> true

let check_clause (c: clause) (tbl: var_val array): bool =
  List.fold_left (fun acc curr -> acc || eval tbl curr) false c

(* Check if there is a clause that is impossible to satisfy given the partial assignment *)
let get_unsat_clause (f: formula) (tbl: var_val array): (clause * int) option =
  findi_opt (fun c -> not (check_clause c tbl)) f

let resolve (r: literal) (c1: clause) (c2: clause) : clause =
  assert (List.mem r c1);
  assert (List.mem (-r) c2);
  let c1' = List.filter (fun l -> l <> r) c1 in
  let c2' = List.filter (fun l -> l <> -r) c2 in
  rem_dups (List.append c1' c2')

let solve ({ n_vars; form }: parsed_instance_data): certificate =
  let form_list = Array.to_list (Array.map Array.to_list form) in
  let n_clauses = Array.length form in
  (* n_clauses = number of clauses (initial + added) before running this node *)
  let rec loop i tbl n_clauses =
    match get_unsat_clause form_list tbl with
      | Some (c, i) -> Error (mkProofCert [InputClause (c, i + 1)] 0)
      | None -> begin
          if i > n_vars then Ok tbl
          else begin
            tbl.(i) <- False;
            let s1 = loop (i + 1) tbl n_clauses in
            match s1 with
              | Ok _ -> s1
              | Error cert1 ->
                  tbl.(i) <- True;
                  let s2 = loop (i + 1) tbl (n_clauses + cert1.added_clauses) in
                  match s2 with
                    | Ok _ -> s2
                    | Error cert2 ->
                        tbl.(i) <- Unassigned;
                        (* These lists are always non-empty *)
                        let c1 = List.hd cert1.proof in
                        let c2 = List.hd cert2.proof in
                        let c1_clause = clause_of_proof_step c1 in
                        let c2_clause = clause_of_proof_step c2 in
                        let c1_idx = clause_index_of_proof_step c1 in
                        let c2_idx = clause_index_of_proof_step c2 in
                        if List.mem i c1_clause && List.mem (-i) c2_clause then
                          let resolved = resolve i c1_clause c2_clause in
                          let added_clauses = cert1.added_clauses + cert2.added_clauses + 1 in
                          let step =
                            mkResolution resolved (n_clauses + added_clauses) c1_idx c2_idx i
                          in
                          let pf_cert =
                            mkProofCert (step :: List.append cert2.proof cert1.proof) added_clauses
                          in
                          Error pf_cert
                        else if not (List.mem (-i) c1_clause) then
                          Error cert1
                        else Error cert2
          end
      end
  in
  let tbl = Array.make (n_vars + 1) Unassigned in
  match loop 1 tbl n_clauses with
    | Ok tbl ->
        (* Recover table of bools from table of var_vals *)
        let cast_entry i =
          if i = 0 then false
          else cast_var_val tbl.(i)
        in
        Ok (Array.init (n_vars + 1) cast_entry)
    | Error cs -> Error (mkProofCert (List.rev cs.proof) cs.added_clauses)
