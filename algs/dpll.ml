(* Implementation of Davis–Putnam–Logemann–Loveland algorithm *)

open Lib.Parsed_struct
open Lib.Util
open Certificate

module SI = Set.Make(Int)

type clause = SI.t

type formula = clause array

type instance_data =
  { form: formula
  ; n_vars: int
  }

let set_of_arr (arr: literal array): SI.t =
  SI.of_list (Array.to_list arr)

let cast_input (d: parsed_instance_data): instance_data =
  {form = Array.map set_of_arr d.form; n_vars = d.n_vars}

let filter (p: 'a -> bool) (arr: 'a array): 'a array =
  let res = ref [] in
  for i = Array.length arr - 1 downto 0 do
    if p arr.(i) then
      res := arr.(i) :: !res;
  done;
  Array.of_list !res

let get_pure_lits (input: instance_data): int list =
  let occ = Array.make (2 * input.n_vars + 1) false in
  let upd l = occ.(l + input.n_vars) <- true in
  Array.iter (SI.iter upd) input.form;
  let lits = ref [] in
  for i = input.n_vars downto 1 do
    let occ_pos = if occ.(i + input.n_vars) then true else false in
    let occ_neg = if occ.(-i + input.n_vars) then true else false in
    if occ_pos && not occ_neg then lits := i :: !lits;
    if not occ_pos && occ_neg then lits := -i :: !lits;
  done;
  !lits

let get_unit_lits: formula -> int list = fun f ->
  let unit_clauses = ref [] in
  for i = Array.length f - 1 downto 0 do
    if SI.cardinal f.(i) = 1 then
      unit_clauses := SI.min_elt f.(i) :: !unit_clauses;
  done;
  !unit_clauses

let upd_tbl (tbl: bool array) (lit: literal): unit =
  if lit < 0 then tbl.(-lit) <- false else tbl.(lit) <- true

(* If l is positive assign true, if l is negative, assign false *)
let assign (l: literal) (form: formula) (tbl: bool array): formula =
  let res = ref [] in
  for i = 0 to Array.length form - 1 do
    if not (SI.mem l form.(i)) then
      res := SI.remove (-l) form.(i) :: !res;
  done;
  upd_tbl tbl l;
  Array.of_list !res

let rec run (tbl: bool array) (input: instance_data): bool =
  let form = input.form in
  let n_vars = input.n_vars in
  if Array.length form = 0 then true
  else if Array.mem SI.empty form then false
  else
    match get_unit_lits form with
      | _::_ as u_lits ->
          (* Unit Propagation *)
          List.iter (upd_tbl tbl) u_lits;
          let u_lits_set = SI.of_list u_lits in
          let rem_set = SI.map (fun i -> -i) u_lits_set in
          map_inplace (fun c -> SI.diff c rem_set) form;
          if Array.mem SI.empty form then false
          else
            let new_form = filter (fun c -> SI.is_empty (SI.inter c u_lits_set)) form in
            run tbl { form = new_form; n_vars = n_vars }
      | [] -> begin
        match get_pure_lits input with
          | _::_ as p_lits -> 
              (* Pure literal elimination *)
              List.iter (upd_tbl tbl) p_lits;
              let rem_set = SI.of_list p_lits in
              let new_form =
                filter (fun c -> SI.is_empty (SI.inter c rem_set)) form in
              run tbl { form = new_form; n_vars = n_vars }
          | [] ->
              (* Backtracking *)
              let l = SI.min_elt form.(0) in
              let try_pos =
                run tbl { form = assign l form tbl; n_vars = n_vars } in
              if try_pos then true
              else
                run tbl { form = assign (-l) form tbl; n_vars = n_vars }
      end

let solve (pf: parsed_instance_data): certificate =
  let tbl = Array.make (pf.n_vars + 1) false in
  let sat = cast_input pf |> run tbl in
  if sat then Ok tbl else Error []
