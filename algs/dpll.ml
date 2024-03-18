(* Implementation of Davis–Putnam–Logemann–Loveland algorithm *)

open Lib.Parsed_struct
open Lib.Util
open Common

module SI = Set.Make(Int)

type clause = SI.t

type formula = clause array

type instance_data =
  { formula: formula
  ; n_vars: int
  }

let set_of_arr (arr: literal array): SI.t =
  SI.of_list (Array.to_list arr)

let cast_input (d: parsed_instance_data): instance_data =
  {formula = Array.map set_of_arr d.formula; n_vars = d.n_vars}

let filter (p: 'a -> bool) (arr: 'a array): 'a array =
  let res = ref [] in
  for i = Array.length arr - 1 downto 0 do
    if p arr.(i) then
      res := arr.(i) :: !res;
  done;
  Array.of_list !res

let get_pure_lits (input: instance_data): int list =
  let occ = Array.make (2 * input.n_vars) false in
  let upd l =
    if l < 0 then
      occ.(-l + input.n_vars - 1) <- true
    else occ.(l - 1) <- true in
  Array.iter (SI.iter upd) input.formula;
  let lits = ref [] in
  for i = input.n_vars - 1 downto 0 do
    let occ_pos = if occ.(i) then true else false in
    let occ_neg = if occ.(i + input.n_vars) then true else false in
    if occ_pos && not occ_neg then lits := (i + 1) :: !lits;
    if not occ_pos && occ_neg then lits := (-i - 1) :: !lits;
  done;
  !lits

let get_unit_lits: formula -> int list = fun f ->
  let unit_clauses = ref [] in
  for i = Array.length f - 1 downto 0 do
    if SI.cardinal f.(i) = 1 then
      unit_clauses := SI.min_elt f.(i) :: !unit_clauses;
  done;
  !unit_clauses

(* If l is positive assign true, if l is negative, assign false *)
let assign (l: literal) (form: formula): formula =
  let res = ref [] in
  for i = 0 to Array.length form - 1 do
    if not (SI.mem l form.(i)) then
      res := SI.remove (-l) form.(i) :: !res;
  done;
  Array.of_list !res

(* TODO: retrieve assignment in SAT case *)
let rec run (input: instance_data): output =
  let form = input.formula in
  let n_vars = input.n_vars in
  if Array.length form = 0 then Ok [||]
  else if Array.mem SI.empty form then Error ()
  else
    match get_unit_lits form with
      | _::_ as u_lits ->
          (* Unit Propagation *)
          let rem_set = SI.of_list (List.map (fun i -> -i) u_lits) in
          map_inplace (fun c -> SI.diff c rem_set) form;
          if Array.mem SI.empty form then Error ()
          else
            let new_form = filter (fun c -> SI.cardinal c > 1) form in
            run { formula = new_form; n_vars = n_vars }
      | [] -> begin
        match get_pure_lits input with
          | _::_ as p_lits -> 
              let rem_set = SI.of_list p_lits in
              let new_form = filter (fun c -> SI.is_empty (SI.inter c rem_set)) form in
              (* Pure literal elimination *)
              run { formula = new_form; n_vars = n_vars }
          | [] -> begin
            let l = SI.min_elt form.(0) in
            let try_pos = run { formula = assign l form; n_vars = n_vars } in
            match try_pos with
              | Ok _ -> Ok [||]
              | _ -> run { formula = assign (-l) form; n_vars = n_vars }
          end
      end

let solve (pf: parsed_instance_data): output =
  cast_input pf |> run
