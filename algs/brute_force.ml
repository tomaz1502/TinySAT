open Lib.Parsed_struct
open Lib.Util
open Certificate

type clause = literal list

type formula = clause list

type _instance_data = parsed_instance_data

type var_val = True | False | Unassigned

let cast_var_val : var_val -> bool = function
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
let get_unsat_clause (f: formula) (tbl: var_val array): clause option =
  List.find_opt (fun c -> not (check_clause c tbl)) f

let resolve (r: literal) (c1: clause) (c2: clause) : clause =
  assert (List.mem r c1);
  assert (List.mem (-r) c2);
  let c1' = List.filter (fun l -> l <> r) c1 in
  let c2' = List.filter (fun l -> l <> -r) c2 in
  rem_dups (List.append c1' c2')

let solve ({ n_vars; form }: parsed_instance_data): certificate =
  let form_list = Array.to_list (Array.map Array.to_list form) in
  let rec loop i tbl =
    match get_unsat_clause form_list tbl with
      | Some c -> Error [c]
      | None -> begin
          if i > n_vars then Ok tbl
          else begin
            tbl.(i) <- False;
            let s1 = loop (i + 1) tbl in
            match s1 with
              | Ok _ -> s1
              | Error cs1 ->
                  tbl.(i) <- True;
                  let s2 = loop (i + 1) tbl in
                  match s2 with
                    | Ok _ -> s2
                    | Error cs2 ->
                        tbl.(i) <- Unassigned;
                        (* These lists are always non-empty *)
                        let c1 = List.hd cs1 in
                        let c2 = List.hd cs2 in
                        if List.mem i c1 && List.mem (-i) c2 then
                          let resolved = resolve i c1 c2 in
                          Error (resolved :: List.append cs1 cs2)
                        else if not (List.mem (-i) c1) then
                          Error cs1
                        else Error cs2
          end
      end
  in
  let tbl = Array.make (n_vars + 1) Unassigned in
  match loop 1 tbl with
    | Ok tbl ->
        (* Recover table of bools from table of var_vals *)
        let cast_entry i =
          if i = 0 then false
          else cast_var_val tbl.(i)
        in
        Ok (Array.init (n_vars + 1) cast_entry)
    | Error cs -> Error (List.rev cs)
