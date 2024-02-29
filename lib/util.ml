open Defs

exception UnexpectedInput

let read_file (file_name: string) : string =
  let rec read_chan (rc: in_channel): string list =
    try
      let l = input_line rc in
      let r = read_chan rc in
      l :: r
    with End_of_file -> []
  in
  let chan = open_in file_name in
  let result = read_chan chan in
  close_in chan;
  String.concat "\n" result

let is_not_empty_list = function
  | [] -> false
  | _::_ -> true

let rec drop_while (p: 'a -> bool) (xs : 'a list) : 'a list =
  match xs with
  | []     -> []
  | x::xs' -> if p x then drop_while p xs' else x::xs'

let map_inplace (f : 'a -> 'a) (xs : 'a array) : unit =
  for i = 0 to Array.length xs - 1 do
    Array.unsafe_set xs i (f (Array.unsafe_get xs i))
  done

let comp f g = fun x -> f (g x)

let split_list_on_elem elem list =
  let rec go elem acc list =
    match list with
      | [] -> acc
      | hd :: tl ->
          if hd = elem then
            go elem ([] :: acc) tl
          else
            match acc with
              | [] -> go elem [[hd]] tl
              | hd_acc :: tl_acc -> go elem ((hd :: hd_acc) :: tl_acc) tl
  in
  go elem [] list               |>
  List.filter is_not_empty_list |>
  List.rev                      |>
  List.map List.rev

let int_of_string_opt' x =
  match int_of_string_opt x with
  | Some x -> Some x
  | None   ->
    if x = "" then None else raise UnexpectedInput

let scan_int_list s =
  String.split_on_char ' ' s |>
  List.filter_map int_of_string_opt'

let pp_sat = function
  | true  -> "SAT"
  | false -> "UNSAT"

let print_sat fmt b = 
  Format.fprintf fmt "%s" (if b then "SAT" else "UNSAT")

let rev_array arr =
  let n = Array.length arr in
  Array.init n (fun i -> arr.(n - i - 1))

let pp_clause clause =
  Array.to_list clause   |>
  List.map string_of_int |>
  String.concat " v "

let pp_form form =
  Array.to_list form                            |>
  List.map (fun c -> "( " ^ pp_clause c ^ " )") |>
  String.concat " ^ "

let pp_input { formula; n_vars } =
  String.concat "\n"
  [ "n_vars = " ^ (string_of_int n_vars)
  ; "form = " ^ (pp_form formula)
  ]

let print_clause fmt c =
  if Array.length c > 0 then
    Format.fprintf fmt "%d" c.(0);
  for i = 1 to Array.length c - 1 do
    Format.fprintf fmt " v %d" c.(i)
  done

let print_form fmt form =
  if Array.length form > 0 then
    Format.fprintf fmt "(%a)" print_clause form.(0);
  for i = 1 to Array.length form - 1 do
    Format.fprintf fmt " ^ (%a)" print_clause form.(i);
  done

let print_input fmt { formula; n_vars } =
  Format.fprintf fmt "n_vars = %d\n" n_vars;
  Format.fprintf fmt "form = %a" print_form formula
