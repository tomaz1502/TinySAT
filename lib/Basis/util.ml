open Types

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

let is_empty_list = function
    | [] -> true
    | _::_ -> false

let rec drop_while (p: 'a -> bool) (xs: 'a list) : 'a list =
    match xs with
    | []     -> []
    | x::xs' -> if p x then drop_while p xs' else x::xs'

let map_inplace (f : 'a -> 'a) (xs: 'a array) : unit =
    for i = 0 to Array.length xs - 1 do
        Array.unsafe_set xs i (f (Array.unsafe_get xs i))
    done

let comp f g = fun x -> f (g x)

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

let rev_array arr =
    let n = Array.length arr in
    Array.init n (fun i -> arr.(n - i - 1))

let pp_clause clause =
  String.concat " v " (List.map string_of_int (Array.to_list clause))

let pp_form form =
    let form_s = Array.map (fun c -> "( " ^ pp_clause c ^ " )") form in
    let s = Array.fold_right (fun curr acc -> curr ^ " ^ " ^ acc) form_s "" in
    String.sub s 0 (String.length s - 3) (* trim the last ' ^ ' *)

let pp_dimacs { form = form; n_vars = n_vars; _n_clauses = _n_clauses } =
    String.concat "\n"
    [ "n_vars = " ^ (string_of_int n_vars)
    ; "n_clauses = " ^ (string_of_int _n_clauses)
    ; "form = " ^ (pp_form form)
    ]

