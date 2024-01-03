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
    Array.to_list clause   |>
    List.map string_of_int |>
    String.concat " v "

let pp_form form =
    Array.to_list form                            |>
    List.map (fun c -> "( " ^ pp_clause c ^ " )") |>
    String.concat " ^ "

let pp_dimacs { form = form; n_vars = n_vars; _n_clauses = _n_clauses } =
    String.concat "\n"
    [ "n_vars = " ^ (string_of_int n_vars)
    ; "n_clauses = " ^ (string_of_int _n_clauses)
    ; "form = " ^ (pp_form form)
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

let print_dimacs fmt { form = form; n_vars = n_vars; _n_clauses = n_clauses } =
    Format.fprintf fmt "n_vars = %d\n" n_vars;
    Format.fprintf fmt "n_clauses = %d\n" n_clauses;
    Format.fprintf fmt "form = %a" print_form form
