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

let is_not_empty_list: 'a list -> bool = function
  | [] -> false
  | _::_ -> true

let rec drop_while (p: 'a -> bool) (xs : 'a list) : 'a list =
  match xs with
  | []     -> []
  | x::xs' -> if p x then drop_while p xs' else x::xs'

let rec rem_dups (xs: 'a list): 'a list =
  match xs with
    | [] -> []
    | hd :: tl -> hd :: rem_dups (List.filter (fun x -> x <> hd) tl)

let rec take (i: int) (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | hd::tl -> if i > 0 then hd :: take (i - 1) tl else []

let map_inplace (f : 'a -> 'a) (xs : 'a array) : unit =
  for i = 0 to Array.length xs - 1 do
    Array.unsafe_set xs i (f (Array.unsafe_get xs i))
  done

let comp (f: 'b -> 'c) (g: 'a -> 'b): 'a -> 'c = fun x -> f (g x)

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

let int_of_string_opt' (x: string): int option =
  match int_of_string_opt x with
  | Some x -> Some x
  | None   ->
    if x = "" then None else raise UnexpectedInput

let scan_int_list (s: string): int list =
  String.split_on_char ' ' s |>
  List.filter_map int_of_string_opt'

let rev_array (arr: 'a array): 'a array =
  let n = Array.length arr in
  Array.init n (fun i -> arr.(n - i - 1))

let findi_opt (p: 'a -> bool) (xs: 'a list): ('a * int) option =
  let p' (a, _) = p a in
  let xs' = List.combine xs (List.init (List.length xs) Fun.id) in
  List.find_opt p' xs'
