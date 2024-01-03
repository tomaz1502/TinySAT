open Angstrom
open Types

let is_digit = function
  | '0'..'9' -> true
  | _        -> false

let is_white_space = function
  | ' ' -> true
  | _   -> false

let nat_p = take_while1 is_digit

let white_space_p = take_while is_white_space

let lit_p =
  peek_char >>= function
  | None -> fail "sorry"
  | Some '-' ->
    advance 1 *>
    take_while1 is_digit >>= fun s ->
    return (- (int_of_string s))
  | Some _   ->
    take_while1 is_digit >>= fun s ->
    if s = "0" then fail "done"
    else return (int_of_string s)

let clause_p = many1 (lit_p <* white_space_p)

let between_clauses_p =
  peek_char >>= function
  | None -> return ()
  | Some '0' -> begin
    advance 1 *>
    peek_char >>= function
    | None   -> return ()
    | Some _ -> advance 1 *> return ()
    end
  | Some _    -> fail ""

let dimacs_p =
  advance 6 *> (* 'p cnf ' *)
  nat_p >>= fun n_vars_s ->
  advance 1 *> (* ' ' *)
  nat_p >>= fun n_clauses_s ->
  advance 1 *> (* '\n' *)
  many (clause_p <* between_clauses_p) >>= fun form_l ->
  let form_a  = List.map Array.of_list form_l in
  let form_a' = Array.of_list form_a in
  return
    { form       = form_a'
    ; n_vars     = int_of_string n_vars_s
    ; _n_clauses = int_of_string n_clauses_s
    }

let parse_dimacs = parse_string ~consume:Prefix dimacs_p
