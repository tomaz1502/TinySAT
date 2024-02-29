type parsed_input =
  { formula : int array array
  ; n_vars  : int
  }

module type Input_format =
  sig
    type literal (* usually will be just int, but who knows *)
    type clause
    type t (* formula *)

    val cast : parsed_input -> t
    val get_clause : t -> int -> clause
    val get_lit : clause -> int -> literal
    val fold_formula : ('a -> clause -> 'a) -> 'a -> t -> 'a
    val fold_clause : ('a -> literal -> 'a) -> 'a -> clause -> 'a
  end

module type Solver =
  sig
    val solve : parsed_input -> bool
  end

module I_arr_arr =
  struct
    type literal = int
    type clause = literal array
    type t = clause array

    let cast p = p.formula
    let get_clause = Array.unsafe_get
    let get_lit = Array.unsafe_get
    let fold_formula = Array.fold_left
    let fold_clause = Array.fold_left
  end

