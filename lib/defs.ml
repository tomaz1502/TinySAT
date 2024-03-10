type literal = int

type parsed_input =
  { formula : literal array array
  ; n_vars  : int
  }

module type Input_format =
  sig
    type clause
    type t

    val cast_parsed_input : parsed_input -> t
    val get_clause : t -> int -> clause
    val get_lit : clause -> int -> literal
    val fold_formula : ('a -> clause -> 'a) -> 'a -> t -> 'a
    val fold_clause : ('a -> literal -> 'a) -> 'a -> clause -> 'a
  end

module Solver (I : Input_format) =
  struct
    module type M =
      sig
        (*          form   n_vars*)
        val solve : I.t -> int -> bool
      end
  end

module I_arr_arr =
  struct
    type clause = literal array
    type t = clause array

    let cast_parsed_input inp = inp.formula
    let get_clause = Array.unsafe_get
    let get_lit = Array.unsafe_get
    let fold_formula = Array.fold_left
    let fold_clause = Array.fold_left
  end

