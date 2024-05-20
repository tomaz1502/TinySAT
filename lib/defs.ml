(* Unused for now *)
open Parsed_struct;;

module type Formula =
  sig
    type clause
    type t

    val cast_parsed_input : parsed_instance -> t
    val get_clause : t -> int -> clause
    val get_lit : clause -> int -> literal
    val form_len : t -> int
    val clause_length : clause -> int
    (* Assumes that clause is non empty *)
    val unsafe_get_arbitrary_lit : clause -> literal
    val fold_formula : ('a -> clause -> 'a) -> 'a -> t -> 'a
    val fold_clause : ('a -> literal -> 'a) -> 'a -> clause -> 'a
  end

module Solver (I : Formula) =
  struct
    module type M =
      sig
        val solve : I.t -> int -> bool
      end
  end

module I_arr_arr: Formula =
  struct
    type clause = literal array
    type t = clause array

    let cast_parsed_input inp = inp.form
    let get_clause = Array.unsafe_get
    let get_lit = Array.unsafe_get
    let form_len = Array.length
    let clause_length = Array.length
    let unsafe_get_arbitrary_lit cl = Array.unsafe_get cl 0
    let fold_formula = Array.fold_left
    let fold_clause = Array.fold_left
  end

