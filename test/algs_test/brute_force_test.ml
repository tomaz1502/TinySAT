open Algs.Brute_force;;
open Lib.Parser;;
open Lib.Util;;

open Check_assignment;;

let%test "sat1.dimacs should be SAT" =
  let input = read_file "./data/sat1.dimacs" in
  match dimacs_from_string input with
    | Ok dimacs -> begin
        match solve dimacs with
          | Ok assignment -> check_assignment dimacs assignment
          | Error _       -> false
    end
    | Error _err -> failwith "sorry"

