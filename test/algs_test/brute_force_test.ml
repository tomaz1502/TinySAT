open Algs.Brute_force;;
open Lib.Parser;;
open Lib.Util;;

open Check_assignment;;

let sat_test_from_instance_name i_name =
  let input = read_file ("data/" ^ i_name) in
  match dimacs_from_string input with
    | Ok dimacs -> begin
        match solve dimacs with
          | Ok assignment -> check_assignment dimacs assignment
          | Error _       -> false
    end
    | Error _ -> false

let%test "sat1.dimacs should be SAT" =
  sat_test_from_instance_name "sat1.dimacs"

let%test "ex_00.dimacs should be SAT" =
  sat_test_from_instance_name "ex_00.dimacs"

let%test "ex_01.dimacs should be SAT" =
  sat_test_from_instance_name "ex_01.dimacs"

let%test "ex_02.dimacs should be SAT" =
  sat_test_from_instance_name "ex_02.dimacs"
