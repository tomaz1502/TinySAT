open Algs;;
open Lib.Parser;;
open Lib.Util;;

open Check_assignment;;

let sat_test_from_instance_name i_name =
  let input = read_file ("../sat_cases/" ^ i_name) in
  match dimacs_from_string input with
    | Ok dimacs -> begin
        match Dpll.solve dimacs with
          | Ok assignment -> check_assignment dimacs assignment
          | Error _       -> false
    end
    | Error _ -> false

let%test "SAT certificate for sat1.dimacs" =
  sat_test_from_instance_name "sat1.dimacs"

let%test "SAT certificate for ex_00.dimacs" =
  sat_test_from_instance_name "ex_00.dimacs"

let%test "SAT certificate for ex_01.dimacs" =
  sat_test_from_instance_name "ex_01.dimacs"

let%test "SAT certificate for ex_02.dimacs" =
  sat_test_from_instance_name "ex_02.dimacs"

let%test "SAT certificate for 100vars_sat.dimacs" =
  sat_test_from_instance_name "100vars_sat.dimacs"

let%test "SAT certificate for uf20-01000.dimacs" =
  sat_test_from_instance_name "uf20-01000.dimacs"

