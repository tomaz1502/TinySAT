open Algs;;
open Lib.Parser;;
open Lib.Util;;

open Check_assignment;;

let sat_test_from_instance_name i_name =
  let input = read_file ("../test_cases/sat/" ^ i_name) in
  match dimacs_from_string input with
    | Ok dimacs -> begin
        match Brute_force.solve dimacs with
          | Ok assignment -> check_assignment dimacs assignment
          | Error _       -> false
    end
    | Error _ -> false

let unsat_test_from_instance_name i_name =
  let input = read_file ("../test_cases/unsat/" ^ i_name) in
  match dimacs_from_string input with
    | Ok dimacs -> Result.is_error (Brute_force.solve dimacs)
    | _ -> false

let%test "SAT certificate for sat1.dimacs" =
  sat_test_from_instance_name "sat1.dimacs"

let%test "SAT certificate for ex_00.dimacs" =
  sat_test_from_instance_name "ex_00.dimacs"

let%test "SAT certificate for ex_01.dimacs" =
  sat_test_from_instance_name "ex_01.dimacs"

let%test "SAT certificate for ex_02.dimacs" =
  sat_test_from_instance_name "ex_02.dimacs"

let%test "UNSAT for gpt1.dimacs" =
  unsat_test_from_instance_name "gpt1.dimacs"

let%test "UNSAT for unsat1.dimacs" =
  unsat_test_from_instance_name "unsat1.dimacs"

let%test "UNSAT for handcraft1.dimacs" =
  unsat_test_from_instance_name "handcraft1.dimacs"
