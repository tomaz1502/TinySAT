open Algs;;
open Lib.Parser;;
open Lib.Util;;

open Checkers;;

let sat_test_from_instance_name i_name =
  let input = read_file ("../test_cases/sat/" ^ i_name) in
  match dimacs_from_string input with
    | Ok dimacs -> begin
        match Dpll.solve dimacs with
          | Ok assignment -> check_assignment dimacs assignment
          | Error _       -> false
    end
    | Error _ -> false

let unsat_test_from_instance_name i_name =
  let input = read_file ("../test_cases/unsat/" ^ i_name) in
  match dimacs_from_string input with
    | Ok dimacs -> Result.is_error (Dpll.solve dimacs)
    | _ -> false

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

let%test "UNSAT for dubois20.dimacs" =
  unsat_test_from_instance_name "dubois20.dimacs"

let%test "UNSAT for gpt1.dimacs" =
  unsat_test_from_instance_name "gpt1.dimacs"

let%test "UNSAT for unsat1.dimacs" =
  unsat_test_from_instance_name "unsat1.dimacs"

let%test "UNSAT for handcraft1.dimacs" =
  unsat_test_from_instance_name "handcraft1.dimacs"

