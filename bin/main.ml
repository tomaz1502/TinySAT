open Basis.Util
open Basis.Parser
(* open Algs.Brute_force *)
(* open Algs.Dpll *)

let input_file = ref ""
let output_file = ref ""
let usage_msg = "tinysat --input=<path/to/input> [--output=<path/to/output>]"

let others = fun _ -> 
  print_endline "Unexpected command line argument.";
  print_endline ("Usage: " ^ usage_msg);
  exit 1;;

let speclist =
  [ ("--input", Arg.Set_string input_file, "Path to input DIMACS file.")
  ; ("--output", Arg.Set_string output_file,
     "[Optional] Output file. If no file is specified, the output will be printed in stdout.")
  ]

let () =
  Arg.parse speclist others usage_msg;
  let input = read_file !input_file in
  match dimacs_from_string input with
  | Ok dim ->
    Format.printf "Input:\n%a" print_dimacs dim
  | Error err ->
    Format.printf "[Parsing Error]: %s" err
