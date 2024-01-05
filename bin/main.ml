open Basis.Util
open Basis.Parser
open Algs.Brute_force
(* open Algs.Dpll *)

type mode =
    Parse
  | Solve

let exec_mode = ref Solve
let input_file = ref ""
let output_file = ref ""
let usage_msg = "tinysat --input=<path/to/input> [--output=<path/to/output>] [--only-parse]"

let others = fun _ -> 
  print_endline "Unexpected command line argument.";
  print_endline ("Usage: " ^ usage_msg);
  exit 1;;

let speclist =
  [ ("--input", Arg.Set_string input_file, ": Path to input DIMACS file.")
  ; ("--output", Arg.Set_string output_file,
     ": [Optional] Output file. If no file is specified, the output will be printed in stdout.")
  ; ("--only-parse", Arg.Unit (fun _ -> exec_mode := Parse), ": Only parse input file and print the result.")
  ]

let () =
  Arg.parse speclist others usage_msg;
  let input = read_file !input_file in
  match dimacs_from_string input with
  | Ok dim -> begin
    match !exec_mode with
    | Parse ->
        Format.printf "Input:\n%a" print_dimacs dim;
    | Solve ->
      brute_force dim |>
      Format.printf "%a@\n" print_sat
    end
  | Error err ->
    Format.printf "[Parsing Error]: %s" err
