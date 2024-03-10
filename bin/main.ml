open Lib.Util
open Lib.Parser
open Lib.Defs
open Algs

type mode = Parse | Solve
type algo = Dpll | Brute_force

let exec_mode = ref Solve
let chosen_algo = ref Dpll
let input_file = ref ""
let output_file = ref ""
let usage_msg = "tinysat --input=<path/to/input> [--output=<path/to/output>] \
                [--only-parse] [--algo=<algo_name>]"

let unexpected_cmd = fun _ -> 
  print_endline "Unexpected command line argument.";
  print_endline ("Usage: " ^ usage_msg);
  exit 1;;

let speclist =
  [ ("--input", Arg.Set_string input_file, ": Path to input DIMACS file.")
  ; ("--output", Arg.Set_string output_file,
       ": [Optional] Output file. If no file is specified, the output will be \
       printed in stdout.")
  ; ("--only-parse", Arg.Unit (fun _ -> exec_mode := Parse), ": Only parse \
     input file and print the result.")
  ; ("--algo", Arg.String (function
                             | "dpll"        -> chosen_algo := Dpll
                             | "brute_force" -> chosen_algo := Brute_force
                             | _             -> unexpected_cmd ()),
       ": Set the solving algorithm. Available algorithms: `dpll`, \
       `brute_force`. Default is `dpll`.")
  ]

module B = Brute_force.Make (I_arr_arr)
module D = Dpll.Make (I_arr_arr)

let () =
  Arg.parse speclist unexpected_cmd usage_msg;
  let input = read_file !input_file in
  let output_chan =
    match !output_file with
    | "" -> stdout
    | _  -> open_out !output_file
  in
  Format.set_formatter_out_channel output_chan;
  match dimacs_from_string input with
  | Ok inp -> begin
    match !exec_mode with
    | Parse ->
        Format.printf "Input:\n%a\n" print_input inp;
    | Solve ->
        let solution =
          match !chosen_algo with
          | Dpll -> D.solve (I_arr_arr.cast_parsed_input inp) inp.n_vars
          | Brute_force -> B.solve (I_arr_arr.cast_parsed_input inp) inp.n_vars
        in
        Format.printf "%a@\n" print_sat solution
    end
  | Error err ->
    Format.printf "[Parsing Error]: %s" err
