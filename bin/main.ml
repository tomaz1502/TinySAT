open Lib.Util
open Lib.Parser

type mode = Parse | Solve | Certify
type algo = Dpll | Brute_force

let exec_mode = ref Solve
let chosen_algo = ref Dpll
let input_file = ref ""
let output_file = ref ""
let usage_msg = "tinysat <path/to/input> [--output=<path/to/output>] \
                [--mode=<mode_name>] [--algo=<algo_name>]"

let unexpected_cmd = fun _ -> 
  print_endline "Unexpected command line argument.";
  print_endline ("Usage: " ^ usage_msg);
  exit 1;;

let anon_fun str =
  if !input_file <> "" then unexpected_cmd str
  else input_file := str

let speclist =
  [ ("--output", Arg.Set_string output_file,
       "[Optional] Output file. If no file is specified, the output will be \
       printed in stdout.")
  ; ("--mode", Arg.String (function 
                             | "parse" -> exec_mode := Parse
                             | "solve" -> exec_mode := Solve
                             | "certify" -> exec_mode := Certify
                             | _ -> unexpected_cmd ()),
     "[Optional] Select the mode to run the solver. Options are: `parse`, \
     `solve` and `certify`. Default is `solve`.")
  ; ("--algo", Arg.String (function
                             | "dpll"        -> chosen_algo := Dpll
                             | "brute_force" -> chosen_algo := Brute_force
                             | _             -> unexpected_cmd ()),
       "[Optional] Set the solving algorithm. Available algorithms: `dpll` and \
       `brute_force`. Default is `dpll`.")
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !input_file = String.empty then unexpected_cmd ();
  let input = read_file !input_file in
  let output_chan =
    if !output_file = String.empty then stdout else open_out !output_file
  in
  Format.set_formatter_out_channel output_chan;
  let algo =
    match !chosen_algo with
      | Dpll -> Algs.Dpll.solve
      | Brute_force -> Algs.Brute_force.solve
  in
  match dimacs_from_string input with
  | Ok inp -> begin
    match !exec_mode with
    | Parse   -> Format.printf "Input:\n%a\n" print_input inp;
    | Solve   -> Format.printf "%a" print_sat (algo inp);
    | Certify -> Format.printf "%a" print_cert (algo inp);
  end
  | Error err ->
    Format.printf "[Parsing Error]: %s" err
