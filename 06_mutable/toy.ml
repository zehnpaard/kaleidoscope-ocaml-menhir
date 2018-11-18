open Lexing
open Lexer
open Llvm

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
      | SyntaxError msg -> print_endline msg; exit (-1)
      | Parser.Error -> print_endline "Parser error"; exit (-1)
;;

let check_asts asts =
    try let _ = List.fold_left Check.check_toplevel Check.fenv asts in ()
    with Check.Error msg -> print_endline msg; exit (-1)

let generate_and_dump = function
  | `TLMain func | `TLFunction func ->
        dump_value (Codegen.codegen_func func)

let main () =
  begin
    print_string "ready> "; 
    flush stdout;
    let input_string = read_line () in
    let lexbuf = Lexing.from_string input_string in
    let es = parse_with_error lexbuf in
    print_endline "Parse successful";
    check_asts es;
    List.iter generate_and_dump es;
    print_newline ()
  end
;;

main ();;
