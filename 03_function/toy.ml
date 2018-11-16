open Lexing
open Lexer
open Llvm

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
      | SyntaxError msg -> print_endline msg; exit (-1)
      | Parser.Error -> print_endline "Parser error"; exit (-1)
;;

let main () =
  begin
    print_string "ready> "; 
    flush stdout;
    let input_string = read_line () in
    let lexbuf = Lexing.from_string input_string in
    let e = parse_with_error lexbuf in
    match e with
      | `TLMain func | `TLFunction func ->
            begin
                print_endline "Parse successful";
                dump_value (Codegen.codegen_func func);
                print_newline ()
            end
  end
;;

main ();;
