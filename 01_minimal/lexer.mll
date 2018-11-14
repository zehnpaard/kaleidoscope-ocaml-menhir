{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let float = digit+ '.'? digit*

rule read = parse
  | float { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
