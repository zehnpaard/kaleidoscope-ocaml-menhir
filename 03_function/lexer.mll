{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let float = digit+ '.'? digit*

let white = [' ' '\t']+

rule read = parse
  | float { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | white { read lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
