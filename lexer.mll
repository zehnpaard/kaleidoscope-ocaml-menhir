{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let float = digit+ '.'? digit*

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

let white = [' ' '\t']

rule read = parse
  | white { read lexbuf }
  | "def" { DEF }
  | "extern" { EXTERN }
  | float { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | id { ID (Lexing.lexeme lexbuf) }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
