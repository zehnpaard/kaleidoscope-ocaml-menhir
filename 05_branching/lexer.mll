{
open Parser
open Lexing

exception SyntaxError of string
}

let digit = ['0'-'9']
let float = digit+ '.'? digit*

let white = [' ' '\t']+

let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule read = parse
  | float { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | "def" { DEF }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | white { read lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '<' { LT }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
