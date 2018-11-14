%start <ast.toplevel> prog

%%

prog:
  | f = func; EOF { TFunction f }
  | x = extern; EOF { x }
  | e = expr; EOF { TFunction (Function (Prototype ("", [| |]), e)) }
  ;

func:
  | DEF; p = proto; e = expr { Function (p, e) }
  ;

proto:
  | i = ID; LPAREN; a = list (ID); RPAREN { Prototype (i, a) }
  ;

extern:
  | EXTERN; p = proto { TExtern p }
  ;

expr:
  | n = NUMBER { Number n }
  | i = ID; LPAREN; a = separated_list (COMMA, expr); RPAREN { Call (i, a) }
  | LPAREN; e = expr; RPAREN { e }
  | i = ID { Variable i }
  | i1 = ID; ADD; i2 = ID { Binary ('+', i1, i2) }
  | i1 = ID; SUB; i2 = ID { Binary ('-', i1, i2) }
  | i1 = ID; MUL; i2 = ID { Binary ('*', i1, i2) }
  | i1 = ID; DIV; i2 = ID { Binary ('/', i1, i2) }
  ;
