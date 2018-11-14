%token <string> ID
%token <float> NUMBER
%token LPAREN
%token RPAREN
%token COMMA
%token DEF
%token EXTERN
%token EOF

%token ADD
%token SUB
%token MUL
%token DIV

%left ADD SUB
%left MUL DIV

%start <Ast.toplevel> prog

%%

prog:
  | f = func; EOF { `TFunction f }
  | x = extern; EOF { x }
  | e = expr; EOF { `TFunction (`Function (`Prototype ("", []), e)) }
  ;

func:
  | DEF; p = proto; e = expr { `Function (p, e) }
  ;

proto:
  | i = ID; LPAREN; a = list (ID); RPAREN { `Prototype (i, a) }
  ;

extern:
  | EXTERN; p = proto { `TExtern p }
  ;

expr:
  | n = NUMBER { `Number n }
  | i = ID; LPAREN; a = separated_list (COMMA, expr); RPAREN { `Call (i, a) }
  | LPAREN; e = expr; RPAREN { e }
  | i = ID { `Variable i }
  | e1 = expr; ADD; e2 = expr { `Binary ('+', e1, e2) }
  | e1 = expr; SUB; e2 = expr { `Binary ('-', e1, e2) }
  | e1 = expr; MUL; e2 = expr { `Binary ('*', e1, e2) }
  | e1 = expr; DIV; e2 = expr { `Binary ('/', e1, e2) }
  ;
