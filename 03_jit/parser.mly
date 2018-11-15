%token <float> NUMBER
%token ADD
%token SUB
%token MUL
%token LPAREN
%token RPAREN
%token EOF

%left ADD SUB
%left MUL

%start <Ast.toplevel> prog

%%

prog:
  | e = expr; EOF { `TLMain (`Function (`Prototype ("", []), e)) }
  ;

expr:
  | n = NUMBER { `Number n }
  | LPAREN; e = expr; RPAREN { e }
  | e1 = expr; ADD; e2 = expr { `BinOp ('+', e1, e2) }
  | e1 = expr; SUB; e2 = expr { `BinOp ('-', e1, e2) }
  | e1 = expr; MUL; e2 = expr { `BinOp ('*', e1, e2) }
  ;
