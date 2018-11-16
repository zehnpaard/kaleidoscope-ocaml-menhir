%token <float> NUMBER
%token <string> ID
%token ADD
%token SUB
%token MUL
%token LPAREN
%token RPAREN
%token DEF
%token COMMA
%token SEMICOLON
%token EOF

%left ADD SUB
%left MUL

%start <Ast.toplevel list> prog

%%

prog:
  | tls = list(toplevel); EOF { tls }
  ;

toplevel:
  | e = expr; SEMICOLON { `TLMain (`Function (`Prototype ("", []), e)) }
  | f = func; SEMICOLON { `TLFunction f }
  ;

func:
  | DEF; p = proto; e = expr { `Function (p, e) }
  ;

proto:
  | name = ID; LPAREN; args = list (ID); RPAREN { `Prototype (name, args) }

expr:
  | i = ID { `Variable i }
  | n = NUMBER { `Number n }
  | LPAREN; e = expr; RPAREN { e }
  | e1 = expr; ADD; e2 = expr { `BinOp ('+', e1, e2) }
  | e1 = expr; SUB; e2 = expr { `BinOp ('-', e1, e2) }
  | e1 = expr; MUL; e2 = expr { `BinOp ('*', e1, e2) }
  | i = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN { `Call (i, args) }
  ;
