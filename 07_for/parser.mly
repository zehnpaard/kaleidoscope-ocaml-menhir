%token <float> NUMBER
%token <string> ID
%token ADD
%token SUB
%token MUL
%token LT
%token ASSIGN
%token LPAREN
%token RPAREN
%token DEF
%token IF
%token THEN
%token ELSE
%token VAR
%token FOR
%token IN
%token COMMA
%token SEMICOLON
%token COLON
%token EOF

%left IN
%left COLON
%left ASSIGN
%left LT
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
  | e1 = expr; LT; e2 = expr { `BinOp ('<', e1, e2) }
  | e1 = expr; ASSIGN; e2 = expr { `BinOp ('=', e1, e2) }
  | e1 = expr; COLON; e2 = expr { `BinOp (':', e1, e2) }
  | VAR; i = ID; IN; e = expr { `Var (i, None, e) }
  | VAR; i = ID; ASSIGN; e1 = expr; IN; e2 = expr { `Var (i, Some e1, e2) }
  | FOR; i = ID; ASSIGN; se = expr; COMMA; ee = expr; IN; be = expr
      { `For (i, se, ee, None, be) }
  | FOR; i = ID; ASSIGN; se = expr; COMMA; ee = expr; COMMA; ue = expr; IN; be = expr
      { `For (i, se, ee, Some ue, be) }
  | i = ID; LPAREN; args = separated_list(COMMA, expr); RPAREN { `Call (i, args) }
  | IF; c = expr; THEN; e1 = expr; ELSE; e2 = expr { `If (c, e1, e2) }
  ;
