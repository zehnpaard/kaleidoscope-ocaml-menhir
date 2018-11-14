%token <float> NUMBER
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | n = NUMBER { `Number n }
  ;
