%token <int>    INT
%token <string> IDENT

%token EOF

%start<Ir.program> program
%%

program:
  | EOF { [] }
  | i = INT; EOF { print_int i; [] }
;