%token ADD SUB
%token MUL DIV REC
%token LPAR RPAR
%token <int> INT
%token EOF

%left ADD SUB
%left MUL DIV
%nonassoc REC
%nonassoc USUB

%type <int> e

%start <unit> main

%%

main:
| expression = e EOF                            { Printf.printf "\t%.10d\n" expression; flush stdout }

e:
| expression1 = e ADD expression2 = e           { expression1 + expression2 }
| expression1 = e SUB expression2 = e           { expression1 - expression2 }
| expression1 = e MUL expression2 = e           { expression1 * expression2 }
| expression1 = e DIV expression2 = e           { expression1 / expression2 }
| SUB expression = e %prec USUB                 { - expression }
| expression = e REC                            { 1 / expression }
| expression = INT                              { expression }
| LPAR expression = e RPAR                      { expression }
