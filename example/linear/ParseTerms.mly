
%% 


%public term:
| polish       { $1 }
| infix        { $1 }
| simple       { $1 }

simple:
| TK_NAT            { Term.Int($1)}
| TK_STR            { Term.Var($1)}

polish:
| TK_ADD polish2 polish2  { Term.Add($2,$3)}
| TK_MUL polish2 polish2  { Term.Mul($2,$3)}
| TK_SUB polish2 polish2  { Term.Sub($2,$3)}
| TK_DIV polish2 polish2  { Term.Div($2,$3)}
| TK_MOD polish2 polish2  { Term.Mod($2,$3)}

polish2:
| TK_LBRACE polish TK_RBRACE { $2 }
| polish    { $1 }
| simple    { $1 }

infix:
| infix2 TK_ADD infix2  { Term.Add($1,$3)}
| infix2 TK_MUL infix2  { Term.Mul($1,$3)}
| infix2 TK_SUB infix2  { Term.Sub($1,$3)}
| infix2 TK_DIV infix2  { Term.Div($1,$3)}
| infix2 TK_MOD infix2  { Term.Mod($1,$3)}

infix2:
| TK_LBRACE infix TK_RBRACE { $2 }
| infix     { $1 }
| simple    { $1 }