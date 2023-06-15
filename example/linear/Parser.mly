
%{
%}

%start <Program.program> main

%on_error_reduce term program instruction

%%

main:
| p = program TK_EOF { p }

program [@backtrack true]: 
| p = program i = instruction SEMICOLON  { p@[i] }
| i = instruction SEMICOLON { [i] }
| p = program TK_LACC p2 = program TK_RACC { p@p2 }

instruction:
| name = TK_STR ASSIGN t = term { Program.Assign(name,t) }
| t = term                      { Program.Eval(t) }

