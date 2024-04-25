
%{
%}

%start <Program.program> main
%start <Program.program> main_verbose

%on_error_reduce term program instruction

%%

main:
| p = program TK_EOF { p }

main_verbose:
| p = program TK_EOF { Format.printf "The verbose version is used@,"; p}

program [@backtrack true]: 
| p = program i = instruction SEMICOLON  { p@[i] }
| i = instruction SEMICOLON { [i] }
| p = program TK_LACC p2 = program TK_RACC { p@p2 }

instruction:
| name = TK_STR ASSIGN t = term { Program.Assign(name,t) }
| t = term                      { Program.Eval(t) }

