
%token TK_ADD "+" [@short +]
%token TK_MUL "*" [@short *]
%token TK_SUB "-" [@short -]
%token TK_DIV "/" [@short /]
%token TK_MOD     [@short %]
%token TK_EOF     [@short EOF]
%token TK_LBRACE  [@short "("]
%token TK_RBRACE  [@short ")"]
%token TK_LACC    [@short "{"]
%token TK_RACC    [@short "}"]
%token SEMICOLON  [@short ;]
%token ASSIGN     [@short :=]
%token <int> TK_NAT [@short nat]
%token <string> TK_STR [@short str]

%left TK_ADD TK_SUB     /* lowest precedence */
%left TK_MUL TK_DIV TK_MOD    /* medium precedence */

%%