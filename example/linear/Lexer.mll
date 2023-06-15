

{
    open Tokens
    exception Error of string
}

let digit = ['0'-'9']
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '\'' '_']

rule token = parse
    | [' ' '\t' '\r']   {token lexbuf}
    | '\n'              { Lexing.new_line lexbuf ; token lexbuf }
    | ":="              { ASSIGN }
    | ";"               { SEMICOLON }
    | "+"               { TK_ADD }
    | "-"               { TK_SUB }
    | "*"               { TK_MUL }
    | "/"               { TK_DIV }
    | "%"               { TK_MOD }
    | "("               { TK_LBRACE }
    | ")"               { TK_RBRACE }
    | "{"               { TK_LACC }
    | "}"               { TK_RACC }

    | (digit)+ as s     { TK_NAT(try int_of_string s with Failure _ -> raise (Error(s)) )}
    | eof               { TK_EOF }
    | (alphanum)+ as s  { TK_STR(s) }
    | _ as s            { raise (Error(String.make 1 s)) }