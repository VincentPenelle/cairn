{
    open Parser
    exception Error of string
}

let digit = ['0'-'9']
let integer = digit+
let reciprocal = "^-1"
let comment = "/*" ("/*" ([^ '*' '/'] | ('*'[^ '/']) | ([^ '*'] '/'))* "*/" |"/*/") "*/"

rule token = parse
    | "//" [^ '\n']* '\n'   {token lexbuf}
    | comment               {token lexbuf}
    | [' ' '\t' '\r']       {token lexbuf}
    | '\n'                  { Lexing.new_line lexbuf ; token lexbuf }
    | "+"                   { ADD }
    | "-"                   { SUB }
    | reciprocal            { REC }
    | "*"                   { MUL }
    | "/"                   { DIV }
    | "("                   { LPAR }
    | ")"                   { RPAR }
    | integer as i          { INT(try int_of_string i with Failure _ -> raise (Error(i)) )}
    | eof                   { EOF }
    | _ as s                { raise (Error(String.make 1 s)) }
