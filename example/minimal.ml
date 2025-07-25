module Grammar_eee = MenhirSdk.Cmly_read.Read (struct
  let filename = "_build/default/example/eee/Parser.cmly"
end)
(*Warning: will only work if executable launched from cairn folder : change it if you wish to use elsewhere, or use the trick used in visualiser.ml*)

module Eee_parser =
  Cairn.Parsing.MakeWithDefaultMessage
    (struct
      type value_parsed = unit
    end)
    (Eee.Parser)
    (Eee.Lexer)
    (Grammar_eee)

let () =
  if Array.length Sys.argv < 2 then
    Format.printf
      "Please provide a text to parse (representing an arithmetic expression)\n"
  else ignore (Eee_parser.parse_string Sys.argv.(1))
