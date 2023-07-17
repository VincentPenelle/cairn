module CommandLine = struct
  type parser = Eee | Linear

  let parser = ref Eee
  let string_of_task = function Eee -> "Eee" | Linear -> "Linear"

  let task_of_string = function
    | "Eee" -> Eee
    | "Linear" -> Linear
    | _ -> failwith "wrong task string"

  let parse_from_file = ref false
  let log_file = ref ""
  let error_file = ref ""
  let interactive = ref true

  let arg_spec_list =
    [
      ( "-parser",
        Arg.Symbol ([ "Eee"; "Linear" ], fun s -> parser := task_of_string s),
        " determines which parser is used for parsing the input."
        ^ " (default: " ^ string_of_task !parser ^ ")" );
      ( "-from-file",
        Arg.Set parse_from_file,
        " if present, the argument is a filename, otherwise it is the string \
         to parse." );
      ( "-log-file",
        Arg.Set_string log_file,
        "if present, the argument is the name of the file to which the log of \
         the parser will be saved." );
      ( "-error-file",
        Arg.Set_string error_file,
        "if present, the argument is the name of the file to which the error \
         log of the parser will be saved -- otherwise it is printed to the \
         error output." );
      ( "-no-interactive",
        Arg.Clear interactive,
        "if present, the interactive terminal will not be launched" );
    ]

  let usage_msg =
    "Usage: " ^ Sys.argv.(0)
    ^ " [argument]\n\
       Parses the argument with the selected parser and displays the execution \
       of the parser step by step.\n\
       [argument] is either the string to parse or the filename to be analysed \
       (if [-from-file] is present)\n"

  let parse () =
    let res = ref None in
    Arg.parse (Arg.align arg_spec_list)
      (fun a ->
        match !res with
        | None -> res := Some a
        | Some _ -> raise (Arg.Bad "Got too many inputs"))
      usage_msg;
    match !res with
    | None ->
        Arg.usage arg_spec_list usage_msg;
        exit 0
    | Some s -> s
end

module Grammar_eee = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Eee.Cmly.read "Parser.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)
let str_opt_of_str = function "" -> None | s -> Some s
let str = CommandLine.parse ()

module Eee_Sign : Cairn.Parsing.parser_decorated with type value_parsed = unit =
struct
  type value_parsed = unit

  let error_strategy = Cairn.Parsing.Stop

  module Lexer = Eee.Lexer
  module Parser = Eee.Parser
end

module Eee_parser = Cairn.Parsing.MakeWithDefaultMessage (Eee_Sign) (Grammar_eee)

module Linear_Sign :
  Cairn.Parsing.parser_decorated with type value_parsed = Linear.Program.program =
struct
  type value_parsed = Linear.Program.program

  let error_strategy = Cairn.Parsing.PopFirst

  module Lexer = Linear.Lexer
  module Parser = Linear.Parser
end


module Grammar_linear = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Linear.Cmly.read "Parser.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)


module Linear_parser = Cairn.Parsing.Make (Linear_Sign) (Linear.ParserMessages) (Grammar_linear)

let _ =
  let text, lexbuf =
    if !CommandLine.parse_from_file then MenhirLib.LexerUtil.read str
    else (str, Lexing.from_string str)
  in

  match !CommandLine.parser with
  | Eee ->
      let _ =
        Eee_parser.parse_interactive_or_log text lexbuf !CommandLine.interactive
          (str_opt_of_str !CommandLine.log_file)
          (str_opt_of_str !CommandLine.error_file)
      in
      ()
  | Linear -> (
      match
        Linear_parser.parse_interactive_or_log text lexbuf
          !CommandLine.interactive
          (str_opt_of_str !CommandLine.log_file)
          (str_opt_of_str !CommandLine.error_file)
      with
      | None -> Format.printf "No program parsed\n"
      | Some p ->
          Format.printf "@[<v 0>Program parsed:@,%s@,@]"
            (Linear.Program.string_of_program p))
