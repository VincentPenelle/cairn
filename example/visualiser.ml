module CommandLine = struct
  type parser = Eee | Linear

  let parser = ref Eee
  let string_of_task = function Eee -> "Eee" | Linear -> "Linear"

  let task_of_string = function
    | "Eee" -> Eee
    | "Linear" -> Linear
    | _ -> failwith "wrong task string"

  let parse_from_file = ref false
  let log_file = ref None
  let error_file = ref None
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
        Arg.String (fun s -> log_file := Some s),
        "if present, the argument is the name of the file to which the log of \
         the parser will be saved." );
      ( "-error-file",
        Arg.String (fun s -> error_file := Some s),
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

(* for menhir >= 2023/12/31 *)
(* module Grammar_eee = MenhirSdk.Cmly_read.FromString (struct
  let content = Option.get (Eee.Cmly.read "Parser.cmly")
end) *)

let str = CommandLine.parse ()

module Eee_parser =
  Cairn.Parsing.MakeWithDefaultMessage
    (struct
      type value_parsed = unit
    end)
    (Eee.Parser)
    (Eee.Lexer)
    (Grammar_eee)

module Grammar_linear = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (Linear.Cmly.read "Parser.cmly")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)

(* for menhir >= 2023/12/31 *)
(* module Grammar_eee = MenhirSdk.Cmly_read.FromString (struct
  let content = Option.get (Linear.Cmly.read "Parser.cmly")
end) *)

module Linear_parser =
  Cairn.Parsing.Make
    (struct
      type value_parsed = Linear.Program.program
    end)
    (Linear.Parser)
    (struct
      let token = Linear.Lexer.word
    end)
    (Linear.ParserMessages)
    (Grammar_linear)

let _ =
  let text, lexbuf =
    if !CommandLine.parse_from_file then MenhirLib.LexerUtil.read str
    else (str, Lexing.from_string str)
  in

  match !CommandLine.parser with
  | Eee ->
      let _ =
        Eee_parser.parse_interactive ~interactive:!CommandLine.interactive
          ?log_file:!CommandLine.log_file ?error_file:!CommandLine.error_file
          text lexbuf
      in
      ()
  | Linear -> (
      match
        Linear_parser.parse_interactive ~strategy:PopFirst
          ~interactive:!CommandLine.interactive ?log_file:!CommandLine.log_file
          ?error_file:!CommandLine.error_file text lexbuf
      with
      | None -> Format.printf "No program parsed\n"
      | Some p ->
          Format.printf "@[<v 0>Program parsed:@,%s@,@]"
            (Linear.Program.string_of_program p))
