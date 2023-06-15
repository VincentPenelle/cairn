type error_strategy = Stop | PopFirst

module type parser_decorated = sig
  type value_parsed

  val parser_name : string
  val parser_path : string
  val error_strategy : error_strategy

  module Parser : sig
    type token

    exception Error

    val main : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> value_parsed

    module MenhirInterpreter : sig
      include
        MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token

      type 'a terminal
      type _ nonterminal

      include
        MenhirLib.IncrementalEngine.INSPECTION
          with type 'a lr1state := 'a lr1state
          with type production := production
          with type 'a terminal := 'a terminal
          with type 'a nonterminal := 'a nonterminal
          with type 'a env := 'a env
    end

    module Incremental : sig
      val main : Lexing.position -> value_parsed MenhirInterpreter.checkpoint
    end
  end

  module Lexer : sig
    val token : Lexing.lexbuf -> Parser.token
  end
end

module type parser_messages = sig
  val message : int -> string
end

module type parser_logger = sig
  module Parser : parser_decorated

  val state_to_lr0_list : int -> string list
  (** Function that associates to a lr1 state number a list of string representing the lr0 items it contains.*)

  val parse :
    string ->
    Lexing.lexbuf ->
    Parser.value_parsed option
    * ParserLog.configuration list
    * (string * string * string) list
  (** [parse text lexbuf] parses an input pointed by [lexbuf] whose content is [text]. [text] and [lexbuf] might be obtained with MenhirLib.LexerUtil. It returns [(value,log,errors)], where: 
    {ul {- [value] is either [Some value] if the parser produced a semantical value or [None].}
      {- [log] is a configuration list that represents the execution of the parser (to be used with functions from [ParserLog] alongside [state_to_lr0_list]).}
      {- [errors] is a list of error messages encountered along the execution, in order to which they appeared. If this list is not empty, [value] should probably not be trusted.
      The first string is the position of the error, the second the two tokens between which the error occured, and the last an explanation (from the [ParserMessages] provided).}}*)

  val parse_interactive : string -> Lexing.lexbuf -> Parser.value_parsed option
  (** [parse_interactive text lexbuf] parses an input pointed by [lexbuf], whose content is [text]. Displays a terminal user interface allowing to navigate the log of the parser. Then returns the parsed value (if no error was encountered, [None] otherwise).*)

  val parse_log :
    string -> Lexing.lexbuf -> string -> string -> Parser.value_parsed option
  (** [parse_log text lexbuf log_file error_file] parses an input pointed by [lexbuf], whose content is [text]. Writes a log of the parser execution in the file of name [log_file] and an error log in the file of name [error_file]. Returns the parsed value (if no error was encountered, [None] otherwise)*)

  val parse_interactive_or_log :
    string ->
    Lexing.lexbuf ->
    bool ->
    string option ->
    string option ->
    Parser.value_parsed option
  (** [parse_log text lexbuf interactive log_file error_file] parses an input pointed by [lexbuf], whose content is [text]. Displays a terminal user interface allowing to navigate the log of the parser if [interactive is true]. Writes a log of the parser execution in the file of name [log_file] if it is not [None] and an error log in the file of name [error_file] if it is not [None]. Returns the parsed value (if no error was encountered, [None] otherwise)*)
end

module Make (Parser : parser_decorated) (ParserMessages : parser_messages) =
struct
  module Parser = Parser
  open Parser
  module MI = Parser.MenhirInterpreter
  module StateMap = Map.Make (Int)

  let add_slash_if_needed str =
    if str.[String.length str - 1] = '/' then str else str ^ "/"

  module G = MenhirSdk.Cmly_read.Read (struct
    let remove_first_char s = String.sub s 1 (String.length s - 1)

    let go_up_folder name =
      remove_first_char
        (List.fold_left
           (fun a s -> a ^ "/" ^ s)
           ""
           (List.rev (List.tl (List.rev (String.split_on_char '/' name)))))

    let folder_name = go_up_folder Sys.executable_name
    let raw_filename = add_slash_if_needed parser_path ^ parser_name ^ ".cmly"

    let rec find_filename name =
      if name = "/" then failwith "Error : grammar not found"
      else if Sys.file_exists (name ^ "/" ^ raw_filename) then
        name ^ "/" ^ raw_filename
      else find_filename (go_up_folder name)

    let filename = find_filename folder_name
  end)

  let show text positions =
    MenhirLib.ErrorReports.extract text positions
    |> MenhirLib.ErrorReports.sanitize |> MenhirLib.ErrorReports.compress
    |> MenhirLib.ErrorReports.shorten 20

  let find_short_attribute terminal =
    List.fold_left
      (fun acc attr ->
        if acc <> None then acc
        else if G.Attribute.has_label "short" attr then
          Some (G.Attribute.payload attr)
        else None)
      None
      (G.Terminal.attributes terminal)

  let string_of_gsymbol = function
    | G.N a -> G.Nonterminal.name a
    | G.T a -> (
        match find_short_attribute a with
        | Some str -> str
        | None -> G.Terminal.name a)

  let string_lists_of_gprod g_prod =
    let lhs = G.Nonterminal.name (G.Production.lhs g_prod) in
    let rhs =
      Array.map (fun (a, _, _) -> string_of_gsymbol a) (G.Production.rhs g_prod)
    in
    (lhs, rhs)

  let string_of_gproduction g_prod =
    let lhs, rhs = string_lists_of_gprod g_prod in
    let rhs = Array.fold_left (fun acc s -> acc ^ " " ^ s) "" rhs in
    lhs ^ " ->" ^ rhs

  let string_of_production production =
    string_of_gproduction (G.Production.of_int (MI.production_index production))

  let string_of_gitem (prod, pos) =
    let lhs, rhs = string_lists_of_gprod prod in
    lhs ^ " ->"
    ^ Array.fold_left ( ^ ) ""
        (Array.mapi (fun i symb -> (if i = pos then " _ " else " ") ^ symb) rhs)
    ^ if pos = Array.length rhs then " _" else ""

  let get_env_text text env i =
    match Parser.MenhirInterpreter.get i env with
    | Some (Parser.MenhirInterpreter.Element (_, _, pos1, pos2)) ->
        show text (pos1, pos2)
    | None -> "???"

  let get_env checkpoint =
    match checkpoint with
    | MI.InputNeeded env
    | MI.Shifting (env, _, _)
    | MI.AboutToReduce (env, _)
    | MI.HandlingError env ->
        env
    | Rejected -> failwith "reject"
    | Accepted _ -> failwith "accepted"

  (* Function that parses the input step by step, and logs every step while doing so. Has also a mechanism to automatically drop a part of the stack when encountering errors (if error_strategy is PopFirst).
     It is for all cases but Error a simple mimic of the behaviour of menhir while logging it into derivations. For Error, if error_strategy is PopFirst, it does modify explicitely the stack taking into account the annotations of the grammar.
        - checkpoint is the current state of the menhir parser.
        - supplier is the supplier needed by menhir to read the next token
        - text is the text being parsed as a string (for getting the text corresponding to each token)
        - buffer is the buffer linked to supplier (for error reporting with menhir)
        - derivations is the list of all parser steps (list of ParserLog.configuration) performed until now (in reverse). Accumulator constructing the result to give back ultimately.
        - errors is the list of errors encountered until now. Used both for giving back all encountered errors (at most one if error_strategy is Stop), and to decide if the run was successfull (if not empty and error_strategy is PopFirst, some spurious result might be computed)*)
  let rec stepParsingDerivations checkpoint supplier text buffer derivations
      errors =
    match checkpoint with
    | MI.Rejected ->
        ( None,
          List.rev
            (ParserLog.apply_action (List.hd derivations) ParserLog.Reject
            :: derivations),
          errors )
    | MI.Accepted value ->
        ( Some value,
          List.rev
            (ParserLog.apply_action (List.hd derivations) ParserLog.Accept
            :: derivations),
          errors )
    | MI.InputNeeded _ ->
        let t, n1, n2 = supplier () in
        stepParsingDerivations
          (MI.offer checkpoint (t, n1, n2))
          supplier text buffer
          (ParserLog.apply_action (List.hd derivations)
             (ParserLog.Input (show text (n1, n2)))
          :: derivations)
          errors
    | MI.Shifting (_, env2, notEnd) ->
        let t, n1, n2 = supplier () in
        let tk =
          try
            string_of_gsymbol
              (match
                 G.Lr0.incoming
                   (G.Lr1.lr0 (G.Lr1.of_int (MI.current_state_number env2)))
               with
              | Some s -> s
              | None -> failwith "")
          with _ -> ""
        in
        stepParsingDerivations
          (MI.offer (MI.input_needed env2) (t, n1, n2))
          supplier text buffer
          (ParserLog.apply_action (List.hd derivations)
             (ParserLog.ShiftRead
                ( MI.current_state_number env2,
                  tk,
                  if notEnd then
                    let s = show text (n1, n2) in
                    if s = "" then "EOF" else s
                  else "END OF FILE" ))
          :: derivations)
          errors
    | MI.AboutToReduce (_, prod) ->
        let new_checkpoint = MI.resume checkpoint in
        let prod_str = string_of_production prod in
        let lhs_str = List.hd (String.split_on_char ' ' prod_str) in
        stepParsingDerivations new_checkpoint supplier text buffer
          (ParserLog.apply_action (List.hd derivations)
             (ParserLog.Reduce
                ( lhs_str,
                  prod_str,
                  (try MI.current_state_number (get_env new_checkpoint)
                   with _ -> G.Lr0.count - 1),
                  List.length (MI.rhs prod) ))
          :: derivations)
          errors
    | MI.HandlingError env -> (
        let location =
          List.hd
            (String.split_on_char '\n'
               (MenhirLib.LexerUtil.range (MI.positions env)))
        in
        let indication = MenhirLib.ErrorReports.show (show text) buffer in
        let message =
          try
            List.hd
              (String.split_on_char '\n'
                 (ParserMessages.message (MI.current_state_number env)))
          with _ -> "Not Found"
        in
        let message =
          MenhirLib.ErrorReports.expand (get_env_text text env) message
        in
        (*Format.eprintf "\027[38;5;1m%s%s@,%s\027[0m@," location indication
          message;*)
        match error_strategy with
        | Stop ->
            ( None,
              List.rev
                (ParserLog.apply_action (List.hd derivations)
                   (ParserLog.Error (location, indication, message))
                :: derivations),
              [ (location, indication, message) ] )
        | PopFirst ->
            let deriv = List.hd derivations in
            let new_deriv, new_env =
              let rec pop_st deriv env =
                match Parser.MenhirInterpreter.top env with
                | None -> (deriv, env)
                | Some (Element (s, _, _, _)) -> (
                    let gsymb =
                      G.Lr0.incoming (G.Lr1.lr0 (G.Lr1.of_int (MI.number s)))
                    in
                    if gsymb = None then (deriv, env)
                    else
                      let gsymb = Option.get gsymb in
                      if
                        List.fold_left
                          (fun acc attr ->
                            acc || G.Attribute.has_label "backtrack" attr)
                          false
                          (match gsymb with
                          | G.N n -> G.Nonterminal.attributes n
                          | G.T t -> G.Terminal.attributes t)
                      then (deriv, env)
                      else
                        match MI.pop env with
                        | Some b -> pop_st (ParserLog.pop_configuration deriv) b
                        | None -> (deriv, env))
              in
              pop_st deriv env
            in
            let checkpoint = MI.input_needed new_env in
            let next_lookahead = ref (supplier ()) in
            let cond = ref true in
            let get_token (tk, _, _) = tk
            and get_fst_pos (_, p, _) = p
            and get_snd_pos (_, _, p) = p in
            while
              !cond
              && not
                   (MI.acceptable checkpoint
                      (get_token !next_lookahead)
                      (get_fst_pos !next_lookahead))
            do
              next_lookahead := supplier ();
              if get_fst_pos !next_lookahead = get_snd_pos !next_lookahead then
                cond := false
            done;
            if !cond then
              let new_checkpoint = MI.offer checkpoint !next_lookahead in
              let n1, n2 =
                (get_fst_pos !next_lookahead, get_snd_pos !next_lookahead)
              in
              stepParsingDerivations new_checkpoint supplier text buffer
                (ParserLog.apply_action new_deriv
                   (ParserLog.Input (show text (n1, n2)))
                :: ParserLog.apply_action (List.hd derivations)
                     (ParserLog.Error (location, indication, message))
                :: derivations)
                ((location, indication, message) :: errors)
            else
              ( None,
                List.rev
                  (ParserLog.apply_action (List.hd derivations)
                     (ParserLog.Error (location, indication, message))
                  :: derivations),
                (location, indication, message) :: errors ))

  let state_to_lr0_list num =
    let lr0 = G.Lr1.lr0 (G.Lr1.of_int num) in
    let item_list = G.Lr0.items lr0 in
    List.map string_of_gitem item_list

  let parse text lexbuf =
    let supplier = MI.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
    let checkpoint = Parser.Incremental.main lexbuf.lex_curr_p in
    stepParsingDerivations checkpoint supplier text buffer
      [ ParserLog.initial_configuration ]
      []

  (*
  let parse_file file_to_parse =
    let text, lexbuf = MenhirLib.LexerUtil.read file_to_parse in
    parse text lexbuf
*)

  let interactive_or_log interactive log_file error_file value derivations
      errors =
    (match log_file with
    | Some name ->
        let channel = open_out name in
        let ch = Format.formatter_of_out_channel channel in
        Format.fprintf ch "@[<v 0> %a @]@," ParserLog.print_configuration_list
          (List.tl derivations);
        close_out channel
    | None -> ());
    (if errors != [] then
       let error_log =
         Format.sprintf "@[<v 0>"
         ^ List.fold_left
             (fun acc (location, indication, message) ->
               acc
               ^ Format.sprintf "@[<v 2>%s %s@,%s@]@," location indication
                   message)
             "" errors
         ^ Format.sprintf "@]@,"
       in
       match error_file with
       | None -> Format.eprintf "\027[38;5;9m%s\027[0m" error_log
       | Some name ->
           let channel = open_out name in
           let ch = Format.formatter_of_out_channel channel in
           Format.fprintf ch "%s" error_log;
           close_out channel);
    if interactive then
      ParserLog.derivations_explorer derivations state_to_lr0_list;
    value

  let parse_interactive text lexbuf =
      let value, derivations, errors = parse text lexbuf in
  interactive_or_log true None None value derivations errors

  let parse_log text lexbuf log_file error_file =
    let value, derivations, errors = parse text lexbuf in
    interactive_or_log false (Some log_file) (Some error_file) value derivations
      errors

  let parse_interactive_or_log text lexbuf interactive log_file error_file =
    let value, derivations, errors = parse text lexbuf in
    interactive_or_log interactive log_file error_file value derivations errors

  (* let parse_file_interact file_to_parse interactive log_file error_file =
       let value, derivations, errors = parse_file file_to_parse in
       interactive_or_log interactive log_file error_file value derivations errors

     let parse_string str =
       let lexbuf = Lexing.from_string str in
       parse str lexbuf

     let parse_string_interact str interactive log_file error_file =
       let value, derivations, errors = parse_string str in
       interactive_or_log interactive log_file error_file value derivations errors
  *)

  (*Bon, ça parse bien depuis stdin MAIS le texte ne va évidemment pas (ça doit pouvoir se contourner par d’autres fonctions cela dit) et surtout ce n’est pas exécuté à chaque ligne, ce qui serait mieux…*)
  (*let parse_stdin () =
    let lexbuf = Lexing.from_channel stdin in
    let supplier = MI.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
    let checkpoint = Parser.Incremental.main lexbuf.lex_curr_p in
    Format.printf "@[<v 0>";
    let res, _, _ =
      stepParsingDerivations checkpoint supplier "text" buffer
        [ ParserLog.initial_configuration ]
        []
    in
    Format.printf "@]";
    res*)
end

module MakeWithDefaultMessage (Parser : parser_decorated) =
  Make
    (Parser)
    (struct
      let message x = "Error on state " ^ string_of_int x ^ "."
    end)
