type error_strategy = Stop | PopFirst

module P (T : sig
  type value_parsed
end) =
struct
  module type parser = sig
    type token

    exception Error

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
      val main : Lexing.position -> T.value_parsed MenhirInterpreter.checkpoint
    end
  end

  module type parser_logger = sig
    val state_to_lr0_list : int -> string list

    val parse_to_derivation :
      ?strategy:error_strategy ->
      string ->
      Lexing.lexbuf ->
      T.value_parsed option
      * ParserLog.configuration list
      * (string * string * string) list

    val parse :
      ?strategy:error_strategy ->
      ?interactive:bool ->
      ?log_file:string ->
      ?error_file:string ->
      string ->
      Lexing.lexbuf ->
      T.value_parsed option

    val parse_string :
      ?strategy:error_strategy ->
      ?interactive:bool ->
      ?log_file:string ->
      ?error_file:string ->
      string ->
      T.value_parsed option

    val parse_file :
      ?strategy:error_strategy ->
      ?interactive:bool ->
      ?log_file:string ->
      ?error_file:string ->
      string ->
      T.value_parsed option
  end
end

module type parser_messages = sig
  val message : int -> string
end

module Make
    (T : sig
      type value_parsed
    end)
    (Parser : P(T).parser)
    (Lexer : sig
      val token : Lexing.lexbuf -> Parser.token
    end)
    (ParserMessages : parser_messages)
    (Grammar : MenhirSdk.Cmly_api.GRAMMAR) =
struct
  module MI = Parser.MenhirInterpreter
  module StateMap = Map.Make (Int)
  module G = Grammar

  let show text positions =
    MenhirLib.ErrorReports.extract text positions
    |> MenhirLib.ErrorReports.sanitize |> MenhirLib.ErrorReports.compress
    |> MenhirLib.ErrorReports.shorten 20

  let find_attribute label attributes =
    try
      Some
        (List.hd
           (List.filter_map
              (fun attr ->
                if G.Attribute.has_label label attr then
                  Some (G.Attribute.payload attr)
                else None)
              attributes))
    with _ -> None

  let find_short_attribute attributes = find_attribute "short" attributes

  let string_of_gsymbol = function
    | G.N a -> (
        match find_short_attribute (G.Nonterminal.attributes a) with
        | Some str -> str
        | None -> G.Nonterminal.name a)
    | G.T a -> (
        match find_short_attribute (G.Terminal.attributes a) with
        | Some str -> str
        | None -> G.Terminal.name a)

  let string_arrays_of_gprod g_prod =
    let lhs = string_of_gsymbol (G.N (G.Production.lhs g_prod)) in
    let rhs =
      Array.map (fun (a, _, _) -> string_of_gsymbol a) (G.Production.rhs g_prod)
    in
    (lhs, rhs)

  let string_of_gproduction g_prod =
    let lhs, rhs = string_arrays_of_gprod g_prod in
    let rhs = Array.fold_left (fun acc s -> acc ^ " " ^ s) "" rhs in
    lhs ^ " ->" ^ rhs

  let string_of_production production =
    string_of_gproduction (G.Production.of_int (MI.production_index production))

  let string_of_gitem (prod, pos) =
    let lhs, rhs = string_arrays_of_gprod prod in
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
  let rec stepParsingDerivations strategy checkpoint supplier text buffer
      derivations errors =
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
        stepParsingDerivations strategy
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
        stepParsingDerivations strategy
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
        stepParsingDerivations strategy new_checkpoint supplier text buffer
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
        match strategy with
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
              stepParsingDerivations strategy new_checkpoint supplier text
                buffer
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

  let parse_to_derivation ?(strategy = Stop) text lexbuf =
    let supplier = MI.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    let buffer, supplier = MenhirLib.ErrorReports.wrap_supplier supplier in
    let checkpoint = Parser.Incremental.main lexbuf.lex_curr_p in
    stepParsingDerivations strategy checkpoint supplier text buffer
      [ ParserLog.initial_configuration ]
      []

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

  let parse ?strategy ?(interactive = true) ?log_file ?error_file text lexbuf =
    let value, derivations, errors =
      parse_to_derivation ?strategy text lexbuf
    in
    interactive_or_log interactive log_file error_file value derivations errors

  let parse_string ?strategy ?interactive ?log_file ?error_file string =
    let lexbuf = Lexing.from_string string in
    parse ?strategy ?interactive ?log_file ?error_file string lexbuf

  let parse_file ?strategy ?interactive ?log_file ?error_file file =
    let text, lexbuf = MenhirLib.LexerUtil.read file in
    parse ?strategy ?interactive ?log_file ?error_file text lexbuf
end

module MakeWithDefaultMessage
    (T : sig
      type value_parsed
    end)
    (Parser : P(T).parser)
    (Lexer : sig
      val token : Lexing.lexbuf -> Parser.token
    end)
    (Grammar : MenhirSdk.Cmly_api.GRAMMAR) =
  Make (T) (Parser) (Lexer)
    (struct
      let message x = "Error on state " ^ string_of_int x ^ "."
    end)
    (Grammar)
