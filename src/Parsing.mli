(** 
This module main use is to create a parser that can log its execution and is able to signal several errors (with the right option) from a parser generated with menhir.

The functor Make produces this parser provided it is given the modules generated by menhir, along the .cmly file generated by menhir.

A typical instantiation of this module should look like :
{[module ParserSign :
  StepParsing.Parsing.parser_decorated with type value_parsed = Program.program =
struct
  type value_parsed = Program.program

  let parser_name = "Parser"
  let parser_path = "parser"
  let error_strategy = StepParsing.Parsing.PopFirst

  module Lexer = Lexer
  module Parser = Parser
  module ParserMessages = ParserMessages
end

module P = StepParsing.Parsing.Make (ParserSign)
]}
assuming [Lexer], [Parser] and [ParserMessages] are the modules produced by menhir (with the right options). Namely, type [value_parsed] should be rendered visible for the result of the parser to be usable.

@author Vincent Penelle <vincent.penelle@u-bordeaux.fr>. 
*)

(** type for error-recovery strategy*)
type error_strategy =
  | Stop  (** The parser will stop after the first error encountered.*)
  | PopFirst
      (** After an error, the parser will pop the stack until either the top element is a terminal or non-terminal with {b backup} attribute set (in the grammar) or the stack is empty, and then ignore tokens until the first that can be shifted.*)

(** Signature to provide to the Make functor. The module Parser and Lexer should be those generated by menhir, with options {i --table --inspection --cmly}.*)
module type parser_decorated = sig
  type value_parsed
  (** The type of value that is produced by the parser. For the generated parser to be usable, it is advised to render this type visible.*)

  val parser_name : string
  (** The name of the .cmly file generated by menhir (without .cmly extension).*)

  val parser_path : string
  (** The path to the .cmly file from the root directory of the project. May end with "/" or not.
      This path should be existing from an ancestor directory that contains the executable : the program will start by searching "parser_path/parser_name.cmly" from the directory where the executable is present, and then go up in the directory tree, until it either finds the file from some folder, or reach folder "/" (where it will return an error).
      This is likely to fail on a system which is not Unix, as it relies on the Unix naming convention.
  *)

  val error_strategy : error_strategy
  (** If [Stop], the parser will stop at the first error encountered. If [PopFirst], it will instead pop the stack until a terminal or non-terminal with attributes backup set in the grammar, and then ignores all tokens until the first that can be shifted and resume parser there -- not ideal, but the only way it seems to be possible.*)

  (** Module generated with menhir. Must be generated with options {i --table --inspection --cmly} to work properly, as the inspection API is used, and the .cmly file is analysed.*)
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

  (** Lexer generated by menhir.*)
  module Lexer : sig
    val token : Lexing.lexbuf -> Parser.token
  end
end

(** Signature matching the module generated by menhir with option {i --compile-errors}. Used for error displaying. Will work if provided with a dummy [message] function instead (but less informative).*)
module type parser_messages = sig
  val message : int -> string
end

(** Signature of the modules produced by this library. Common to {!Make} and {!MakeWithDefaultMessage}.
    Contains functions to get a log of the execution of the parser, as long with a graphical explorer of this log.*)
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

(** Main functor of this module. It generates a module that can parse a text with the parser provided as an argument, and generates a log of the partial derivations produced along the run of the parser, a log of errors encountered (several errors supported if generated with PopFirst strategy), and can display a tui explorer of the sequence of partial derivations produced by the parser.*)
module Make : functor
  (Parser : parser_decorated)
  (ParserMessages : parser_messages)
  -> parser_logger with module Parser = Parser

(** Same as Make, but provides defaults error messages of the form "Error on state x" where x is the state where the parser encountered the message. Useful if you do not want to generate messages with menhir (yet).*)
module MakeWithDefaultMessage : functor (Parser : parser_decorated) ->
  parser_logger with module Parser = Parser
