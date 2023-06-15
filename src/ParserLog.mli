(**

This module define a type [configuration] representing the configuration of a parser, and a type [action] representing the actions that can be applied to a parser configuration, only with types independent of any menhir type. These objects are intended as logs for a parser, and helper functions are provided to modify them and display them (including a TUI).

If you are only using this module alongside Parsing, then you should not really need this module, or at the limit, the functions [print_configuration], [print_configuration_list] and [derivations_explorer] might be of use.

If you want to log a handmade parser, then you should start by creating an initial configuration with [initialConfiguration], and then, construct a list of configuration by applying to the first element the action your parser is performing (with [apply_action]). For the sake of simpler log, the Shift action has been merged with the next Input action. [pop_configuration] allow you to pop the stack (for error treatment). The rest of the functions should help you if you want another UI or log of the configurations.

@author Vincent Penelle <vincent.penelle@u-bordeaux.fr>. 
*)

(** Type representing the actions that can be applied to a LR1 parser. *)
type action =
  | ShiftRead of int * string * string
      (** [ShiftRead(state,token,str)] : Shifts the lookahead to state number [state] as token [token] (string representation) and reads [str] as new lookahead.*)
  | Input of string  (** [Input(str)] : Reads [str] as the new lookahead.*)
  | Reduce of string * string * int * int
      (** [Reduce(nonTerm,production,state,size)] : Reduces the production [production] (string representation) to state number [state]. The lefthand of the production is [nonTerm] and the size of the righthand of the production is [size]. Those informations are used to display information correctly. *)
  | Accept  (** The parser is accepting. *)
  | Reject  (** The parser is rejecting. *)
  | Error of string * string * string
      (** [Error(location,indication,message)] : The parser is in an error state. [location] is a string describing the position of the error in the parsed text, [indication] is a string telling between which characters the error was encountered, and [message] is a string describing the nature of the error. *)

val string_of_action : action -> string
(** Returns a string description of the provided action. *)

type configuration
(** Type representing the configuration of a LR1 parser, i.e., its state (number), the stack (and for each element of it, the partial derivation tree associated with it), the lookahead, and the last action applied to it. The parser itself is not known by such an element, and the representation is only composed of integer and string (no types depending of a parser).*)

val initial_configuration : configuration
(** An initial configuration : state number 0, the empty stack, no lookahead and action Reading. *)

val get_top_state : configuration -> int
(** Returns the number of the state that is the current parser configuration. *)

val pop_configuration : configuration -> configuration
(** Pops the last element of the stack if there is one (leaves the empty stack unchanged). *)

val apply_action : configuration -> action -> configuration
(** [apply_action configuration action] produces the effect of the action [action] to the configuration [configuration]. Does NOT check that the action makes sense (as it is only textual representation) and may raise an exception if trying to apply [Reduce] with a [size] bigger than the size of the stack of [configuration]. *)

val markup_of_configuration :
  bool -> bool -> configuration -> string * string * LTerm_text.item list list
(** [let str_act,str_look,repr_deriv = markup_of_configuration color utf8 configuration] produces a LTerm representation of the given parser configuration.
      {ul {- [str_act] is the string representation of the last action applied}
      {- [str_look] is the lookahead of the parser}
      {- [repr_deriv] is a LTerm_text.item list representation of the partial derivation trees on the stack of [configuration]. Each element of the list is a line of the representation.}}
      {ul {- If [color] is true, then the root of the topmost tree (i.e., the last symbol added) will be in green, otherwise no color will be present}
      {- If [utf8] is true, then the tree will use utf8 characters for better depiction, otherwise only ASCII symbol will be present}} *)

val stringlist_of_configuration :
  bool -> configuration -> string * string * string list
(** [let str_act,str_look,repr_deriv = stringlist_of_configuration utf8 configuration] produces the same representation as [markup_of_configuration] except that the list is directly composed of strings. Useful if not destined to a LTerm application (e.g., for displaying in a log).*)

val stack_of_configuration : configuration -> string list
(** [strack_of_configuration configuration] returns the list of non-terminals (their string representation) composing the stack of [configuration]. *)

val print_configuration : Format.formatter -> configuration -> unit
(** Pretty printer for a configuration. *)

val print_configuration_list : Format.formatter -> configuration list -> unit
(** Pretty printer for a list of configuration sequence (representing the execution of a parser). *)

val derivations_explorer : configuration list -> (int -> string list) -> unit
(** [derivations_explorer configurations state_repr] executes a TUI allowing to explore the list of configurations [configurations]. [state_repr] associates to each state number a list of string representing its LR0 components. *)