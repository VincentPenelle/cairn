# Cairn - A derivation explorer and logger for Menhir parser

Cairn is a small library that can be used conjointly with Menhir parser generator.
Its goal is to provide a step-by-step logger to the execution of the LR1 automaton on its input, in order to visualize the sequence of partial derivations produced (until the final derivation).
Its goal is to be a teaching (or self-teaching) tool of the behavior of LR1 parser.

It can either log its result as log files, or launch a small terminal explorer of the derivation (in utf8), made with lambda-term.

It is not made to be executed or very long inputs, as the trees would be too huge to be properly displayed (and understood), and the tool storing every step of the execution of the parser, a long execution would probably be heavy in memory. However, it has been tested on inputs of around several hundreds of tokens without memory issues or slowdown.

## Install

### On Opam

- `opam install cairn`

### From source - globally

- `dune build`
- `dune install`

This will install it in your current opam installation. You might then use `cairn` library in your projects.

### From source - locally

Simply copy `src` directory to your project and add library `cairn` where you want to use it.

## Usage

To use Cairn, you must first produce a Menhir grammar. The grammar must be compiled with options `--table --inspection --cmly` (to produce the incremental API and the cmly file that cairn needs as an input). Option `--compile-errors` may as well be used to produce the file that contains error messages to be displayed by the parser (that is also used by Cairn).

Thus, in the file where you want to use your parser, you must create a module of type `parser_decorated` (see Parsing.mli) that contains the modules produced by Menhir plus some options, and create a module of type `MenhirSdk.Cmly_api.GRAMMAR` obtained by reading the cmly file produced by Menhir (see MenhirSdk for more details on that).

You then create the Cairn module using functor `Parsing.Make` or `Parsing.MakeWithDefaultMessage`.
This module will provide you with parsing functions working like Menhir parsing functions, but which logs and/or displays the execution of your parser.

Cairn also allows you to get the log as an abstract object (`ParserLog.configuration list`) that you can then use with functions of `ParserLog` if you want more custom usage.

Cairn is able to use two attributes that you can add on the Menhir file:

- `short` that allows you to give a smaller or more explicit name to a terminal or non-terminal to improve readability of the trees.
- `backtrack` that is used by the naïve error mechanism recovery. When an error occurs, the parser pops the stack until a element with attributes backtrack is set, and then discards the inputs token until it can shift one when it resumes parsing.

Warning: By default, Cairn only accepts grammars whose starting symbol is named "main" (because the signature of the module accepted by the functor must be fixed). However, it is possible to use a grammar whose starting symbol is named otherwise, in which case you need to add `Incremental.main` to your `Parser` module (which should be the function in `Incremental` you want to use as entry point).

The lexing function is also expected to be named "token", and another lexing function can be used with simply providing it under the name "token".

### Detailed usage example

A typical instantiation of the `Make` functor should look like :

```OCaml
module Grammar = MenhirSdk.Cmly_read.Read (struct let filename = "Parser.cmly" end)

module P = Cairn.Parsing.Make (struct type value_parsed = Program.program) (Parser) (Lexer) (ParserMessages) (Grammar)
```

assuming `Lexer`, `Parser` and `ParserMessages` are the modules produced by menhir (with the right options), and that "Parser.cmly" is the name (with path) to the cmly file produced by menhir. It is assumed that the parsing function of [Parser.Incremental] is [main], and the lexing function of lexer is [token]. If it isn't the case, you need to tweak the corresponding modules to make it so.

For the cmly file, it might not straightforward to use its direct name (especially if the executable is destined to be installed or executed from somewhere else than its own directory).
  
In that case, it might be worth to bundle it in the executable with, for
example, ocaml-crunch (see examples provided to see how). It is then needed
to use the [FromString] functor of {!MenhirSdk.Cmly_read} rather than the
[Read] one as follows:

```OCaml
module Grammar = MenhirSdk.Cmly_read.FromString (struct
  let content = Option.get (<Module_generated_by_ocaml_crunch>.read "<name_of_cmly_file>")
end)
```

If you are running a version of Menhir anterior to 2023/12/31, then the
FromString functor is not expose, and you have instead to reproduce its
behavior with the [Lift] functor as follows:

```OCaml
module Grammar = MenhirSdk.Cmly_read.Lift (struct
  let file_content = Option.get (<Module_generated_by_ocaml_crunch>.read "<name_of_cmly_file>")
  let prefix = "CMLY" ^ MenhirSdk.Version.version
  let grammar = Marshal.from_string file_content (String.length prefix)
end)
```

This is adapted from the [Read] functor of {!MenhirSdk.Cmly_read}.

If you want to use a starting symbol that is not named `main`, you need to modify the `Parser.Incremental` module you use to invoke the `Make` functor. In case that starting symbol is named `foo`, you can achieve that by defining the following module

```Ocaml
module ParserFoo = struct
  include Parser
  module Incremental = struct
    let main = Incremental.foo
  end
end
```

If your Lexing function is not name `token`, but, e.g. `bar`, you need to pass the following module instead of `Lexer` :

```Ocaml
module LexerBar = struct
  let token = Lexer.bar
end
```

## Known limitation and issues

- Cairn has only be made and tested on Linux. Using it on other systems might fail.
- The error mechanism recovery (activated with value `PopFirst` for `error_strategy`) is very basic and might be useless as is. It might be improved in the future.
- The lookahead displays only the text parsed and not the corresponding token. That is due to a limitation in Menhir that does not allow to get the terminal corresponding to a token, and that only terminals can be converted to their names, and not tokens.
- The cmly handling is quite tricky as it must be generated by menhir, then used in the program at execution time. A way to properly deal with this is shown in the examples (with ocaml-crunch), but other solutions might exists. To my knowledge, it is not possible to generate the Grammar module directly with menhir invocation, which would be easier.

## Possible improvements

- Improving display (e.g. navigating stacks and state items, etc.).
- Showing LR1 objects instead of LR0 ones.
- Improving error mechanism and introducing other ones (limited by Menhir not allowing to get terminal from token).
- Others you might suggest.

## Authors

- Vincent Penelle, university of Bordeaux
