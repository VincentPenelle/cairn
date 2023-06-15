# Cairn - A derivation explorer and logger for Menhir parser

Cairn is a small library that can be used conjointly with Menhir parser generator.
Its goal is to provide a step-by-step logger to the execution of the LR1 automaton on its input, in order to visualize the sequence of partial derivations produced (until the final derivation).
Its goal is to be a teaching (or self-teaching) tool of the behavior of LR1 parser.

It can either log its result as log files, or launch a small terminal explorer of the derivation (in utf8), made with lambda-term.

It is not made to be executed or very long inputs, as the trees would be to huge to be properly displayed (and understood), and the tool storing every step of the execution of the parser, a long execution would be heavy in memory. It has been tested on inputs of around several hundreds of tokens without memory issues.

## Install

### From source - globally

- `dune build`
- `dune install`

This will install it in your current opam installation. You might then use `cairn` library in your projects.

### From source - locally

Simply copy `src` directory to your project and add library `cairn` where you want to use it.

## Usage

To use Cairn, you must first produce a Menhir grammar. The grammar must be compiled with options `--table --inspection --cmly` (to produce the incremental API and the cmly file that cairn needs as an input). Option `--compile-errors` may as well be used to produce the file that contains error messages to be displayed by the parser (that is also used by Cairn).

Thus, in the file where you want to use your parser, you must create a module of type `parser_decorated` (see Parsing.mli) that contains the modules produced by Menhir plus some options and the path of the cmly file.

You then create the Cairn module using functor `Parsing.Make` or `Parsing.MakeWithDefaultMessage`.
This module will provide you with parsing functions working like Menhir parsing functions, but which logs and/or displays the execution of your parser.

Cairn also allows you to get the log as an abstract object (`ParserLog.configuration list`) that you can then use with functions of `ParserLog` if you want more custom usage.

Cairn is able to use two attributes that you can add on the Menhir file:

- `short` that allows you to give a smaller or more explicit name to a terminal or non-terminal to improve readability of the trees.
- `backtrack` that is used by the naïve error mechanism recovery. When an error occurs, the parser pops the stack until a element with attributes backtrack is set, and then discards the inputs token until it can shift one when it resumes parsing.

## Known limitation and issues

- Cairn has only be made and tested on Linux. As its searching of cmly file is rudimentary, it is almost guaranteed it will fail on Windows.
- The error mechanism recovery (activated with value `PopFirst` for `error_strategy`) is very basic and might be useless as is. It might be improved in the future.
- The lookahead displays only the text parsed and not the corresponding token. That is due to a limitation in Menhir that does not allow to get the terminal corresponding to a token, and that only terminals can be converted to their names, and not tokens.

## Possible improvements

- Making Cairn Windows friendly (very low priority).
- Improving the gestion of cmly search (if you know how to do this, please explain it to me).
- Improving display (e.g. navigating stacks and state items, etc.).
- Showing LR1 objects instead of LR0 ones.
- Improving error mechanism and introducing other ones (limited by Menhir not allowing to get terminal from token).
- Others you might suggest.

## Authors

- Vincent Penelle, university of Bordeaux
