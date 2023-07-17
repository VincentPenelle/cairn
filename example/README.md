# An example

This folder is an example of how to use Cairn.
Not all functions are used, but all major features are.
The main program is visualiser.ml, and can be executed with `dune build; _build/default/examples/visualiser.exe` (a help message will be displayed).
Two grammars are provided: a small expression grammar, without any particularity, that is used with no error recovery mechanism; and a small linear programming langage (only sequences of affectation), in which the grammar is cut in several pieces, the naïve error recovery mechanism is used and attributes are used (both on tokens and non-terminals, for displaying and the error mechanism).

Both functors are used (with default messages for Eee, and with custom ones for Linear).

The example uses Arg library, which is of course completely facultative, but is used to get all functionalities in a single file.

It also uses Ocaml-crunch to bundle the cmly file with the executable in order to make it independent from the cmly file produced (otherwise, it may crash if it is moved or executed from elsewhere). It is facultative as well, but it is advised to treat carefully how the cmly file is handled.
