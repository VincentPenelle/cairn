# An example

This folder is an example of how to use Cairn.
Not all functions are used, but all major features are.
The main program is visualiser.ml, and can be executed with `dune exec -- visualiser` (a help message will be displayed).
Two grammars are provided: a small expression grammar, without any particularity, that is used with no error recovery mechanism; and a small linear programming langage (only sequences of affectation), in which the grammar is cut in several pieces, the naïve error recovery mechanism is used and attributes are used (both on tokens and non-terminals, for displaying and the error mechanism).

Both functors are used (with default messages for Eee, and with custom ones for Linear).

The example uses Arg library, which is of course completely facultative, but is used to get all functionalities in a single file.
