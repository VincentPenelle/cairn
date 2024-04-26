# Two examples

This folder contains two examples of how to use Cairn.
Not all functions are used, but all major features are.

The file minimal.ml contains a very minimal instanciation of Cairn, with the Parser defined in folder eee. It can be executed with `dune build; _build/default/examples/minimal.exe <string_to_parse>`

The main program is visualiser.ml, and can be executed with `dune build; _build/default/examples/visualiser.exe` (a help message will be displayed).

Two grammars are provided: a small expression grammar (in folder eee), without any particularity, that is used with no error recovery mechanism; and a small linear programming langage with only sequences of affectation (in folder linear), in which the grammar is cut in several pieces, the naïve error recovery mechanism is used and attributes are used (both on tokens and non-terminals, for displaying and the error mechanism).

Both functors are used (with default messages for Eee, and with custom ones for Linear).

The example in visualiser.ml uses Arg library, which is of course completely facultative, but is used to get all functionalities in a single file.

visualiser.ml also uses Ocaml-crunch to bundle the cmly file with the executable in order to make it independent from the cmly file produced (otherwise, it may crash if it is moved or executed from elsewhere). It is facultative as well, but it is advised to treat carefully how the cmly file is handled (e.g., the minimal.exe will only work when launched from cairn main folder).
