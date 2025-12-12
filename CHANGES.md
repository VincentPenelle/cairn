## v2.1

- [Bug fix]: The accepting state was not correctly displayed.

## v2.0

- [Breaking change] entirely change the signature to a simpler one (remove the need to instantiate a technical module to call Make functor, by simulating a generic module signature (encapsulating it in a module defining the return value type)).
- [Breaking change] handling options on how to display log with optional arguments instead of different functions in the produced module.
- adding functions to directly parse string and file
- marginal improvements to the code.
- normalize and clarify a bit the documentation; and adds use example of Cmly_read.FromString functor for loading the grammar.
- clarify how to use parsers and lexers with non-standard names.
- adding short attribute support for non-terminal symbols.

## v1.0

- Initial public release