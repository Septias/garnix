

./26-10-29.typ
13 Tage
./26-10-31.typ


== Fragen
- How close do I want to use the given semantic?
  - The semantic is cbv, we can just note that it should be lazy.
- How should we integrate MacroNix
  - Use Macronix only for examples
  - Both conditionals and lists need union basically


== Misc
- I want a nice out-look section
- Every stuck verdict is typed by T-★-intro


== Todo
- [x] Replace `scoped rows` by `scoped labels`
- Note our "instance-closed" version of let
- Understand the both incompletenesses fully


== Claude Prompts
- [x] Go over the record literature in bib/ and sort every system into categories for why we can not use them. First are systems that do not have asymmetric concatenation with three subgroups: Only symmetric concat, negative information, … ?


== MacroNix
MacroNix (@app-macronix) adds booleans, conditionals, lists and a set of builtins, and patterns, `inherit` and `or` desugar into it (@fig-nix-parity). Of 21 programs from the Nix test suite, translated by hand, 17 type in agreement with their evaluation. The four exceptions are a conditional whose branches have incompatible types, a list whose elements have different types, `or` applied to a non-record, and a function with pattern defaults that is used at two record shapes but not generalized by A-let (@incompleteness-inference)
