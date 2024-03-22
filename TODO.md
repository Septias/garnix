## TODO (17.03)

- Type Inference (1d)
  - [] type_term
    - [] functions
      - [] Restricting set patterns
    - [] let bindings
    - [] with-statements
    - [] inherit-statements
  - [x] constrain
  - [x] extrude
  - [x] fresh_var
  - [x] freshen_above
  - [x] coalsce_type
  - [x] polymorphic type
  - [] builtins (2h)
  - [] Simplification (1d)
    - [] compact Type
    - [] canonicalize-type
    - [] simplify-type
    - [] coalesceCompactType

- Parser

  - [x] Handle `inherit (lib)`
  - [x] Don't use expr as base
  - [] Error reporting
  - [] **String interpolation**
  - [] Add type-parsing

- Writing

  - [x] Abstract
  - [x] Motivation
  - [x] Language introduction
  - [x] Language definition
  - [x] recursive types
  - [x] levels
  - [x] Ausblick
  - [] Proof
  - [] Add novelty of only constraining variables
  - [] Reference Kiselyov [2013]


- Lsp
  - [] Error reporting

## Zentrale Fragen

- Soll ich überall die Syntax schön definieren?
- Schaffe ich es den Soundness Beweis zu machen?
- Allow intersections in negative positions?
- **Kann ich nachher module parsen?** <- Nein
- **Wie funktioniert import?** <- Nein
- **Lazy inferenz** <- Nein
- **Wie Typing Regeln für Typvariablen**
- **Was sind fixpoints?**
- **What does nix use?**

## Improvements

- Use own implementation of Span

## Restrictions

- Comment positions
- Line comments have to end in '\n'

## Misc

```nix
# An overlay to auto-call packages in ../by-name.
# By defining it at the top of the file,
# this value gets reused even if this file is imported multiple times,
# thanks to Nix's import-value cache.
autoCalledPackages = import ./by-name-overlay.nix ../by-name;
```
