## TODO (17.03)
- Type Inference
  - [] type_term
    - [] operators
    - [] functions
      - Restricting set patterns
    - [] let bindings
    - [] with-statements
    - [] inherit-statements
  - [x] constrain
  - [x] extrude
  - [x] fresh_var
  - [x] freshen_above
  - [x] coalsce_type
  - [x] polymorphic type
  - [] Type annotations
  - [] Simplification
    - [] compact Type
    - [] canonicalize-type
    - [] simplify-type
    - [] coalesceCompactType

- Parser
  - [x] Handle `inherit (lib)`
  - [x] Don't use expr as base
  - [] Error reporting
  - [] out: String interpolation
  - [] Add type-parsing

- Writing
  - [x] Abstract
  - [x] Motivation
  - [x] Language introduction
  - [x] Language definition
  - [] recursive types
  - [] levels
  - [] Builtins
  - [] Ausblick
  - [] Nice references

## Zentrale Fragen
- Soll ich überall die Syntax schön definieren?
- Schaffe ich es den Soundness Beweis zu machen?
- Allow intersections in negative positions?
- **Kann ich nachher module parsen?**
- **Wie funktioniert import?**
- **Lazy inferenz**
- **Wie Typing Regeln für Typvariablen**
- **Wie funktionieren overlays**

## TODO
- Read Thiemann papers
- typeTerm is similar to algorithm W in HM

## Improvements
- Nice Verbose error
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