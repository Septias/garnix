## TODO (17.03)

- Type Inference

  - [] type_term
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
  - [] builtins
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
  - [] Nix language support
  - [] Builtins
  - [] One-step Semantic
  - [] Ausblick

## Zentrale Fragen

- Soll ich überall die Syntax schön definieren?
- Schaffe ich es den Soundness Beweis zu machen?
- Allow intersections in negative positions?
- **Kann ich nachher module parsen?**
- **Wie funktioniert import?**
- **Lazy inferenz**

## TODO

- Read Thiemann papers
- typeTerm is similar to algorithm W in HM

## Improvements

- Nice Verbose error
- Use own implementation of Span

## Restrictions

- Comment positions
- Line comments have to end in '\n'
