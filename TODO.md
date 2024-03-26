## TODO (17.03)

- Type Inference
  - [x] type_term
    - [x] operators
    - [x] functions
      - [x] Restricting set patterns
    - [x] let bindings
    - [x] with-statements
    - [x] inherit-statements
    - [x] if-statements
    - [] Recursive records
  - [x] constrain
    - [] functions with pattern
  - [x] extrude
  - [x] fresh_var
  - [x] freshen_above
  - [x] coalsce_type
  - [x] polymorphic type
  - [] List subsumption
  - [] List type creation

- Parser
  - [x] Handle `inherit (lib)`
  - [x] Don't use expr as base
  - [x] Error reporting
  - [] out: String interpolation

- Writing
  - [x] Abstract
  - [x] Motivation
  - [x] Language introduction
  - [x] Language definition
  - [x] recursive types
  - [x] levels
  - [x] Ausblick
  - [x] Nice references (NixOs, Dolan etc.)
  - [x] check bool naming in typing rules
  - [] Interpolation
  - [] Lengthen introducition
  - [] Examples section
  - [] Soundness

## Zentrale Fragen

- Allow intersections in negative positions?

## Improvements

- Nice Verbose error
- Use own implementation of Span

## Restrictions

- Comment positions
