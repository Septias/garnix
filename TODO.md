##

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
    - [x] functions with pattern
    - [] recursive
    - [] unions
  - [x] extrude
    - [] Pattern
    - [] Union
    - [] Recursive
  - [x] fresh_var
  - [x] freshen_above
    - [] Recursive
  - [x] coalsce_type
    - [] pattern
    - [] union
    - [] recursive
    - [] cache
    - [] rec
  - [x] polymorphic type
  - [x] List subsumption
  - [x] List type creation
  - [x] fix let-binding order
  - [x] fix argument type fordwarding

- Parser
  - [x] Handle `inherit (lib)`
  - [x] Don't use expr as base
  - [x] Error reporting
  - [] out: String interpolation

- LSP
  - [] Error reporting
  - [] inlay hints

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
  - [x] Typing rules finalisieren
  - [x] More subsumption rules
  - [] Examples section

## Improvements
- Nice Verbose error
- Use own implementation of Span

## Restrictions
- Comment positions
