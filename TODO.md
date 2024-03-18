## TODO (17.03)

- Type Inference
  - [] type_term
    - [] let bindings
    - [] with-statements
    - [] inherit-statements
  - [] constrain
  - [x] extrude
  - [x] fresh_var
  - [x] freshen_above
  - [x] coalsce_type
  - [] polymorphic type
  - [] builtins
  - [] Simplification
    - [] compact Type
    - [] canonicalize-type
    - [] simplify-type
    - [] coalesceCompactType

- Parser
  - [x] Handle `inherit (lib)`
  - [x] Don't use expr as base
  - [] out: String interpolation
  - [] Add type-parsing

- Writing
  - [x] Abstract
  - [x] Motivation
  - [x] Language introduction
  - [x] Language definition
  - [x] Nix language support
  - [] One-step Semantic
  - [] Ausblick

- Misc
  - Read Thiemann papers
  - Read original paper
  - Allow intersections in positive positions?
  - Add if with f: bool -> a -> a -> a
  - typeTerm is similar to algorithm W in HM
  - contra and co-variance of parameters
  - records: width and depth subtyping

## Improvements
- Nice Verbose error
- Use own implementation of Span

## Restrictions
- Comment positions
- Line comments have to end in '\n'
