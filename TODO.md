## TODO (17.03)

- Type Inference
  - All

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
- Errors while parsing
- Line comments have to end in '\n'
