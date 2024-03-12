## TODO (17.03)

- Type Inference

  - [] Handle `with`
  - [] Handle `inherit`
  - [] Handle `multi-argument` functions
  - [] Handle String interpolation
  - [] Load flake inputs
  - [] Return error array

- Parser

  - [] Handle `inherit (lib)`
  - [] Add type-parsing
  - [] Don't use expr as base
  - [] out: Use case analysis nixpkgs

- LSP

  - [] Inlay type hints
    - [] Enable Capability
    - [] Collect all identifiers
    - [] Listen for request
  - [] Error reporting
  - [] Goto source
  - [] When to load files?
  - [] Reduce get_node_at()

- Writing
  - [x] Abstract
  - [x] Language introduction
  - [] Language definition
  - Subtyping introduction?
  - Read Thiemann papers
  - Multi-let as syntactic sugar?
  - Mutli-arguments as syntatcti sugar?

## Improvements

- Nice Verbose error
- Use own implementation of Span

## Restrictions

- Comment positions
- Errors while parsing
- Line comments have to end in '\n'

## Useless

- `Result<T, InferError>.span(span)`
