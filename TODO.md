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
  - [] import

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
  - [x] Language definition
  - [x] Nix language support

- Misc
  - Read Thiemann papers
  - Read original paper
  - Read Timpe paper


## Improvements

- Nice Verbose error
- Use own implementation of Span

## Restrictions

- Comment positions
- Errors while parsing
- Line comments have to end in '\n'

## Useless

- `Result<T, InferError>.span(span)`
