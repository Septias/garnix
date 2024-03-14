## TODO (17.03)

- Type Inference
  - All

- Parser
  - [x] Handle `inherit (lib)`
  - [x] Don't use expr as base
  - [] out: String interpolation
  - [] import
  - [] Add type-parsing

- LSP
  - [] Inlay type hints
    - [] Enable Capability
    - [] Collect all identifiers
    - [] Listen for request
  - [] Error reporting
  - [] Goto source
  - [] Reduce get_node_at()

- Writing
  - [x] Abstract
  - [x] Language introduction
  - [x] Language definition
  - [x] Nix language support
  - [] One-step Semantic

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
