# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Garnix is a Rust-based parser and language server for the Nix programming language, featuring type inference using SimpleSub algorithm (Lionel Parreaux: "The simple essence of Subtyping"). The project consists of three main components in a Cargo workspace architecture:

- **parser2/**: Nix language parser with comprehensive error handling and AST generation using Rowan
- **infer/**: Type inference engine implementing SimpleSub algorithm for principal type inference
- **lsp/**: Language Server Protocol implementation providing IDE integration with hover hints, diagnostics, and inlay hints

## Development Commands

### Building
```bash
# Build all workspace members
cargo build

# Build specific component
cargo build -p parser2
cargo build -p infer  
cargo build -p lsp

# Build release version (required for LSP usage)
cargo build --release
```

### Testing
```bash
# Run all tests
cargo test

# Run tests for specific component
cargo test -p parser2
cargo test -p infer

# Run parser coverage analysis (parser2 only)
cd parser2 && ./cov.sh test && ./cov.sh merge && ./cov.sh report
```

### Running the Language Server
```bash
# After building in release mode
./target/release/lsp

# Or run directly with cargo
cargo run -p lsp
```

## Architecture Details

### Parser Architecture (parser2/)
- Uses Rowan library for lossless syntax trees with error recovery
- Features fuzzing support and extensive test suite with golden files in `test_data/`
- Error-resilient parsing with detailed error reporting via `ErrorKind` enum
- AST nodes auto-generated with typed wrappers over `SyntaxNode`

### Type Inference (infer/)
- Implements SimpleSub algorithm for subtyping-based type inference
- Uses Salsa for incremental computation and caching
- Lower-level IR transformation from parser AST to typed AST
- Context-based constraint solving with diagnostic generation

### LSP Integration (lsp/)
- Async LSP server using `async-lsp` with tower middleware stack
- Provides hover information showing inferred types and constraints
- Real-time diagnostics for parse errors and type errors
- Inlay hints displaying type information inline
- File watching and incremental updates

### Key Dependencies
- **rowan**: Lossless syntax tree implementation
- **salsa**: Incremental computation framework for type inference
- **async-lsp**: Asynchronous LSP server framework
- **strum**: Enum utilities for AST node types
- **annotate-snippets**: Error message formatting

### Test Structure
- Parser tests use golden file testing with `.nix` input and `.ast` expected output
- Inference tests located in `infer/src/tests/`
- Fuzzing infrastructure in `parser2/fuzz/`
- Test data organized by success/failure cases in `test_data/parser/{ok,err}/`

### Code Conventions
- Use `expect-test` crate for golden file testing
- Error types implement `Display` and `std::error::Error`
- AST traversal uses visitor pattern with `rowan::ast::AstNode`
- Salsa queries marked with `#[salsa::tracked]` for incremental computation
- LSP handlers use async/await with proper error propagation

### Migration Notes
- accessing a field from a tracked struct is a function call like `apply.fun(db)` change the previous direct field accesses to this style and add db to function parameters where needed to do so
