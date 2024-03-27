## Garnix

**Garnix** is another **parser** and **language server** for the Nix programming language. It's theoretical foundation is the SimpleSub [Lionel Parreaux: The simple essence of Subtyping] algorithm which is much stronger than normal HM-style type inferce.

### Structure
This project is a mono repository and contains the parser, inference algorithm and language server in three respective folder:

| Folder | Content |
| ------ | ------- |
| parser | The **parser** folder contains the parser for the nix language. It is capable of parsing all modern nix language features (no deprecated let-bindings) but needs extensive randomised testing to verify. |
|infer | The **infer** folder contains the type inference algorithm capable of infering principal types for the nix language.
|lsp| The **lsp** folder contains the language server which wraps the inference library to bring type hints and error reporting too the user.

### Theoretical foundation
The repository is related to the Bachelor Thesis of Sebastian Klähn at []. It contains a discussion about the current state of developer toolings in the nix ecosystem, a thorough language definition and the theoretical background for the type inference algorithm and it's drawbacks.


### Development
This project uses common development guidlines. To contribut PRs have to be made against this repository that comply to the cargo format and clippy guidlines. Tests also have to run through.

#### Testing
The parser and inference module have extensive tests in the respective `tests` subfolders. These tests can be used to test new implementations and learn more about the capabiliets of the current project.
