= A Note on Implementation
One unique problem of nix is that everything (all 100,000 packages, the operating system, and the standard library) are rooted in a _single file_ at #link("https://github.com/NixOS/nixpkgs/blob/master/flake.nix") or #link("https://github.com/NixOS/nixpkgs/blob/master/default.nix"), depending on whether you use a flake based system or not. To not get lost in the weeds, the nix evaluator heavily relies on the laziness features of the language to not evaluate all of the packages exhaustively. For the ultimate goal of auto-completing nixos options one would have to parse and type this very file with the goal to resolve the module system. This includes the standard library and bootstrapping code for the module system. To even reach it, the type inference algorithm has to support the same kind of laziness the nix evaluator uses to not get lost.


== Practical Type Inference in Face of Huge Syntax
Code inference in the general case is similar to depth-first search, digging down one syntax tree and only returning as soon as all branches have been exhausted. Since nix trees are huge, this approach is not feasible and one has to lean towards a breadth-first search style, which focuses on the currently inferred file and stops when "too far away". To achieve this behavior, the inference algorithm at some point has to decide to stop inference and jump to another unfinished function, remembering at which place it left off.
In the nix language, there are two natural places to do so. Laziness of records and let statements gives the natural approach that every newly named binding is a stop-point at which inference only proceeds as far as needed. One heuristic could be to go two more functions down and then return to the let or record to generate at least some approximation of the final type.

The import statement semantics of nix come in very handy at this point. Import statements act just as function calls with the only difference being, that the goto location is defined by path and not by name. Other than that, they can take arguments just as a function, and then try to apply given arguments to the file's expression.

This language design comes in very handy because that way, import statements do not occur at the top of the file where it would need to be decided how to continue typechecking them. They occur right at the location where they are needed, sometimes in let-expressions or record fields. This way, the laziness of records and let-expressions could already be enough to get laziness into the language.
As for the practical approach, I propose a new marker type which can be set to bindings of a context. This marker type should contain all the information to go back to type inference at a previous location. This probably means cloning the context or restoring it to the previous state – cloning is probably easier. Another approach could be to keep the names undefined and add another mapping between names and reconstruction information somewhere that acts as a fallback.


Some real-world example of import:
```nix
let
  overrides = {
    builtins = builtins // overrides;
  }
  // import ./lib.nix;
in
scopedImport overrides ./imported.nix
```

== Type Inference in a Language Server Setting
A language server setting adds one more level of complexity. A language server has to handle the communication between client (an editor like vim, emacs, vscode, etc.) and the server itself. It will be notified frequently of code changes and has to adapt to these changes almost immediately to not annoy the user. This is why rust-analyzer and nil, which I take as template for my own efforts, have chosen to use or create _incremental computation_ frameworks for the rust language.
The one used by rust-analyzer and nil (which is based off of rust-analyzer) is _salsa_. The name stems from the underlying red-green algorithm that decides whether a function needs to be reevaluated because its arguments changed or whether the memoized return value can be returned immediately.
In the end, salsa consists of _inputs_, _tracked functions_ and _tracked structs_. Inputs are divided into their durability and given to tracked functions. These tracked functions record the inputs and do some arbitrary computation with them. During these computations, the functions might create immutable tracked structs which can act as new inputs to other tracked functions. Tracked structs are interned into a db and act as a single identifier which are cheap to copy around and provide great performance benefits. With these components alone it is possible to create a hirarchy of pure functions that allow for reproducibility.

When implementing this incrementality framework, one has to decide where to draw the line between tracking everything too closely such that the framework bloat adds latency and tracking too few intermediate results such that recomputation is heavy again.

The generalized structure of the three language servers has this structure. A user opens a file and the lsp client sends the text to the language server. The language server stores the text somewhere and adds it to the typing pipeline. The first step of this pipeline is of course lexing and parsing the file. Nil already provides a parser for lossless syntax trees that are handy for error reporting. The file is then lowered into another HIR which is more or less syntax independent and thus changes less frequently. This is necessary because otherwise, everything would have to be recomputed all the time. After this, the HIR is given to the inference algorithm that tries to infer a type.


= Code Overview
*Inputs of LSP*:
- `File {content: string, }`

*Inputs of infer:*
- `AST { With(ExprId, ExprId) }` (lowered AST with expressions from the arena)

*Tracked structs:*
- `Ty { Lambda(Ty), With(Ty, Ty)}` (enum that stores the whole AST)
- `Context {bindings: Vec<_>, }`
- `TyVar {lower_bounds: Vec<Ty>, upper_bounds: Vec<Ty>, level: int}`

*Functions:*
- `infer` (main work)
  - calls itself with subtrees of the AST and new contexts
  - *Mutates* context
- `constrain` (constrains two types to be the same)
  - calls itself with subtrees of Ty and might cycle
  - *Mutates* Type variables → *Changes context*
- `coalesce` (reduce types to unions and intersections)
  - Create new types
- `extrude` (fix levels of problematic variables in a type scheme)
  - only creates new types
- `freshen_above` (Add new type variables at level > x)
  - only creates new types

