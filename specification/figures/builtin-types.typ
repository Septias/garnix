#import "../functions.typ": *


#let builtin_types = figure(caption: [The nix language builtins and their respective types.], table(
  columns: (auto, 1fr),
  table.header([*Builtin*], [*Type*]),
  [abort `s`                   ], $ τ -> ⊥ $,
  [add `e1 e2`                 ], $ number -> number -> number $,
  [addDrvOutputDependencies `s`], $ str -> str $,
  [all `pred list`             ], $ (α -> bool) -> [α] -> bool $,
  [any `pred list`             ], $ (α -> bool) -> [α] -> bool $,
  [attrNames `set`             ], $ recordType -> [ str ] $,
  table.cell(colspan: 2, [This looses precision and could be a list of strings if they are first-class values.] ),
  [attrValues `set`            ], $ recordType -> [ or.big oi(τ_i)   ] $,
  [baseNameOf `x`              ], $ str ∨ path -> str $,
  [bitAnd `e1 e2`              ], $ int -> int -> int $,
  [bitOr `e1 e2`               ], $ int -> int -> int $,
  [bitXor `e1 e2`              ], $ int -> int -> int $,
  [break `v`                   ], $ α -> α $,
  [builtins                    ], $ {"abort": τ → bot, "all": (α -> bool) -> [α] -> bool, .. } $,
  table.cell(colspan: 2, [This type has to be filled with all the builtins.] ),
  [ catAttrs `attr list`      ], $ str -> [oj(recordType)_j] -> [or.big τ_(j i)] "where" l_(j i) =  str $,
  [ceil `double`             ], $ number -> int $,
  [compareVersions `s1 s2`     ], $ str -> str -> int$,
  [concatLists `lists`       ],   $ [[α]] -> [α] $,

  [concatMap `f list`          ], $ (α -> β) -> [[α]] -> [β] $,
  [concatStringsSep `sep list` ], $ str -> [str] -> str $,
  table.cell(colspan: 2, [The strings don't have to be equal, how to dot that?] ),
  [convertHash `args`          ], $ { "hash": str, \ "toHashFormat": "base16" ∨ "nix32" ∨ "base32" ∨ "base64" ∨ "sri", \ "hashAlgo": "md5" ∨ "sha1" ∨ "sha256" ∨ "sha512" } -> str $,
  table.cell(colspan: 2, [TODO: the inner strings should we wrapped in ""] ),
  [m currentSystem             ], $ () -> str $,
  [m currentTime               ], $ () -> int $,
  [deepSeq `e1 e2`             ], $ α -> β -> β $,
  [dirOf `s`                   ], $ str ∨ path -> str $,
  [div `e1 e2`                 ], $ number -> number -> number $,
  [elem `x xs`                 ], $ recordType -> l -> bool $,
  [elemAt `xs n`               ], $ [α] -> n -> α $,
  [false                       ], $ bool $,
  [fetchClosure `args`         ], $ { omitted } -> path $,
  [fetchGit `args`             ], $ { omitted } -> path $,
  [fetchTarball `args`         ], $ { omitted } -> path $,
  [fetchTree `input`           ], $ { omitted } -> path $,
  [fetchurl `arg`              ], $ { omitted } -> path $,
  [toString                    ], $¬{} ∨ {"toString": α ∧ {} -> str} -> str$,
  [filter `f list`             ], $ (α -> bool) -> [α] -> [α] $,
  [filterSource `pred path`    ], $ (path -> bool) -> path -> [path] $,
  [findFile `search lookup`    ], $ [{path: str, "prefix": str}] -> str -> str $,
  [floor `double`              ], $ number -> int $,
  [foldl' `op null list`       ], $ (α -> β -> α) -> α -> [β] -> α $,
  [fromJSON `e`                ], $ str -> {} $,
  [fromTOML `e`                ], $ str -> {} $,
  [functionArgs `f`            ], $ (openPat -> α) -> {l_i: bool} $,
  [genList `generator length`  ], $ (int -> α) -> int -> [α] $,
  [genericClosure `attrset`    ], $ todo(?) $,
  [getAttr `s set`             ], $ l -> recordType -> τ_i "where" l = l_i $,
  [getContext `s`              ], $ str -> todo({}) $,
  [i getEnv `s`                ], $ str -> str -> todo({}) $,
  [getFlake `args`             ], $ todo({}) -> path $,
  [groupBy `f list`            ], $ (α -> str) -> [α] -> {oi(str\: [α])} $,
  table.cell(colspan: 2, [Could be more specific, but this is already hard.] ),
  [hasAttr `s set`             ], $ recordType -> l -> bool $,
  [hasContext `s`              ], $ str -> bool $,
  [hashFile `type p`           ], $ path ∨ str -> str $,
  [hashString `type s`         ], $ str -> str $,
  [head `list`                 ], $ [α] -> α $,
  [import `path`               ], $ path -> α $,
  [intersectAttrs `e1 e2`      ], $ recordType -> recordType -> todo(recordType) $,
  [isAttrs `e`                 ], $ τ -> bool $,
  [isBool `e`                  ], $ τ -> bool $,
  [isFloat `e`                 ], $ τ -> bool $,
  [isFunction `e`              ], $ τ -> bool $,
  [isInt `e`                   ], $ τ -> bool $,
  [isList `e`                  ], $ τ -> bool $,
  [isNull `e`                  ], $ τ -> bool $,
  [isPath `e`                  ], $ τ -> bool $,
  [isString `e`                ], $ τ -> bool $,
  [langVersion                 ], $ () -> str $,
  [length `e`                  ], $ [α] -> int $,
  [lessThan `e1 e2`            ], $ number -> number -> bool $,
  [listToAttrs `e`             ], $ [todo(oi((l,τ))] -> record) $,
  [map `f list`                ], $ (α -> β) -> [α] -> [β] $,
  [mapAttrs `f attrset`        ], $ (α -> β) -> recordType -> todo("?") $,
  [match `regex str`           ], $ str -> str -> [str] $,
  [mul `e1 e2`                 ], $ number -> number -> number $,
  [nixPath                     ], $ () -> [{path: str, "prefix": str}] $,
  [nixVersion                  ], $ () -> str $,
  [null                        ], $ null $,
  [outputOf `drv out`          ], $ drv -> path $,
  [parseDrvName `s`            ], $ str -> [str] $,
))

#let descr = [
  [ Abort Nix expression evaluation and print the error message `s`.],
  [ Return the sum of the numbers `e1` and `e2`.],
  [ Copy string `s` while turning constant string context elements into derivation-deep string context.],
  [ Return `true` if `pred` returns `true` for all elements of `list`, else `false`.],
  [ Return `true` if `pred` returns `true` for any element of `list`, else `false`.],
  [ Return the attribute names of `set`, sorted alphabetically.],
  [ Return the values of attributes in `set`, ordered by sorted names.],
  [ Return the last component of path or string `x`.],
  [ Bitwise AND of integers `e1` and `e2`.],
  [ Bitwise OR of integers `e1` and `e2`.],
  [ Bitwise XOR of integers `e1` and `e2`.],
  [ In debug mode, pause evaluation and enter REPL; otherwise return `v`.],
  [ A set containing all built-in functions and values.],
  [ Collect the attribute `attr` from each set in `list`, ignoring sets without it.],
  [ Round `double` up to the nearest integer.],
  [ Compare version strings; returns `-1`, `0`, or `1`.],
  [ Flatten a list of lists into a single list.],

  [ Equivalent to `concatLists (map f list)`.],
  [ Join strings in `list` with separator `sep`.],
  [ Convert a hash string between formats (base16, sha256, SRI, etc.).],
  [ System string like `"x86_64-linux"`.],
  [ Unix time at moment of evaluation (cached).],
  [ Like `seq`, but fully evaluate nested structures in `e1` first.],
  [ Directory component of string `s`.],
  [ Integer division.],
  [ `true` if `x` is in list `xs`.],
  [ Return the `n`-th element of `xs`.],
  [ Boolean literal `false`.],
  [ Fetch a store path closure from a binary cache.],
  [ Fetch a Git repo or revision.],
  [ Download and unpack a tarball.],
  [ Fetch a tree or file with metadata.],
  [ Download a URL and return store path.],
  [ Return elements where `f` yields `true`.],
  [ Copy sources filtering by `pred`.],
  [ Search `lookup` in `search` path.],
  [ Round `double` down to nearest integer.],
  [ Left fold over `list` with `op`.],
  [ Parse JSON string `e` into Nix value.],
  [ Parse TOML string `e` into Nix value.],
  [ Return formal argument set of function `f`.],
  [ Generate list of given `length` using `generator`.],
  [ Compute transitive closure of a relation.],
  [ Return attribute `s` from `set`.],
  [ Return derivation context of string `s`.],
  [ Return environment variable `s`.],
  [ Fetch flake reference and outputs.],
  [ Group elements by key `f(element)`.],
  [ `true` if string `s` has nonempty context.],
  [ Compute hash of file at `p`.],
  [ Compute hash of string `s`.],
  [ First element of `list`.],
  [ Load and evaluate Nix file at `path`.],
  [ Attributes in `e2` whose names occur in `e1`.],
  [ `true` if `e` is an attribute set.],
  [ `true` if `e` is a boolean.],
  [ `true` if `e` is a float.],
  [ `true` if `e` is a function.],
  [ `true` if `e` is an integer.],
  [ `true` if `e` is a list.],
  [ `true` if `e` is `null`.],
  [ `true` if `e` is a path.],
  [ `true` if `e` is a string.],
  [ Integer of current Nix language version.],
  [ Length of list `e`.],
  [ `true` if `e1 < e2`.],
  [ Convert list of `{name, value}` to attrset.],
  [ Apply `f` to each element of `list`.],
  [ Apply `f` to each attribute in `attrset`.],
  [ If `regex` matches `str`, return capture groups, else `null`.],
  [ Multiply integers `e1 * e2`.],
  [ List of search path entries for lookups.],
  [ String version of Nix.],
  [ Literal `null`.],
  [ Return output path of derivation.],
  [ Parse a derivation name into components.],
  [ Parse a derivation name into components.]
]

*abort* `s` : Abort Nix expression evaluation and print the error message `s`.
*add* `e1 e2` : Return the sum of the numbers `e1` and `e2`.
*addDrvOutputDependencies* `s` : Copy string `s` while turning constant string context elements into derivation-deep string context.
*all* `pred list` : Return `true` if `pred` returns `true` for all elements of `list`, else `false`.
*any* `pred list` : Return `true` if `pred` returns `true` for any element of `list`, else `false`.
*attrNames* `set` : Return the attribute names of `set`, sorted alphabetically.
*attrValues* `set` : Return the values of attributes in `set`, ordered by sorted names.
*baseNameOf* `x` : Return the last component of path or string `x`.
*bitAnd* `e1 e2` : Bitwise AND of integers `e1` and `e2`.
*bitOr* `e1 e2` : Bitwise OR of integers `e1` and `e2`.
*bitXor* `e1 e2` : Bitwise XOR of integers `e1` and `e2`.
*break* `v` : In debug mode, pause evaluation and enter REPL; otherwise return `v`.
*builtins* : A set containing all built-in functions and values.
*catAttrs* `attr list` : Collect the attribute `attr` from each set in `list`, ignoring sets without it.
*ceil* `double` : Round `double` up to the nearest integer.
*compareVersions* `s1 s2` : Compare version strings; returns `-1`, `0`, or `1`.
*concatLists* `lists` : Flatten a list of lists into a single list.
*concatMap* `f list` : Equivalent to `concatLists (map f list)`.
*concatStringsSep* `sep list` : Join strings in `list` with separator `sep`.
*convertHash* `args` : Convert a hash string between formats (base16, sha256, SRI, etc.).
*currentSystem* : System string like `"x86_64-linux"`.
*currentTime* : Unix time at moment of evaluation (cached).
*deepSeq* `e1 e2` : Like `seq`, but fully evaluate nested structures in `e1` first.
*dirOf* `s` : Directory component of string `s`.
*div* `e1 e2` : Integer division.
*elem* `x xs` : `true` if `x` is in list `xs`.
*elemAt* `xs n` : Return the `n`-th element of `xs`.
*false* : Boolean literal `false`.
*fetchClosure* `args` : Fetch a store path closure from a binary cache.
*fetchGit* `args` : Fetch a Git repo or revision.
*fetchTarball* `args` : Download and unpack a tarball.
*fetchTree* `input` : Fetch a tree or file with metadata.
*fetchurl* `arg` : Download a URL and return store path.
*filter* `f list` : Return elements where `f` yields `true`.
*filterSource* `pred path` : Copy sources filtering by `pred`.
*findFile* `search lookup` : Search `lookup` in `search` path.
*floor* `double` : Round `double` down to nearest integer.
*foldl'* `op nul list` : Left fold over `list` with `op`.
*fromJSON* `e` : Parse JSON string `e` into Nix value.
*fromTOML* `e` : Parse TOML string `e` into Nix value.
*functionArgs* `f` : Return formal argument set of function `f`.
*genList* `generator length` : Generate list of given `length` using `generator`.
*genericClosure* `attrset` : Compute transitive closure of a relation.
*getAttr* `s set` : Return attribute `s` from `set`.
*getContext* `s` : Return derivation context of string `s`.
*getEnv* `s` : Return environment variable `s`.
*getFlake* `args` : Fetch flake reference and outputs.
*groupBy* `f list` : Group elements by key `f(element)`.
*hasAttr* `s set` : `true` if `set` has attribute `s`.
*hasContext* `s` : `true` if string `s` has nonempty context.
*hashFile* `type p` : Compute hash of file at `p`.
*hashString* `type s` : Compute hash of string `s`.
*head* `list` : First element of `list`.
*import* `path` : Load and evaluate Nix file at `path`.
*intersectAttrs* `e1 e2` : Attributes in `e2` whose names occur in `e1`.
*isAttrs* `e` : `true` if `e` is an attribute set.
*isBool* `e` : `true` if `e` is a boolean.
*isFloat* `e` : `true` if `e` is a float.
*isFunction* `e` : `true` if `e` is a function.
*isInt* `e` : `true` if `e` is an integer.
*isList* `e` : `true` if `e` is a list.
*isNull* `e` : `true` if `e` is `null`.
*isPath* `e` : `true` if `e` is a path.
*isString* `e` : `true` if `e` is a string.
*langVersion* : Integer of current Nix language version.
*length* `e` : Length of list `e`.
*lessThan* `e1 e2` : `true` if `e1 < e2`.
*listToAttrs* `e` : Convert list of `{name, value}` to attrset.
*map* `f list` : Apply `f` to each element of `list`.
*mapAttrs* `f attrset` : Apply `f` to each attribute in `attrset`.
*match* `regex str` : If `regex` matches `str`, return capture groups, else `null`.
*mul* `e1 e2` : Multiply integers `e1 * e2`.
*nixPath* : List of search path entries for lookups.
*nixVersion* : String version of Nix.
*null* : Literal `null`.
*outputOf* `drv out` : Return output path of derivation.
*parseDrvName* `s` : Parse a derivation name into components.
