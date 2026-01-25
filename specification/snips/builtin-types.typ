#import "../functions.typ": *


#let builtin_types = table(
  columns: (auto, 1fr, 1fr),
  table.header([*Builtin*], [*Description*],  [*Type*]),
  [abort `s`                   ], [ Abort Nix expression evaluation and print the error message `s`.], $ τ -> ⊥ $,
  [add `e1 e2`                 ], [ Return the sum of the numbers `e1` and `e2`.], $ {α <= int}. α -> α -> α $,
  [addDrvOutputDependencies `s`], [ Copy string `s` while turning constant string context elements into derivation-deep string context.], $ str -> str $,
  [all `pred list`             ], [ Return `true` if `pred` returns `true` for all elements of `list`, else `false`.], $ manyTypes -> bool $,
  [any `pred list`             ], [ Return `true` if `pred` returns `true` for any element of `list`, else `false`.], $ manyTypes -> bool $,
  [attrNames `set`             ], [ Return the attribute names of `set`, sorted alphabetically.], $ openPat -> [oi(l_i)] $,
  [| attrValues `set`          ], [ Return the values of attributes in `set`, ordered by sorted names.], $ openPat -> [oi(τ_i)] $,
  [baseNameOf `x`              ], [ Return the last component of path or string `x`.], $ {a <= str ∨ path}. a -> str $,
  [bitAnd `e1 e2`              ], [ Bitwise AND of integers `e1` and `e2`.], $ int -> int -> int $,
  [bitOr `e1 e2`               ], [ Bitwise OR of integers `e1` and `e2`.], $ int -> int -> int $,
  [bitXor `e1 e2`              ], [ Bitwise XOR of integers `e1` and `e2`.], $ int -> int -> int $,
  [break `v`                   ], [ In debug mode, pause evaluation and enter REPL; otherwise return `v`.], $ α -> α ∨ ⊥ $,
  [builtins                    ], [ A set containing all built-in functions and values.], $ {} $,
  [| catAttrs `attr list`      ], [ Collect the attribute `attr` from each set in `list`, ignoring sets without it.], $ str -> [oj(record)_j] -> todo([oj(τ_j)]) $,
  [) ceil `double`             ], [ Round `double` up to the nearest integer.], $ number -> int $,
  [compareVersions `s1 s2`     ], [ Compare version strings; returns `-1`, `0`, or `1`.], $ str -> str -> (-1 ∨ 0 ∨ 1) $,
  [| concatLists `lists`       ], [ Flatten a list of lists into a single list.],
  $ [oj([oi(τ_i)])] -> [oi(τ_(i 1)) … oi(τ_(i j))] $,

  [concatMap `f list`          ], [ Equivalent to `concatLists (map f list)`.], $ (α -> β) -> [α] -> [β] $,
  [concatStringsSep `sep list` ], [ Join strings in `list` with separator `sep`.], $ str -> [str] -> str $,
  [convertHash `args`          ], [ Convert a hash string between formats (base16, sha256, SRI, etc.).], $ str -> str $,
  [m currentSystem             ], [ System string like `"x86_64-linux"`.], $ () -> str $,
  [m currentTime               ], [ Unix time at moment of evaluation (cached).], $ () -> int $,
  [deepSeq `e1 e2`             ], [ Like `seq`, but fully evaluate nested structures in `e1` first.], $ τ₁ -> τ₂ -> τ₂ $,
  [dirOf `s`                   ], [ Directory component of string `s`.], $ {α <= str ∨ path}. α -> str $,
  [div `e1 e2`                 ], [ Integer division.], $ number -> number -> number $,
  [elem `x xs`                 ], [ `true` if `x` is in list `xs`.], $ openPat -> l -> bool $,
  [elemAt `xs n`               ], [ Return the `n`-th element of `xs`.], $ manyTypes -> todo(int -> τ_(int)) $,
  [false                       ], [ Boolean literal `false`.], $ bool $,
  [fetchClosure `args`         ], [ Fetch a store path closure from a binary cache.], $ todo({}) -> path $,
  [fetchGit `args`             ], [ Fetch a Git repo or revision.], $ todo({}) -> path $,
  [fetchTarball `args`         ], [ Download and unpack a tarball.], $ todo({}) -> path $,
  [fetchTree `input`           ], [ Fetch a tree or file with metadata.], $ todo({}) -> path $,
  [fetchurl `arg`              ], [ Download a URL and return store path.], $ todo({}) -> path $,
  [filter `f list`             ], [ Return elements where `f` yields `true`.], $ (α -> bool) -> [α] -> [α] $,
  [filterSource `pred path`    ], [ Copy sources filtering by `pred`.], $ (path -> bool) -> path -> [path] $,
  [findFile `search lookup`    ], [ Search `lookup` in `search` path.], $ ? $,
  [floor `double`              ], [ Round `double` down to nearest integer.], $ number -> int $,
  [foldl' `op null list`       ], [ Left fold over `list` with `op`.], $ (α -> β -> α) -> α -> [β] -> α $,
  [fromJSON `e`                ], [ Parse JSON string `e` into Nix value.], $ str -> {} $,
  [fromTOML `e`                ], [ Parse TOML string `e` into Nix value.], $ str -> {} $,
  [functionArgs `f`            ], [ Return formal argument set of function `f`.], $ (openPat -> α) -> manyTypes $,
  [genList `generator length`  ], [ Generate list of given `length` using `generator`.], $ ? $,
  [genericClosure `attrset`    ], [ Compute transitive closure of a relation.], $ ? $,
  [getAttr `s set`             ], [ Return attribute `s` from `set`.], $ l -> openPat -> todo(τ_l) $,
  [getContext `s`              ], [ Return derivation context of string `s`.], $ str -> todo({}) $,
  [i getEnv `s`                ], [ Return environment variable `s`.], $ str -> str -> str $,
  [getFlake `args`             ], [ Fetch flake reference and outputs.], $ todo({}) -> path $,
  [groupBy `f list`            ], [ Group elements by key `f(element)`.], $ (α -> bool) -> [α] -> ([α], $,
  [hasAttr `s set`             ], [ `true` if string `s` has nonempty context.], $ openPat -> l -> bool $,
  [hasContext `s`              ], [ Compute hash of file at `p`.], $ str -> bool $,
  [hashFile `type p`           ], [ Compute hash of string `s`.], $ path ∨ str -> str $,
  [hashString `type s`         ], [ First element of `list`.], $ str -> str $,
  [head `list`                 ], [ Load and evaluate Nix file at `path`.], $ [α] -> α $,
  [import `path`               ], [ Attributes in `e2` whose names occur in `e1`.], $ path -> τ $,
  [intersectAttrs `e1 e2`      ], [ `true` if `e` is an attribute set.], $ openPat -> openPat -> todo(record) $,
  [isAttrs `e`                 ], [ `true` if `e` is a boolean.], $ τ -> bool $,
  [isBool `e`                  ], [ `true` if `e` is a float.], $ τ -> bool $,
  [isFloat `e`                 ], [ `true` if `e` is a function.], $ τ -> bool $,
  [isFunction `e`              ], [ `true` if `e` is an integer.], $ τ -> bool $,
  [isInt `e`                   ], [ `true` if `e` is a list.], $ τ -> bool $,
  [isList `e`                  ], [ `true` if `e` is `null`.], $ τ -> bool $,
  [isNull `e`                  ], [ `true` if `e` is a path.], $ τ -> bool $,
  [isPath `e`                  ], [ `true` if `e` is a string.], $ τ -> bool $,
  [isString `e`                ], [ Integer of current Nix language version.], $ τ -> bool $,
  [langVersion                 ], [ Length of list `e`.], $ () -> str $,
  [length `e`                  ], [ `true` if `e1 < e2`.], $ [α] -> int $,
  [lessThan `e1 e2`            ], [ Convert list of `{name, value}` to attrset.], $ number -> number -> bool $,
  [listToAttrs `e`             ], [ Apply `f` to each element of `list`.], $ [todo(oi((l,τ))] -> record) $,
  [map `f list`                ], [ Apply `f` to each attribute in `attrset`.], $ (α -> β) -> [α] -> [β] $,
  [mapAttrs `f attrset`        ], [ If `regex` matches `str`, return capture groups, else `null`.], $ (α -> β) -> openPat -> todo("yay") $,
  [match `regex str`           ], [ Multiply integers `e1 * e2`.], $ str -> str -> [str] $,
  [mul `e1 e2`                 ], [ List of search path entries for lookups.], $ number -> number -> number $,
  [nixPath                     ], [ String version of Nix.], $ () -> [str] $,
  [nixVersion                  ], [ Literal `null`.], $ () -> str $,
  [null                        ], [ Return output path of derivation.], $ null $,
  [outputOf `drv out`          ], [ Parse a derivation name into components.], $ drv -> path $,
  [parseDrvName `s`            ], [ Parse a derivation name into components.], $ str -> [str] $,
)


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
