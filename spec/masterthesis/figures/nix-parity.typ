#import "../text/functions.typ": *

// Feature parity of MacroNix (lean/, Builtins.lean) with NixLang.
// Legend:  ● typed and mechanized   ◐ restricted, encoded or derived   · missing
// Provenance: every ●/◐ row is a construct or builtin of the Lean development
// (minimal.lean, Qualified.lean, Builtins.lean); the encodings are those of
// lean/NixSuite.lean.

#let np-y = $●$
#let np-p = $◐$
#let np-n = text(fill: luma(170), $·$)

#let np-gr(body) = table.cell(colspan: 3, fill: luma(240), text(
  size: 8pt,
  weight: "bold",
  body,
))
#let np-f(body) = text(size: 8pt, body)
#let np-r(body) = text(size: 7.5pt, body)

#let nix-parity = figure(
  caption: [Feature parity of MacroNix with NixLang. ● typed and mechanized, ◐ restricted, encoded or derived, · not in place.],
  table(
    columns: (auto, auto, 1fr),
    align: (left + horizon, center + horizon, left + horizon),
    inset: 4pt,
    stroke: 0.4pt + luma(200),
    table.header([*Feature*], [*MacroNix*], [*Remark*]),

    np-gr[Language features],
    np-f[Functions, application], np-y, np-r[call-by-value application],
    np-f[`let` (non-recursive)], np-y, np-r[let-polymorphism with qualified schemes],
    np-f[`let { …; body = e; }` (deprecated)], np-p, np-r[desugared to nested `let`s],
    np-f[Record literals, selection `e.l`], np-y, np-r[scoped rows, three-way lookup],
    np-f[Nested keys `a.b.c = v`], np-p, np-r[desugared to nested records],
    np-f[Update `a // b`], np-y, np-r[asymmetric concatenation ‖],
    np-f[Dynamic keys `${e}`], np-y, np-r[first-class labels; string-typed keys answer ★],
    np-f[`e.l or d`], np-p, np-r[derived as `(e ‖ {l = d}).l`; clashes on a non-record `e`],
    np-f[`e ? l`], np-p, np-r[literal label only; refines nothing],
    np-f[`e ? a.b`], np-p, np-r[literal path desugared to nested `?` and `&&`; dynamic segments not in place],
    np-f[`inherit`, `inherit (e)`], np-p, np-r[desugared to bindings and selections],
    np-f[Patterns `{x, y ? d, ...}@s:`], np-p, np-r[desugared; every pattern is open, as rows are],
    np-f[`true`, `false`, `if`, `assert`], np-y, np-r[conditional unifies its branches],
    np-f[`!`, `&&`, `||`, `->`, `!=`, `>`, `>=`], np-p, np-r[derived from `if`, `not`, `==`, `<`],
    np-f[Pipes `x |> f`, `f <| x`], np-p, np-r[derived as application],
    np-f[Integers, strings, `null`], np-y, np-r[base types `int`, `string`, `null`],
    np-f[String interpolation, string `+`], np-p, np-r[`strcat` on strings; no implicit coercion],
    np-f[Indented strings `'' … ''`], np-p, np-r[string literals; a matter of the parser],
    np-f[Lists `[e₁ … eₙ]`], np-p, np-r[homogeneous `[τ]`; mixed lists only at `[★]`],
    np-f[Laziness], np-p, np-r[record fields and list cells lazy, application strict],

    np-gr[Builtins],
    np-f[`add` `sub` `mul` `div`], np-y, np-r[on `int`; `div` fails on 0],
    np-f[`lessThan`, `<=`], np-p, np-r[on `int` only],
    np-f[`==`], np-p, np-r[atomic values only; fails on records, functions, non-empty lists],
    np-f[`head` `tail` `elemAt` `length`], np-y, np-r[`head`, `tail` and `elemAt` fail out of range],
    np-f[`map` `filter` `foldl'` `++`], np-y, np-r[],
    np-f[`toString`], np-p, np-r[on `int` only],
    np-f[`isAttrs`], np-y, np-r[],
    np-f[`hasAttr` `getAttr`], np-p, np-r[as `e ? l` and dynamic selection],

    np-gr[Not yet in place],
    np-f[`rec { … }`, recursive `let`], np-n, np-r[needs recursive bindings (Recursive Types)],
    np-f[`with e;`], np-n, np-r[planned as elaboration to concatenation and selection],
    np-f[Closed patterns `{ a }:` (no `...`)], np-n, np-r[extra fields not rejected; needs absence (Negative Information)],
    np-f[Dynamic `let ${e} = …;`], np-n, np-r[binds a variable by a computed name],
    np-f[Floats, paths, `<nixpkgs>`, URIs], np-n, np-r[further base types and their coercions],
    np-f[Optional fields, `if c then {a = 1;} else {}`], np-n, np-r[rejected declaratively; needs a dynamic row ★ᵣ (Gradual)],
    np-f[Heterogeneous lists], np-n, np-r[`[★]` declaratively, clash algorithmically (Gradual)],
    np-f[`is*` predicates, `typeOf`], np-n, np-r[typable as `α → bool`; useful only with occurrence typing],
    np-f[`elem` `all` `any` `concatMap` `genList` `sort` `concatLists`], np-n, np-r[routine, not added],
    np-f[String builtins (`substring`, `replaceStrings`, `match`, …)], np-n, np-r[routine, not added],
    np-f[`attrNames` `attrValues` `mapAttrs` `listToAttrs` `catAttrs`], np-n, np-r[need label lists or type connectives],
    np-f[`removeAttrs` `intersectAttrs`], np-n, np-r[need absence (Negative Information)],
    np-f[`__functor`, `__overrides`], np-n, np-r[magic attributes],
    np-f[`throw` `abort` `seq` `deepSeq` `trace` `tryEval`], np-n, np-r[control and evaluation order],
    np-f[`import`, `derivation`, `fetch*`, `readFile`, `getEnv`], np-n, np-r[impure or I/O; out of scope],
    np-f[`toJSON` `fromJSON` `fromTOML`], np-n, np-r[serialization; results only at ★],
  ),
)
