-- `runF` ON THE NIX TEST-SUITE. Hand translations of `testdata/eval-okay-*.nix`
-- into the calculus, run and compared against Nix's `.exp`.
--
-- Run with  `lake env lean NixSuite.lean`  (not in the library globs).
--
-- ## Translation
-- - `a // b`            → `b ‖ a`              (Nix: right wins; ‖: left wins)
-- - `e.p₁.….pₙ or d`    → `((e ‖ {p₁ = {}}).p₁ … ‖ {pₙ = d}).pₙ`
-- - `a.b.c = v`         → nested records, siblings merged
-- - `"${"s"}"` as a key  → the label `s`;  a string bound to a variable and used
--                         as a key → `lab s` and `selDyn` / `rcdDyn`
-- - `{x, y ? d}@s: e`   → `λs. let x = s.x in let y = (s ‖ {y = d}).y in e`
--                         (open and closed patterns coincide: rows are open)
-- - non-recursive `rec {…}` / `let {… body}` → nested `let`s, then the record
-- - OPAQUE operators (`+`, `if`, `?`, `assert`, lists, string interpolation,
--   `toString`) → a TUPLE record `{_1 = a; _2 = b; …}` of their operands.
--   Every operand stays typed, nothing is forced equal, and — unlike a
--   λ-bound hole — nothing enters Γ, so let-generalization is untouched.
--   The price: the tuple's type says nothing about the operator's result.
-- Constants carry their sort: `int`, `str`, `bool`, `null`.
--
-- ## Results                                    runF                .exp   agrees
-- attrs               (taken branch)            int                 987    ✓
--                     (whole, opaque if)        ok, `as.a` at ★     —      ✓
-- attrs3              (opaque if/++)            ok, every leaf      str    ✓
-- attrs5 e1–e6        or-chains, incl. a        int str int int     ✓      ✓
--                     missing intermediate      str str
-- attrs5 e7           `(123).bla or …`          clash               "xyzzy" ✗  (1)
-- dynamic-attrs-2     literal / ⌊l⌋ keys        bool / bool         true   ✓
-- dynamic-attrs-bare  5 of 6 fields             bool (hasAttrs opaque) true ✓
--                     binds, open hole          ★                   true   ✓ (2)
--                     binds, hole := `bc`       bool                true   ✓
-- scope-1/2/3/7       shadowing, rec, inherit   int ×4              3 1 4 1 ✓
-- let, new-let        opaque `+`                ok                  str    ✓
-- patterns            @-patterns, defaults,     ok, h polymorphic   str    ✓
--                     h at two row shapes
-- empty-args          `{}:` pattern             ok                  str    ✓
-- inherit-from        inherit (e), merged attrs {inner: {d: int | c: int}} ✓
-- skipped: scope-4/6 (mutually recursive defaults), callable-attrs (__functor),
--          overrides / attrs6 (__overrides), attrs5's `with` element
--
-- (1) Nix's `or` also catches selection on a NON-record; `‖` with an `int`
--     clashes. Encoding limit of `or`, or a missing rule.
-- (2) A label computed by interpolation needs a λ-hole (a tuple has no label
--     type): the key is unknown, so the field reads at ★, refined to `bool`
--     once the hole computes `bc`.

import InferFnTerm

namespace MinimalCalculus
namespace NixSuite

def keyS : Key → String
  | .lit l => l
  | .var α => "k" ++ toString α.length

mutual
def tyS : Ty String → String
  | .var α    => "t" ++ toString α.length
  | .base b   => b
  | .lab k    => "⌊" ++ keyS k ++ "⌋"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyS τ₁ ++ " → " ++ tyS τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowS ρ ++ "}"
def rowS : Row String → String
  | .empty     => "ε"
  | .var α     => "r" ++ toString α.length
  | .sing l τ  => l ++ ": " ++ tyS τ
  | .cat ρ₁ ρ₂ => rowS ρ₁ ++ " | " ++ rowS ρ₂
  | .dsing q τ => "${" ++ keyS q ++ "}: " ++ tyS τ
end

def verdict : IRes (Ty String × SolverState String) → String
  | .ok (τ, S) => tyS (τ.applySubst S.subst)
  | .fail m    => "fail: " ++ m
  | .oof       => "out of fuel"

abbrev E := Expr String
def run (e : E) : String := verdict (runF id 1000 e)

def v (x : String) : E := .var x
def int : E := .con "int"
def str : E := .con "str"
def bool : E := .con "bool"
def null : E := .con "null"

def body : List (String × E) → RecBody E
  | []           => .empty
  | [(l, e)]     => .field l e
  | (l, e) :: bs => .cat (.field l e) (body bs)
def rcd (bs : List (String × E)) : E := .rcd (body bs)
/-- the opaque tuple -/
def tup (es : List E) : E :=
  rcd ((es.zipIdx 1).map fun (e, i) => ("_" ++ toString i, e))

def lam (xs : List String) (e : E) : E := xs.foldr .lam e
def app (f : E) (as : List E) : E := as.foldl .app f
def lets (bs : List (String × E)) (e : E) : E := bs.foldr (fun (x, e₁) e₂ => .letE x e₁ e₂) e
def sels (e : E) (ls : List String) : E := ls.foldl .sel e
/-- `a // b` -/
def upd (a b : E) : E := .cat b a
/-- `e.p₁.….pₙ or d` -/
def orE (e : E) : List String → E → E
  | [],      _ => e
  | [l],     d => .sel (.cat e (rcd [(l, d)])) l
  | l :: ls, d => orE (.sel (.cat e (rcd [(l, rcd [])])) l) ls d
/-- `{x, y ? d, …}@s: e` -/
def pat (s : String) (fs : List (String × Option E)) (e : E) : E :=
  .lam s (lets (fs.map fun
    | (x, none)   => (x, .sel (v s) x)
    | (x, some d) => (x, .sel (.cat (v s) (rcd [(x, d)])) x)) e)

---------------------------------------------------------------- attrs ---------
-- let { as = { x = 123; y = 456; } // { z = 789; } // { z = 987; };
--       body = if as ? a then as.a else assert as ? z; as.z; }          → 987
def asAttrs : E := upd (rcd [("x", int), ("y", int)]) (upd (rcd [("z", int)]) (rcd [("z", int)]))
def attrs : E := lets [("as", asAttrs)]
  (tup [tup [v "as"], .sel (v "as") "a", tup [tup [v "as"], .sel (v "as") "z"]])
#guard run attrs
  = "{_1: {_1: {x: int | y: int | ε | z: int | z: int | ε}} | _2: ★ | _3: {_1: {_1: {x: int | y: int | ε | z: int | z: int | ε}} | _2: int}}"
-- the branch Nix takes
#guard run (lets [("as", asAttrs)] (.sel (v "as") "z"))
  = "int"

---------------------------------------------------------------- attrs3 --------
-- config = { services.sshd.enable = true; services.sshd.port = 22;
--   services.httpd.port = 80; hostName = "itchy"; a.b.….z = "x";
--   foo = { a = "a"; b.c = "c"; }; };
-- in if config.services.sshd.enable then "foo ${toString ….port} …" else "bar"
def deep : List String := "abcdefghijklmnopqrstuvwxyz".toList.map toString
def config : E := rcd
  [ ("services", rcd [ ("sshd", rcd [("enable", bool), ("port", int)])
                     , ("httpd", rcd [("port", int)]) ])
  , ("hostName", str)
  , ("a", deep.tail.foldr (fun l e => rcd [(l, e)]) str)
  , ("foo", rcd [("a", str), ("b", rcd [("c", str)])]) ]
def attrs3 : E := lets [("config", config)] (tup
  [ sels (v "config") ["services", "sshd", "enable"]
  , tup [ tup [str, tup [sels (v "config") ["services", "sshd", "port"]]
              , tup [sels (v "config") ["services", "httpd", "port"]]
              , sels (v "config") ["hostName"]]
        , sels (v "config") deep
        , sels (v "config") ["foo", "a"]
        , sels (v "config") ["foo", "b", "c"] ]
  , str ])
#guard run attrs3
  = "{_1: bool | _2: {_1: {_1: str | _2: {_1: int} | _3: {_1: int} | _4: str} | _2: str | _3: str | _4: str} | _3: str}"

---------------------------------------------------------------- attrs5 --------
-- as = { x.y.z = 123; a.b.c = 456; };  bs = { f-o-o.bar = "foo"; };
-- [ as.x.y.z  as.foo or "foo"  as.x.y.bla or as.a.b.c  as.a.b.c or as.x.y.z
--   as.x.y.bla or bs.f-o-o.bar or "xyzzy"  as.x.y.bla or bs.bar.foo or "xyzzy"
--   (123).bla or null.foo or "xyzzy"  (fold or [] [true false false]) ]
--                               → [ 123 "foo" 456 456 "foo" "xyzzy" "xyzzy" true ]
-- (the last element needs `with import ./lib.nix` — dropped)
def as5 : E := rcd [("x", rcd [("y", rcd [("z", int)])]), ("a", rcd [("b", rcd [("c", int)])])]
def bs5 : E := rcd [("f-o-o", rcd [("bar", str)])]
def attrs5 (e7 : Bool) : E := lets [("as", as5), ("bs", bs5)] (rcd (
  [ ("e1", sels (v "as") ["x", "y", "z"])
  , ("e2", orE (v "as") ["foo"] str)
  , ("e3", orE (v "as") ["x", "y", "bla"] (sels (v "as") ["a", "b", "c"]))
  , ("e4", orE (v "as") ["a", "b", "c"] (sels (v "as") ["x", "y", "z"]))
  , ("e5", orE (v "as") ["x", "y", "bla"] (orE (v "bs") ["f-o-o", "bar"] str))
  , ("e6", orE (v "as") ["x", "y", "bla"] (orE (v "bs") ["bar", "foo"] str)) ]
  ++ if e7 then [("e7", orE int ["bla"] (orE null ["foo"] str))] else []))
#guard run (attrs5 false)
  = "{e1: int | e2: str | e3: int | e4: int | e5: str | e6: str}"
-- `or` on a non-record: Nix falls through to the default
#guard run (attrs5 true)
  = "fail: clash"

--------------------------------------------------------- dynamic-attrs-2 -------
-- { a."${"b"}" = true; a."${"c"}" = false; }.a.b                         → true
#guard run (sels (rcd [("a", rcd [("b", bool), ("c", bool)])]) ["a", "b"])
  = "bool"
-- …with the keys as first-class labels
#guard run (sels (rcd [("a", .cat (.rcdDyn (.lab "b") bool) (.rcdDyn (.lab "c") bool))]) ["a", "b"])
  = "bool"

------------------------------------------------------- dynamic-attrs-bare ------
-- let aString = "a"; bString = "b"; in {
--   hasAttrs     = { a.b = null; } ? ${aString}.b;
--   selectAttrs  = { a.b = true; }.a.${bString};
--   selectOrAttrs = { }.${aString} or true;
--   binds        = { ${aString}."${bString}c" = true; }.a.bc;
--   recBinds     = rec { ${bString} = a; a = true; }.b;
--   multiAttrs   = { ${aString} = true; ${bString} = false; }.a; }   → all true
def dynBare (binds : Bool) : E := lets [("aString", .lab "a"), ("bString", .lab "b")] (rcd (
  [ ("hasAttrs", tup [rcd [("a", rcd [("b", null)])], v "aString"])
  , ("selectAttrs", .selDyn (.sel (rcd [("a", rcd [("b", bool)])]) "a") (v "bString"))
  , ("selectOrAttrs", .selDyn (.cat (rcd []) (.rcdDyn (v "aString") bool)) (v "aString"))
  , ("recBinds", .sel (lets [("a", bool)] (.cat (.rcdDyn (v "bString") (v "a")) (rcd [("a", v "a")]))) "b")
  , ("multiAttrs", .sel (.cat (.rcdDyn (v "aString") bool) (.rcdDyn (v "bString") bool)) "a") ]
  -- "${bString}c" builds a label by interpolation. A tuple is a record, not a
  -- label, so here the opaque operator is a λ-bound hole `h` instead
  ++ if binds then [("binds", sels (.rcdDyn (v "aString") (.rcdDyn (.app (v "h") (v "bString")) bool)) ["a", "bc"])]
     else []))
#guard run (dynBare false)
  = "{hasAttrs: {_1: {a: {b: null}} | _2: ⌊a⌋} | selectAttrs: bool | selectOrAttrs: bool | recBinds: bool | multiAttrs: bool}"
-- the key is unknown: ★
#guard run (.lam "h" (dynBare true))
  = "((⌊b⌋ → ⌊k20⌋) → {hasAttrs: {_1: {a: {b: null}} | _2: ⌊a⌋} | selectAttrs: bool | selectOrAttrs: bool | recBinds: bool | multiAttrs: bool | binds: ★})"
-- …and refines once the hole computes the label
#guard run (.app (.lam "h" (dynBare true)) (.lam "z" (.lab "bc")))
  = "{hasAttrs: {_1: {a: {b: null}} | _2: ⌊a⌋} | selectAttrs: bool | selectOrAttrs: bool | recBinds: bool | multiAttrs: bool | binds: bool}"

------------------------------------------------------------------ scope --------
-- scope-1: (({x}: x: { x = 1; y = x; }) {x = 2;} 3).y                       → 3
#guard run (.sel (app (pat "s" [("x", none)] (.lam "x" (rcd [("x", int), ("y", v "x")])))
  [rcd [("x", int)], int]) "y")
  = "int"
-- scope-2: ((x: {x}: rec { x = 1; y = x; }) 2 {x = 3;}).y                  → 1
#guard run (.sel (app (.lam "x" (pat "s" [("x", none)]
    (lets [("x", int)] (rcd [("x", v "x"), ("y", v "x")]))))
  [int, rcd [("x", int)]]) "y")
  = "int"
-- scope-3: ((x: as: {x}: rec { inherit (as) x; y = x; }) 2 {x = 4;} {x = 3;}).y → 4
#guard run (.sel (app (lam ["x", "as"] (pat "s" [("x", none)]
    (lets [("x", .sel (v "as") "x")] (rcd [("x", v "x"), ("y", v "x")]))))
  [int, rcd [("x", int)], rcd [("x", int)]]) "y")
  = "int"
-- scope-4/6: f = {x ? y, y ? x}: x + y  — mutually recursive defaults, skipped
-- scope-7: rec { inherit (x) y; x = { y = 1; }; }.y                        → 1
#guard run (.sel (lets [("x", rcd [("y", int)]), ("y", .sel (v "x") "y")]
  (rcd [("y", v "y"), ("x", v "x")])) "y")
  = "int"

-------------------------------------------------------------- let / new-let ----
-- let { x = "foo"; y = "bar"; body = x + y; }                         → "foobar"
#guard run (lets [("x", str), ("y", str)] (tup [v "x", v "y"]))
  = "{_1: str | _2: str}"
-- let f = z: let x = "foo"; y = "bar"; body = 1; in z + x + y;
--     arg = "xyzzy"; in f arg                                    → "xyzzyfoobar"
#guard run (lets [("f", .lam "z" (lets [("x", str), ("y", str), ("body", int)]
    (tup [tup [v "z", v "x"], v "y"]))), ("arg", str)] (.app (v "f") (v "arg")))
  = "{_1: {_1: str | _2: str} | _2: str}"

--------------------------------------------------------------- patterns --------
-- f = args@{x, y, z}: x + args.y + z;   g = {x, y, z}@args: f args;
-- h = {x ? "d", y ? x, z ? args.x}@args: x + y + z;
-- j = {x, y, z, ...}: x + y + z;
-- in f {x="a";y="b";z="c";} + g {…} + h {x = "D";} + h {x="D";y="E";z="F";}
--  + j {x="i";y="j";z="k";bla="bla";foo="bar";}           → "abcxyzDDDDEFijk"
def xyz : List (String × Option E) := [("x", none), ("y", none), ("z", none)]
def s3 : E := rcd [("x", str), ("y", str), ("z", str)]
def patterns : E := lets
  [ ("f", pat "args" xyz (tup [tup [v "x", .sel (v "args") "y"], v "z"]))
  , ("g", pat "args" xyz (.app (v "f") (v "args")))
  , ("h", pat "args" [("x", some str), ("y", some (v "x")), ("z", some (.sel (v "args") "x"))]
          (tup [tup [v "x", v "y"], v "z"]))
  , ("j", pat "args" xyz (tup [tup [v "x", v "y"], v "z"])) ]
  (rcd [ ("f", .app (v "f") s3), ("g", .app (v "g") s3)
       , ("h1", .app (v "h") (rcd [("x", str)])), ("h2", .app (v "h") s3)
       , ("j", .app (v "j") (rcd [("x", str), ("y", str), ("z", str), ("bla", str), ("foo", str)])) ])
#guard run patterns
  = "{f: {_1: {_1: str | _2: str} | _2: str} | g: {_1: {_1: str | _2: str} | _2: str} | h1: {_1: {_1: str | _2: str} | _2: str} | h2: {_1: {_1: str | _2: str} | _2: str} | j: {_1: {_1: str | _2: str} | _2: str}}"

------------------------------------------------------------- empty-args --------
-- ({}: {x,y,}: "${x}${y}") {} {x = "a"; y = "b";}                           → "ab"
#guard run (app (pat "e" [] (pat "s" [("x", none), ("y", none)] (tup [v "x", v "y"])))
  [rcd [], rcd [("x", str), ("y", str)]])
  = "{_1: str | _2: str}"

----------------------------------------------------------- inherit-from --------
-- let inherit (builtins.trace "used" { a = 1; b = 2; }) a b; x.c = 3; y.d = 4;
--     merged = { inner = { inherit (y) d; }; inner = { inherit (x) c; }; };
-- in [ a b rec { x.c = []; inherit (x) c; inherit (y) d; __overrides.y.d = []; } merged ]
-- (the `rec` with `__overrides` is dropped; `trace` is the identity)
#guard run (lets [ ("t", rcd [("a", int), ("b", int)]), ("a", .sel (v "t") "a"), ("b", .sel (v "t") "b")
                , ("x", rcd [("c", int)]), ("y", rcd [("d", int)])
                , ("merged", rcd [("inner", rcd [("d", .sel (v "y") "d"), ("c", .sel (v "x") "c")])]) ]
  (tup [v "a", v "b", v "merged"]))
  = "{_1: int | _2: int | _3: {inner: {d: int | c: int}}}"

-- callable-attrs (__functor), overrides (__overrides), attrs6 (__overrides):
-- magic attributes, not modelled.

end NixSuite
end MinimalCalculus
