-- `runF` ON THE NIX TEST-SUITE. Hand translations of `testdata/eval-okay-*.nix`
-- into the calculus, run and compared against Nix's `.exp`.
--
-- Run with  `lake env lean NixSuite.lean`  (not in the library globs).
--
-- ## Translation
-- - `a // b`            → `a ‖ b`              (both: right wins; E-conc / T-conc
--                                               put the right operand first)
-- - `e.p₁.….pₙ or d`    → `({pₙ = d} ‖ … ({p₁ = {}} ‖ e).p₁ …).pₙ`
--                         (defaults on the LEFT, so e wins where it has the field)
-- - `a.b.c = v`         → nested records, siblings merged
-- - `"${"s"}"` as a key  → the label `s`;  a string bound to a variable and used
--                         as a key → `lab s` and `selDyn` / `rcdDyn`
-- - `{x, y ? d}@s: e`   → `λs. let x = s.x in let y = ({y = d} ‖ s).y in e`
--                         (open and closed patterns coincide: rows are open)
-- - non-recursive `rec {…}` / `let {… body}` → nested `let`s, then the record
-- - MacroNix (Builtins.lean): `if`, `assert`, `e ? l`, list literals, `++`,
--   `+ - * /`, `== != < …`, `! && || ->`, string `+` and interpolation
--   (`strcat`), `toString`, `map`, `filter`, `foldl'`, `elemAt`, `head`
-- - still OPAQUE (a tuple of the operands): `? ${dynamic}.path`, and a
--   heterogeneous list where the comparison with `[ ]` is wanted
-- Constants: `int`, `string`, `null`; booleans are literals.
--
-- ## Results                                    runF                .exp    agrees
-- attrs               whole program             clash               987     ✗ (1)
--                     (the taken branch)        int                 987     ✓
-- attrs3              if + interpolation        string              str     ✓
-- attrs5 e1–e6        or-chains, incl. a        int string int int  ✓       ✓
--                     missing intermediate      string string
-- attrs5 e7           `(123).bla or …`          clash               "xyzzy" ✗ (2)
-- dynamic-attrs-2     literal / ⌊l⌋ keys        bool / bool         true    ✓
-- dynamic-attrs-bare  5 of 6 fields             bool (hasAttrs opaque) true  ✓
--                     binds, open hole          ★                   true    ✓ (3)
--                     binds, hole := `bc`       bool                true    ✓
-- scope-1/2/3/7       shadowing, rec, inherit   int ×4              3 1 4 1 ✓
-- let, new-let        string `+`                string              str     ✓
-- patterns            @-patterns, defaults,     clash               str     ✗ (5)
--                     h at two row shapes
-- empty-args          `{}:` pattern             string              str     ✓
-- inherit-from        [ a b merged ]            clash               list    ✗ (4)
--                     (as a tuple)              {_1: int | …}       —       ✓
-- if                  != , nested if            int                 3       ✓
-- concat              ++ ++                     [int]               list    ✓
-- logic               assert ! && || ->         int                 1       ✓
-- map                 map, foldl' (+) ""        string              str     ✓
-- filter              filter, / * ==            [int]               list    ✓
-- foldlStrict         foldl' (+) 0              int                 500500  ✓
-- elem (elemAt part)  elemAt                    int                 30      ✓
-- skipped: scope-4/6 (mutually recursive defaults), callable-attrs (__functor),
--          overrides / attrs6 (__overrides), `with import ./lib.nix` (range,
--          elem); lib calls unrolled by hand where the rest is first-order
--
-- (1) `as.a` is a definite miss (★ by T-sel-⊥), `as.z` is int, and `if`
--     unifies its branches: ★ is rigid, so they clash. Declaratively typable
--     (T-★-intro on the else branch). Needs occurrence typing on `as ? a`
--     (Negative Information S3) or a join at ★ (plans/macronix.md R2).
-- (2) Nix's `or` also catches selection on a NON-record; `‖` with an `int`
--     clashes. Encoding limit of `or`, or a missing rule.
-- (3) A label computed by interpolation needs a λ-hole (interpolation yields
--     a string, not a label): the key is unknown, so the field reads at ★,
--     refined to `bool` once the hole computes `bc`.
-- (4) Lists are homogeneous; the element types int and {inner: …} clash.
--     Declaratively `[★]` via T-★-intro (R2).
-- (5) `h = {x ? "foo", y ? x, z ? args.x}@args: x + y + z`, called at `{x = …}`
--     and at s3. Until 2026-10-27 `upd`/`orE`/`pat` put the default on the
--     WINNING side of ‖, so every default lookup hit at once and this row read
--     ✓. With the default on the losing side, the lookups block on the row of
--     args and park as stumps. One call of h types; two calls clash, so A-let
--     does not generalize the row of args. Minimal trigger: a spent lookup of x
--     next to `z ? args.x` (h with only one of `x ? …` / `z ? args.x`, or with
--     an unused x, types). Presumably `LetSpent` (Infer.lean:819: stumps on one
--     blocker with one label must be spent alike), not confirmed. Incompleteness
--     of A-let, not a type error.

import InferFnTerm
import Builtins

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
  | .list τ   => "[" ++ tyS τ ++ "]"
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

open MacroNix (sig Op MConst call)

abbrev E := Expr MConst
def run (e : E) : String := verdict (runF sig 1000 e)

def v (x : String) : E := .var x
def int : E := .con (.int 0)
def str : E := .con (.str "")
def tt : E := .bool true
def ff : E := .bool false
def null : E := .con .null

def body : List (String × E) → RecBody E
  | []           => .empty
  | [(l, e)]     => .field l e
  | (l, e) :: bs => .cat (.field l e) (body bs)
def rcd (bs : List (String × E)) : E := .rcd (body bs)
/-- the opaque tuple, still needed where MacroNix has no construct -/
def tup (es : List E) : E :=
  rcd ((es.zipIdx 1).map fun (e, i) => ("_" ++ toString i, e))
/-- `[e₁ … eₙ]` -/
def lst (es : List E) : E := es.foldr .cons .nil

def lam (xs : List String) (e : E) : E := xs.foldr .lam e
def app (f : E) (as : List E) : E := as.foldl .app f
def lets (bs : List (String × E)) (e : E) : E := bs.foldr (fun (x, e₁) e₂ => .letE x e₁ e₂) e
def sels (e : E) (ls : List String) : E := ls.foldl .sel e
/-- `a // b` -/
def upd (a b : E) : E := .cat a b
/-- `e.p₁.….pₙ or d` -/
def orE (e : E) : List String → E → E
  | [],      _ => e
  | [l],     d => .sel (.cat (rcd [(l, d)]) e) l
  | l :: ls, d => orE (.sel (.cat (rcd [(l, rcd [])]) e) l) ls d
/-- `{x, y ? d, …}@s: e` -/
def pat (s : String) (fs : List (String × Option E)) (e : E) : E :=
  .lam s (lets (fs.map fun
    | (x, none)   => (x, .sel (v s) x)
    | (x, some d) => (x, .sel (.cat (rcd [(x, d)]) (v s)) x)) e)
/-- string `+` and `"${a}${b}"` -/
def cat (es : List E) : E := match es with
  | [] => str
  | e :: es => es.foldl (fun acc x => call .strcat [acc, x]) e
def toStr (e : E) : E := call .toString [e]
def add (a b : E) : E := call .add [a, b]
def eq (a b : E) : E := call .eq [a, b]
def not (a : E) : E := call .not [a]
def and (a b : E) : E := .ite a b ff
def or (a b : E) : E := .ite a tt b
def impl (a b : E) : E := or (not a) b
def append (a b : E) : E := call .concat [a, b]

---------------------------------------------------------------- attrs ---------
-- let { as = { x = 123; y = 456; } // { z = 789; } // { z = 987; };
--       body = if as ? a then as.a else assert as ? z; as.z; }          → 987
def asAttrs : E := upd (rcd [("x", int), ("y", int)]) (upd (rcd [("z", int)]) (rcd [("z", int)]))
-- `as.a` is a definite miss (★, T-sel-⊥), `as.z` is int: the branches clash
#guard run (lets [("as", asAttrs)] (.ite (.has (v "as") "a") (.sel (v "as") "a")
  (.asrt (.has (v "as") "z") (.sel (v "as") "z"))))
  = "fail: clash"
-- the branch Nix takes
#guard run (lets [("as", asAttrs)] (.asrt (.has (v "as") "z") (.sel (v "as") "z")))
  = "int"

---------------------------------------------------------------- attrs3 --------
-- in if config.services.sshd.enable
--    then "foo ${toString config.services.sshd.port} ${toString config.services.httpd.port} ${config.hostName}"
--       + "${config.a.b.c.….z}" + "${config.foo.a}" + "${config.foo.b.c}"
--    else "bar"
def deep : List String := "abcdefghijklmnopqrstuvwxyz".toList.map toString
def config : E := rcd
  [ ("services", rcd [ ("sshd", rcd [("enable", tt), ("port", int)])
                     , ("httpd", rcd [("port", int)]) ])
  , ("hostName", str)
  , ("a", deep.tail.foldr (fun l e => rcd [(l, e)]) str)
  , ("foo", rcd [("a", str), ("b", rcd [("c", str)])]) ]
#guard run (lets [("config", config)] (.ite (sels (v "config") ["services", "sshd", "enable"])
  (cat [ str, toStr (sels (v "config") ["services", "sshd", "port"]), str
       , toStr (sels (v "config") ["services", "httpd", "port"]), str
       , sels (v "config") ["hostName"]
       , sels (v "config") deep, sels (v "config") ["foo", "a"]
       , sels (v "config") ["foo", "b", "c"] ])
  str))
  = "string"

---------------------------------------------------------------- attrs5 --------
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
  = "{e1: int | e2: string | e3: int | e4: int | e5: string | e6: string}"
#guard run (attrs5 true)
  = "fail: clash"

--------------------------------------------------------- dynamic-attrs-2 -------
#guard run (sels (rcd [("a", rcd [("b", tt), ("c", ff)])]) ["a", "b"])
  = "bool"
#guard run (sels (rcd [("a", .cat (.rcdDyn (.lab "b") tt) (.rcdDyn (.lab "c") ff))]) ["a", "b"])
  = "bool"

------------------------------------------------------- dynamic-attrs-bare ------
-- `? ${aString}.b` asks a DYNAMIC path: MacroNix's `?` takes a literal label,
-- so that operand pair stays a tuple
def dynBare (binds : Bool) : E := lets [("aString", .lab "a"), ("bString", .lab "b")] (rcd (
  [ ("hasAttrs", tup [rcd [("a", rcd [("b", null)])], v "aString"])
  , ("selectAttrs", .selDyn (.sel (rcd [("a", rcd [("b", tt)])]) "a") (v "bString"))
  , ("selectOrAttrs", .selDyn (.cat (rcd []) (.rcdDyn (v "aString") tt)) (v "aString"))
  , ("recBinds", .sel (lets [("a", tt)] (.cat (.rcdDyn (v "bString") (v "a")) (rcd [("a", v "a")]))) "b")
  , ("multiAttrs", .sel (.cat (.rcdDyn (v "aString") tt) (.rcdDyn (v "bString") ff)) "a") ]
  ++ if binds then [("binds", sels (.rcdDyn (v "aString") (.rcdDyn (.app (v "h") (v "bString")) tt)) ["a", "bc"])]
     else []))
#guard run (dynBare false)
  = "{hasAttrs: {_1: {a: {b: null}} | _2: ⌊a⌋} | selectAttrs: bool | selectOrAttrs: bool | recBinds: bool | multiAttrs: bool}"
#guard run (.lam "h" (dynBare true))
  = "((⌊b⌋ → ⌊k20⌋) → {hasAttrs: {_1: {a: {b: null}} | _2: ⌊a⌋} | selectAttrs: bool | selectOrAttrs: bool | recBinds: bool | multiAttrs: bool | binds: ★})"
#guard run (.app (.lam "h" (dynBare true)) (.lam "z" (.lab "bc")))
  = "{hasAttrs: {_1: {a: {b: null}} | _2: ⌊a⌋} | selectAttrs: bool | selectOrAttrs: bool | recBinds: bool | multiAttrs: bool | binds: bool}"

------------------------------------------------------------------ scope --------
#guard run (.sel (app (pat "s" [("x", none)] (.lam "x" (rcd [("x", int), ("y", v "x")])))
  [rcd [("x", int)], int]) "y")
  = "int"
#guard run (.sel (app (.lam "x" (pat "s" [("x", none)]
    (lets [("x", int)] (rcd [("x", v "x"), ("y", v "x")]))))
  [int, rcd [("x", int)]]) "y")
  = "int"
#guard run (.sel (app (lam ["x", "as"] (pat "s" [("x", none)]
    (lets [("x", .sel (v "as") "x")] (rcd [("x", v "x"), ("y", v "x")]))))
  [int, rcd [("x", int)], rcd [("x", int)]]) "y")
  = "int"
#guard run (.sel (lets [("x", rcd [("y", int)]), ("y", .sel (v "x") "y")]
  (rcd [("y", v "y"), ("x", v "x")])) "y")
  = "int"

-------------------------------------------------------------- let / new-let ----
-- let { x = "foo"; y = "bar"; body = x + y; }                         → "foobar"
#guard run (lets [("x", str), ("y", str)] (cat [v "x", v "y"]))
  = "string"
-- let f = z: let x = "foo"; y = "bar"; body = 1; in z + x + y; arg = "xyzzy"; in f arg
#guard run (lets [("f", .lam "z" (lets [("x", str), ("y", str), ("body", int)]
    (cat [v "z", v "x", v "y"]))), ("arg", str)] (.app (v "f") (v "arg")))
  = "string"

--------------------------------------------------------------- patterns --------
def xyz : List (String × Option E) := [("x", none), ("y", none), ("z", none)]
def s3 : E := rcd [("x", str), ("y", str), ("z", str)]
def patterns : E := lets
  [ ("f", pat "args" xyz (cat [v "x", .sel (v "args") "y", v "z"]))
  , ("g", pat "args" xyz (.app (v "f") (v "args")))
  , ("h", pat "args" [("x", some str), ("y", some (v "x")), ("z", some (.sel (v "args") "x"))]
          (cat [v "x", v "y", v "z"]))
  , ("j", pat "args" xyz (cat [v "x", v "y", v "z"])) ]
  (cat [ .app (v "f") s3, .app (v "g") s3
       , .app (v "h") (rcd [("x", str)]), .app (v "h") s3
       , .app (v "j") (rcd [("x", str), ("y", str), ("z", str), ("bla", str), ("foo", str)]) ])
#guard run patterns
  = "fail: clash"

------------------------------------------------------------- empty-args --------
#guard run (app (pat "e" [] (pat "s" [("x", none), ("y", none)] (cat [v "x", v "y"])))
  [rcd [], rcd [("x", str), ("y", str)]])
  = "string"

----------------------------------------------------------- inherit-from --------
-- in [ a b merged ]: a HETEROGENEOUS list (int, int, record) — clashes (R2)
def inheritFrom (asList : Bool) : E :=
  lets [ ("t", rcd [("a", int), ("b", int)]), ("a", .sel (v "t") "a"), ("b", .sel (v "t") "b")
       , ("x", rcd [("c", int)]), ("y", rcd [("d", int)])
       , ("merged", rcd [("inner", rcd [("d", .sel (v "y") "d"), ("c", .sel (v "x") "c")])]) ]
  (if asList then lst [v "a", v "b", v "merged"] else tup [v "a", v "b", v "merged"])
#guard run (inheritFrom true)
  = "fail: clash"
#guard run (inheritFrom false)
  = "{_1: int | _2: int | _3: {inner: {d: int | c: int}}}"

------------------------------------------------------- new: if, lists, logic ---
-- eval-okay-if:  if "foo" != "f" + "oo" then 1 else if false then 2 else 3    → 3
#guard run (.ite (not (eq str (cat [str, str]))) int (.ite ff int int))
  = "int"
-- eval-okay-concat:  [1 2 3] ++ [4 5 6] ++ [7 8 9]                     → [ 1 … 9 ]
#guard run (append (lst [int, int, int]) (append (lst [int, int, int]) (lst [int, int, int])))
  = "[int]"
-- eval-okay-logic:  assert !false && (true || false) -> true; 1                → 1
#guard run (.asrt (impl (and (not ff) (or tt ff)) tt) int)
  = "int"
-- eval-okay-map:  concat (map (x: x + "bar") [ "foo" "bla" "xyzzy" ])   (lib's concat = foldl' (+) "")
#guard run (call .foldl [lam ["a", "b"] (cat [v "a", v "b"]), str,
  call .map [.lam "x" (cat [v "x", str]), lst [str, str, str]]])
  = "string"
-- eval-okay-filter:  builtins.filter (x: x / 2 * 2 == x) [ 0 … ]   (range unrolled)
#guard run (call .filter [.lam "x" (eq (call .mul [call .div [v "x", int], int]) (v "x")),
  lst [int, int, int]])
  = "[int]"
-- eval-okay-foldlStrict:  builtins.foldl' (x: y: x + y) 0 [ 1 … ]   (range unrolled)
#guard run (call .foldl [lam ["x", "y"] (add (v "x") (v "y")), int, lst [int, int, int]])
  = "int"
-- eval-okay-elem (elemAt part):  builtins.elemAt xs 20
#guard run (call .elemAt [lst [int, int], int])
  = "int"
-- a builtin is a FUNCTION only eta-expanded: `map head`
#guard run (.lam "xss" (call .map [.lam "xs" (call .head [v "xs"]), v "xss"]))
  = "([[t6]] → [t6])"

end NixSuite
end MinimalCalculus
