-- `runF` ON PROGRAMS. Executable regressions for the inference function: each
-- `#guard` pins what a run answers, read under its final substitution. Every
-- `ok` here is a declarative typing by `runF_typed`.

import InferFnTerm

namespace MinimalCalculus
namespace InferRuns

def keyS : Key → String
  | .lit l => l
  | .var α => "k" ++ toString α.length

mutual
def tyS : Ty Unit → String
  | .var α    => "t" ++ toString α.length
  | .base _   => "𝓫"
  | .lab k    => "⌊" ++ keyS k ++ "⌋"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyS τ₁ ++ " → " ++ tyS τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowS ρ ++ "}"
def rowS : Row Unit → String
  | .empty     => "ε"
  | .var α     => "r" ++ toString α.length
  | .sing l τ  => l ++ ": " ++ tyS τ
  | .cat ρ₁ ρ₂ => rowS ρ₁ ++ " | " ++ rowS ρ₂
  | .dsing q τ => "${" ++ keyS q ++ "}: " ++ tyS τ
end

def verdict : IRes (Ty Unit × SolverState Unit) → String
  | .ok (τ, S) => tyS (τ.applySubst S.subst)
  | .fail m    => "fail: " ++ m
  | .oof       => "out of fuel"

abbrev E := Expr Unit
def v (x : String) : E := .var x
def c : E := .con ()
def rec1 (l : String) (e : E) : E := .rcd (.field l e)
def rec2 (l₁ : String) (e₁ : E) (l₂ : String) (e₂ : E) : E :=
  .rcd (.cat (.field l₁ e₁) (.field l₂ e₂))

def run (e : E) : String := verdict (runF (fun _ => ()) 50 e)

-- λx. x.l — the stump finalizes: the lookup stays `?`, the result is ★
#guard run (.lam "x" (.sel (v "x") "l")) = "({r2} → ★)"
-- …and applied, the promise is kept: found at 𝓫, absent at ★
#guard run (.app (.lam "x" (.sel (v "x") "l")) (rec1 "l" c)) = "𝓫"
#guard run (.app (.lam "x" (.sel (v "x") "l")) (.rcd .empty)) = "★"

-- refinement: ({l = c} ‖ x).l cannot look past x …
#guard run (.lam "x" (.sel (.cat (rec1 "l" c) (v "x")) "l")) = "({r3} → ★)"
-- … until x is instantiated
#guard run (.app (.lam "x" (.sel (.cat (rec1 "l" c) (v "x")) "l")) (.rcd .empty)) = "𝓫"

-- let-polymorphism with stumps: one selector, two record shapes
#guard run (.letE "g" (.lam "x" (.sel (v "x") "l"))
    (rec2 "a" (.app (v "g") (rec1 "l" c)) "b" (.app (v "g") (.rcd .empty)))) = "{a: 𝓫 | b: ★}"
#guard run (.letE "i" (.lam "x" (v "x"))
    (rec2 "a" (.app (v "i") c) "b" (.app (v "i") (.rcd .empty)))) = "{a: 𝓫 | b: {ε}}"

-- Γ-freshness (`runSound_false_unguarded_let`): the alias is NOT generalized
#guard run (.lam "y" (.letE "z" (v "y") (v "z"))) = "(t1 → t1)"

-- the Perm split: e₁ parks the generalizable stump (x's) BEFORE a Γ-stump
-- (w's); a prefix split could not generalize it, so f would be monomorphic
#guard run (.lam "w" (.letE "f"
    (.sel (rec2 "b" (.lam "x" (.sel (v "x") "l")) "a" (.sel (v "w") "m")) "b")
    (rec2 "p" (.app (v "f") (rec1 "l" c)) "q" (.app (v "f") (.rcd .empty)))))
  = "({r5} → {p: 𝓫 | q: ★})"

-- the spent promise (`spentEx_declarative`): A-app writes δ ≔ t2 → t5, so F-★
-- cannot fire — MATERIALIZATION extends x's row with the field instead
-- (`Materialize`). The trailing ε is the unifier's spine form, ≈-invisible.
#guard run (.lam "x" (.lam "y" (.app (.sel (v "x") "l") (v "y"))))
  = "({l: (t2 → t5) | r6 | ε} → (t2 → t5))"
-- …the materialized row stays open: a second, unspent selection on it is F-★'s
#guard run (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.sel (v "x") "m")))
  = "({l: (𝓫 → t4) | r7 | ε | ε} → {p: t4 | q: ★})"
-- …and a second selection of the SAME field wakes and hits what was materialized
#guard run (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.app (.sel (v "x") "l") c)))
  = "({l: (𝓫 → t7) | r8 | ε | ε} → {p: t7 | q: t7})"

-- a spent promise under a let is GENERALIZED: f stays polymorphic in x's row
-- (it used to be forced monomorphic, and the second use clashed)
#guard run (.letE "f" (.lam "x" (.lam "y" (.app (.sel (v "x") "l") (v "y"))))
    (rec2 "a" (.app (.app (v "f") (rec1 "l" (.lam "z" (v "z")))) c)
          "b" (.app (.app (v "f") (rec2 "l" (.lam "z" (v "z")) "m" c)) c)))
  = "{a: 𝓫 | b: 𝓫}"
-- …two spent selections of different fields on one row generalize together
#guard run (.letE "f" (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.app (.sel (v "x") "m") c)))
    (.app (v "f") (rec2 "l" (.lam "z" (v "z")) "m" (.lam "z" (.rcd .empty)))))
  = "{p: 𝓫 | q: {ε}}"
-- …and an instance at a row WITHOUT the field has no typing: ⊥ wants ★, the
-- promise was spent on an arrow (declaratively: ★ cannot be applied)
#guard run (.letE "f" (.lam "x" (.lam "y" (.app (.sel (v "x") "l") (v "y"))))
    (.app (.app (v "f") (rec1 "m" c)) c))
  = "fail: clash"

-- D-HIT UP TO ≈ (`analysis/let-review.md` §4.4). With no χ-correction, A-let
-- dropped linear-pattern, nodup, independence and disjoint results; these used
-- to be monomorphic.
-- nested selection: the second stump's row is the first one's result
#guard run (.letE "g" (.lam "x" (.sel (.sel (v "x") "l") "m"))
    (rec2 "a" (.app (v "g") (rec1 "l" (rec1 "m" c))) "b" (.app (v "g") (rec1 "l" (.rcd .empty)))))
  = "{a: 𝓫 | b: ★}"
-- a record literal in a spent result ({a: 𝓫} → β is no linear pattern)
#guard run (.letE "f" (.lam "x" (.app (.sel (v "x") "l") (rec1 "a" c)))
    (rec2 "a" (.app (v "f") (rec1 "l" (.lam "y" (v "y")))) "b" (.app (v "f") (rec1 "l" (.lam "y" c)))))
  = "{a: {a: 𝓫} | b: 𝓫}"
-- a spent result mentioning an unspent one (x.m's result is x.l's argument)
#guard run (.letE "f" (.lam "x" (.app (.sel (v "x") "l") (.sel (v "x") "m")))
    (rec2 "a" (.app (v "f") (rec2 "l" (.lam "y" (v "y")) "m" c))
          "b" (.app (v "f") (rec2 "l" (.lam "y" c) "m" (.rcd .empty)))))
  = "{a: 𝓫 | b: 𝓫}"

-- an instance's stump whose RESULT was aliased (found by this function,
-- 2026-09-26, then fixed): in `h = λy. g y`, A-app emits δ ≐ β and the unifier
-- binds δ ≔ β. A-let used to want δ itself unsolved, so h stayed monomorphic and
-- the second use clashed; it now generalizes the stump at β, the variable δ
-- reads as at S₁ (`SolverState.resVar`, `LetResults`)
#guard run (.letE "g" (.lam "x" (.sel (v "x") "l"))
    (.letE "h" (.lam "y" (.app (v "g") (v "y")))
      (rec2 "a" (.app (v "h") (rec1 "l" c)) "b" (.app (v "h") (.rcd .empty)))))
  = "{a: 𝓫 | b: ★}"

-- re-binding an instance: h's type consists ONLY of names A-var drew, so it is
-- generalized only because A-var now records their kinds (before, `Assigns`
-- failed and h was monomorphic)
#guard run (.letE "i" (.lam "x" (v "x")) (.letE "h" (v "i")
    (rec2 "a" (.app (v "h") c) "b" (.app (v "h") (.rcd .empty))))) = "{a: 𝓫 | b: {ε}}"
-- …and an instance's stump is re-generalized with it
#guard run (.letE "g" (.lam "x" (.sel (v "x") "l")) (.letE "h" (v "g")
    (rec2 "a" (.app (v "h") (rec1 "l" c)) "b" (.app (v "h") (.rcd .empty))))) = "{a: 𝓫 | b: ★}"

-- FC-LABELS (`plans/fc-labels-plan.md`, phase A). The key of a selection is a
-- value of the label sort; its TYPE ⌊k⌋ keys the lookup, k a literal or a label
-- variable (phase B: keys got their own sort, so a non-label key clashes).
def L (l : String) : E := .lab l
def sd (e₁ e₂ : E) : E := .selDyn e₁ e₂

-- the headline λa. λx. x.(a): a stump blocked on the record's row, keyed by a
-- label variable; at the top level it finalizes at ★ …
#guard run (.lam "a" (.lam "x" (sd (v "x") (v "a")))) = "(⌊k4⌋ → ({r3} → ★))"
-- … and let-bound it is a qualified scheme, used at two keys: found, and absent
#guard run (.letE "get" (.lam "a" (.lam "x" (sd (v "x") (v "a"))))
    (rec2 "p" (.app (.app (v "get") (L "foo")) (rec1 "foo" c))
          "q" (.app (.app (v "get") (L "bar")) (rec1 "foo" c)))) = "{p: 𝓫 | q: ★}"
#guard run (.app (.app (.lam "a" (.lam "x" (sd (v "x") (v "a")))) (L "foo")) (rec1 "foo" c))
  = "𝓫"

-- LABEL REFINEMENT: the record is literal, only the key is unknown, so the stump
-- is blocked on the KEY (L-?-lab); applying the function solves the key, and
-- wake-up — which judges staleness by the keyed lookup — finds the field
#guard run (.lam "a" (sd (rec2 "foo" c "bar" (.rcd .empty)) (v "a"))) = "(⌊k3⌋ → ★)"
#guard run (.app (.lam "a" (sd (rec2 "foo" c "bar" (.rcd .empty)) (v "a"))) (L "foo"))
  = "𝓫"
#guard run (.app (.lam "a" (sd (rec2 "foo" c "bar" (.rcd .empty)) (v "a"))) (L "baz"))
  = "★"

-- a literal key through the dynamic door is the static selection …
#guard run (sd (rec1 "foo" c) (L "foo")) = "𝓫"
-- … and a key that is not a label is a type error (no junk keys since phase B)
#guard run (sd (rec1 "foo" c) c) = "fail: clash"

-- the spent promise through the second door: blocked on its KEY, so there is no
-- row to extend and materialization does not apply — still incomplete
#guard run (.lam "r" (.lam "a" (.app (sd (v "r") (v "a")) c)))
  = "fail: spent promise: a stump's result is no longer a variable"

-- a key that is itself an unresolved selection; the key's field is forced to a
-- label, so F-★ never touches it …
#guard run (.lam "r" (.lam "k" (sd (v "r") (.sel (v "k") "name"))))
  = "({r3} → ({name: ⌊k6⌋ | r8 | ε} → ★))"
-- … and under a let it is generalized (it used to be held back by the
-- independence premise's key clause: the key IS another stump's result)
#guard run (.letE "f" (.lam "r" (.lam "k" (sd (v "r") (.sel (v "k") "name")))) (v "f"))
  = "({r11} → ({name: ⌊k9⌋ | r12 | ε} → ★))"
-- no KeySafe: a key blocked on a row variable stays a label variable
#guard run (.lam "r" (rec1 "x" (sd (rec1 "x" c) (.sel (v "r") "a"))))
  = "({a: ⌊k5⌋ | r7 | ε} → {x: ★})"

-- DYNAMIC CONSTRUCTION (phase B). `{${a} = v}` keys its field by the key's
-- label variable; the headline infers exactly `rcdDynQ`
def rd (e₁ e₂ : E) : E := .rcdDyn e₁ e₂
def mk : E := .lam "a" (.lam "v" (rd (v "a") (v "v")))
#guard run mk = "(⌊k3⌋ → (t2 → {${k3}: t2}))"
-- a literal key makes it the static record, let-bound at two keys
#guard run (.sel (.app (.app mk (L "foo")) c) "foo") = "𝓫"
#guard run (.letE "mk" mk (rec2 "p" (.sel (.app (.app (v "mk") (L "foo")) c) "foo")
    "q" (.sel (.app (.app (v "mk") (L "bar")) (.rcd .empty)) "bar"))) = "{p: 𝓫 | q: {ε}}"
-- the same key variable finds its own field; a literal one is blocked on it
#guard run (.lam "a" (sd (rd (v "a") c) (v "a"))) = "(⌊k4⌋ → 𝓫)"
#guard run (.lam "a" (.sel (rd (v "a") c) "foo")) = "(⌊k2⌋ → ★)"
#guard run (.app (.lam "a" (.sel (rd (v "a") c) "foo")) (L "foo")) = "𝓫"
-- `r // { ${n} = v }`: the update, which shadows an old field of that name
def upd : E := .lam "n" (.lam "r" (.cat (v "r") (rd (v "n") c)))
#guard run upd = "(⌊k3⌋ → ({r4} → {${k3}: 𝓫 | ε | r4}))"
#guard run (.sel (.app (.app upd (L "x")) (rec1 "x" (.rcd .empty))) "x") = "𝓫"
-- two records under one unknown key flow into one function (U-key)
#guard run (.lam "a" (.lam "f" (rec2 "p" (.app (v "f") (rd (v "a") c))
    "q" (.app (v "f") (rd (v "a") c))))) = "(⌊k5⌋ → (({${k5}: 𝓫} → t6) → {p: t6 | q: t6}))"
-- a key that is not a label is a type error
#guard run (rd c c) = "fail: clash"
#guard run (.lam "a" (rd (.sel (v "a") "k") c)) = "({k: ⌊k4⌋ | r5 | ε} → {${k4}: 𝓫})"

end InferRuns
end MinimalCalculus
