-- `runF` ON PROGRAMS. Executable regressions for the inference function: each
-- `#guard` pins what a run answers, read under its final substitution. Every
-- `ok` here is a declarative typing by `runF_typed`.

import InferFnTerm

namespace MinimalCalculus
namespace InferRuns

mutual
def tyS : Ty Unit → String
  | .var α    => "t" ++ toString α.length
  | .base _   => "𝓫"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyS τ₁ ++ " → " ++ tyS τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowS ρ ++ "}"
def rowS : Row Unit → String
  | .empty     => "ε"
  | .var α     => "r" ++ toString α.length
  | .sing l τ  => l ++ ": " ++ tyS τ
  | .cat ρ₁ ρ₂ => rowS ρ₁ ++ " | " ++ rowS ρ₂
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

-- the spent promise (`spentEx_declarative`): typeable, but no run finalizes it
#guard run (.lam "x" (.lam "y" (.app (.sel (v "x") "l") (v "y"))))
  = "fail: spent promise: a stump's result is no longer a variable"

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

end InferRuns
end MinimalCalculus
