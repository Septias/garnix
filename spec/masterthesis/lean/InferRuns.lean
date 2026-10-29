-- `runF` ON PROGRAMS. Executable regressions for the inference function: each
-- `#guard` pins the scheme a run answers. Every instance of an `ok` here is a
-- declarative typing, and there is one (`runF_typed`).

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
  | .list τ   => "[" ++ tyS τ ++ "]"
def rowS : Row Unit → String
  | .empty     => "ε"
  | .var α     => "r" ++ toString α.length
  | .sing l τ  => l ++ ": " ++ tyS τ
  | .cat ρ₁ ρ₂ => rowS ρ₁ ++ " | " ++ rowS ρ₂
  | .dsing q τ => "${" ++ keyS q ++ "}: " ++ tyS τ
end

abbrev E := Expr Unit
def v (x : String) : E := .var x
def c : E := .con ()
def rec1 (l : String) (e : E) : E := .rcd (.field l e)
def rec2 (l₁ : String) (e₁ : E) (l₂ : String) (e₂ : E) : E :=
  .rcd (.cat (.field l₁ e₁) (.field l₂ e₂))

-- a run's answer prints as `Q ⇒ τ`: at the top level every variable is bound,
-- so the ∀ is left implicit, and Q = ∅ prints as the plain type
def keyDS : Key → String
  | .lit l => l
  | .var α => "${k" ++ toString α.length ++ "}"

def stumpS (st : Stump Unit) : String :=
  "⟨" ++ rowS st.row ++ "." ++ keyDS st.label ++ " ↓ " ++ tyS st.res ++ "⟩"

def schemeS (σ : QScheme Unit) : String :=
  (if σ.constraints.isEmpty then "" else
     String.intercalate ", " (σ.constraints.map stumpS) ++ " ⇒ ") ++ tyS σ.body

def verdict : IRes (QScheme Unit × SolverState Unit) → String
  | .ok (σ, _) => schemeS σ
  | .fail m    => "fail: " ++ m
  | .oof       => "out of fuel"

def run (e : E) : String := verdict (runF (Sig.plain fun _ => ()) 50 e)

-- λx. x.l — the stump stays in the answer: the lookup is `?`, so its result is
-- whatever an instance's row makes it (`runF_selEx_principal`, below)
#guard run (.lam "x" (.sel (v "x") "l")) = "⟨r2.l ↓ t3⟩ ⇒ ({r2} → t3)"
-- …and applied, the promise is kept: found at 𝓫, absent at ★
#guard run (.app (.lam "x" (.sel (v "x") "l")) (rec1 "l" c)) = "𝓫"
#guard run (.app (.lam "x" (.sel (v "x") "l")) (.rcd .empty)) = "★"

-- refinement: ({l = c} ‖ x).l cannot look past x, so the answer asks x first …
#guard run (.lam "x" (.sel (.cat (rec1 "l" c) (v "x")) "l")) = "⟨r3 | l: 𝓫 | ε.l ↓ t5⟩ ⇒ ({r3} → t5)"
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
  = "⟨r5.m ↓ t6⟩ ⇒ ({r5} → {p: 𝓫 | q: ★})"

-- the spent promise (`spentEx_declarative`): A-app writes δ ≔ t2 → t5, and
-- the answer keeps the constraint on the arrow. The old entry put the field into
-- x's row (F-hit), which is the fill instance (`spentA_inst_fill`, below).
#guard run (.lam "x" (.lam "y" (.app (.sel (v "x") "l") (v "y"))))
  = "⟨r3.l ↓ (t2 → t5)⟩ ⇒ ({r3} → (t2 → t5))"
-- …a second, unspent selection on the row is a second constraint. The trailing
-- ε is the unifier's spine form, ≈-invisible
#guard run (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.sel (v "x") "m")))
  = "⟨r5.m ↓ t6⟩, ⟨r5 | ε.l ↓ (𝓫 → t4)⟩ ⇒ ({r5 | ε} → {p: t4 | q: t6})"
-- …and a second selection of the SAME field is merged with the first (M-merge,
-- `merge_forced`): one lookup, one result
#guard run (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.app (.sel (v "x") "l") c)))
  = "⟨r5.l ↓ (𝓫 → t4)⟩, ⟨r5 | ε.l ↓ (𝓫 → t4)⟩ ⇒ ({r5 | ε} → {p: t4 | q: t4})"

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
-- the same field spent twice on one row (`Parked.spentAlike`): the two results
-- 𝓫 → β₁ and 𝓫 → β₂ agree once β₁, β₂ go to ★, so one field answers both
#guard run (.letE "f" (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.app (.sel (v "x") "l") c)))
    (rec2 "a" (.app (v "f") (rec1 "l" (.lam "z" (v "z"))))
          "b" (.app (v "f") (rec1 "l" (.lam "z" (.rcd .empty))))))
  = "{a: {p: 𝓫 | q: 𝓫} | b: {p: {ε} | q: {ε}}}"

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
-- variable (phase B: keys got their own sort; a key of base type answers ★, a key
-- variable is forced to a label).
def L (l : String) : E := .lab l
def sd (e₁ e₂ : E) : E := .selDyn e₁ e₂

-- the headline λa. λx. x.(a): a stump blocked on the record's row, keyed by a
-- label variable; at the top level it stays a constraint …
#guard run (.lam "a" (.lam "x" (sd (v "x") (v "a")))) = "⟨r3.${k4} ↓ t5⟩ ⇒ (⌊k4⌋ → ({r3} → t5))"
-- … and let-bound it is a qualified scheme, used at two keys: found, and absent
#guard run (.letE "get" (.lam "a" (.lam "x" (sd (v "x") (v "a"))))
    (rec2 "p" (.app (.app (v "get") (L "foo")) (rec1 "foo" c))
          "q" (.app (.app (v "get") (L "bar")) (rec1 "foo" c)))) = "{p: 𝓫 | q: ★}"
#guard run (.app (.app (.lam "a" (.lam "x" (sd (v "x") (v "a")))) (L "foo")) (rec1 "foo" c))
  = "𝓫"

-- LABEL REFINEMENT: the record is literal, only the key is unknown, so the stump
-- is blocked on the KEY (L-?-lab); applying the function solves the key, and
-- wake-up — which judges staleness by the keyed lookup — finds the field
#guard run (.lam "a" (sd (rec2 "foo" c "bar" (.rcd .empty)) (v "a"))) = "⟨foo: 𝓫 | bar: {ε} | ε.${k3} ↓ t4⟩ ⇒ (⌊k3⌋ → t4)"
#guard run (.app (.lam "a" (sd (rec2 "foo" c "bar" (.rcd .empty)) (v "a"))) (L "foo"))
  = "𝓫"
#guard run (.app (.lam "a" (sd (rec2 "foo" c "bar" (.rcd .empty)) (v "a"))) (L "baz"))
  = "★"

-- a literal key through the dynamic door is the static selection …
#guard run (sd (rec1 "foo" c) (L "foo")) = "𝓫"
-- … and a key of BASE type (a computed string) answers ★ (A-sel-dyn-𝓫): the
-- intro's `{…}.${toString t}`
#guard run (sd (rec1 "foo" c) c) = "★"
-- only a DEFINITE base type: a key variable is still forced to a label first, so
-- supplying a base-typed key later clashes (the conflict is not definite yet)
#guard run (.app (.lam "a" (sd (rec1 "foo" c) (v "a"))) c) = "fail: clash"

-- the spent promise through the second door: its key is a variable, so no row
-- can be filled with it. The answer keeps the constraint; the witness guesses
-- the key (`Guesses`, `keyEx_typed` below). The old entry failed here
#guard run (.lam "r" (.lam "a" (.app (sd (v "r") (v "a")) c)))
  = "⟨r3.${k4} ↓ (𝓫 → t6)⟩ ⇒ ({r3} → (⌊k4⌋ → t6))"

-- a key that is itself an unresolved selection; the key's field is forced to a
-- label, a spent result …
#guard run (.lam "r" (.lam "k" (sd (v "r") (.sel (v "k") "name"))))
  = "⟨r3.${k6} ↓ t7⟩, ⟨r4.name ↓ ⌊k6⌋⟩ ⇒ ({r3} → ({r4} → t7))"
-- … and under a let it is generalized (it used to be held back by the
-- independence premise's key clause: the key IS another stump's result)
#guard run (.letE "f" (.lam "r" (.lam "k" (sd (v "r") (.sel (v "k") "name")))) (v "f"))
  = "⟨r9.name ↓ ⌊k11⌋⟩, ⟨r8.${k11} ↓ t10⟩ ⇒ ({r8} → ({r9} → t10))"
-- no KeySafe: a key blocked on a row variable stays a label variable
#guard run (.lam "r" (rec1 "x" (sd (rec1 "x" c) (.sel (v "r") "a"))))
  = "⟨x: 𝓫 | ε.${k5} ↓ t6⟩, ⟨r3.a ↓ ⌊k5⌋⟩ ⇒ ({r3} → {x: t6})"

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
#guard run (.lam "a" (.sel (rd (v "a") c) "foo")) = "⟨${k2}: 𝓫 | ε.foo ↓ t4⟩ ⇒ (⌊k2⌋ → t4)"
#guard run (.app (.lam "a" (.sel (rd (v "a") c) "foo")) (L "foo")) = "𝓫"
-- `r // { ${n} = v }`: the update, which shadows an old field of that name
def upd : E := .lam "n" (.lam "r" (.cat (v "r") (rd (v "n") c)))
#guard run upd = "(⌊k3⌋ → ({r4} → {${k3}: 𝓫 | ε | r4}))"
#guard run (.sel (.app (.app upd (L "x")) (rec1 "x" (.rcd .empty))) "x") = "𝓫"
-- two records under one unknown key flow into one function (U-key)
#guard run (.lam "a" (.lam "f" (rec2 "p" (.app (v "f") (rd (v "a") c))
    "q" (.app (v "f") (rd (v "a") c))))) = "(⌊k5⌋ → (({${k5}: 𝓫} → t6) → {p: t6 | q: t6}))"
-- a key of base type makes the record ★ (A-rcd-dyn-𝓫) …
#guard run (rd c c) = "★"
-- … which has no eliminator: selecting from it clashes (★ ≐ {r})
#guard run (.sel (rd c c) "foo") = "fail: clash"
#guard run (.lam "a" (rd (.sel (v "a") "k") c)) = "⟨r2.k ↓ ⌊k4⌋⟩ ⇒ ({r2} → {${k4}: 𝓫})"


-- THE FORCED MERGE (M-merge, `merge_forced`): a field selected once spent and
-- once unspent asks one lookup twice, so the results are unified. Without it
-- `LetSpent` rejects the scheme: at the top level, and under a let, where the
-- two uses below used to clash (f was monomorphic)
#guard run (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.sel (v "x") "l")))
  = "⟨r5.l ↓ (𝓫 → t4)⟩, ⟨r5 | ε.l ↓ (𝓫 → t4)⟩ ⇒ ({r5 | ε} → {p: t4 | q: (𝓫 → t4)})"
#guard run (.letE "f" (.lam "x" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.sel (v "x") "l")))
    (rec2 "a" (.app (v "f") (rec1 "l" (.lam "z" (v "z"))))
          "b" (.app (v "f") (rec2 "l" (.lam "z" (v "z")) "m" c))))
  = "{a: {p: 𝓫 | q: (𝓫 → 𝓫)} | b: {p: 𝓫 | q: (𝓫 → 𝓫)}}"
-- …the unspent result may also be spent on a different shape
#guard run (.lam "x" (.lam "f" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (.app (v "f") (.sel (v "x") "l")))))
  = "⟨r6.l ↓ (𝓫 → t5)⟩, ⟨r6 | ε.l ↓ (𝓫 → t5)⟩ ⇒ ({r6 | ε} → (((𝓫 → t5) → t8) → {p: t5 | q: t8}))"
-- THE WITNESS SEARCH (`Guesses`): not forced, so only inside the witness. Rows
-- that differ before the blocker get a witness merge; a variable key on a spent
-- stump's blocker gets a key guess; and a guess that must hit a literal record
-- tries the record's labels
#guard run (.lam "x" (rec2 "p" (.app (.sel (.cat (rec1 "m" c) (v "x")) "l") c) "q" (.sel (v "x") "l")))
  = "⟨r7.l ↓ t8⟩, ⟨r7 | ε | m: 𝓫 | ε.l ↓ (𝓫 → t6)⟩ ⇒ ({r7 | ε} → {p: t6 | q: t8})"
#guard run (.lam "x" (.lam "a" (rec2 "p" (.app (.sel (v "x") "l") c) "q" (sd (v "x") (v "a")))))
  = "⟨r6.${k7} ↓ t8⟩, ⟨r6 | ε.l ↓ (𝓫 → t5)⟩ ⇒ ({r6 | ε} → (⌊k7⌋ → {p: t5 | q: t8}))"
#guard run (.lam "a" (.app (sd (rec1 "foo" (.lam "y" (v "y"))) (v "a")) c))
  = "⟨foo: (t2 → t2) | ε.${k4} ↓ (𝓫 → t6)⟩ ⇒ (⌊k4⌋ → t6)"
-- …but A-let has no key guess: f stays monomorphic, and two uses clash (06)
#guard run (.letE "f" (.lam "r" (.lam "a" (.app (sd (v "r") (v "a")) c)))
    (rec2 "p" (.app (.app (v "f") (rec1 "foo" (.lam "y" (v "y")))) (L "foo"))
          "q" (.app (.app (v "f") (rec1 "bar" (.lam "y" c))) (L "bar"))))
  = "fail: clash"

-- TOP-LEVEL SCHEMES (plans/top-level-schemes.md). The witnesses below are
-- about what `runF` answers: `decide` runs it in the kernel. Every instance
-- of such an answer types its program (`answerQ_sound`, from `runSound`).

deriving instance DecidableEq for QScheme

def σU : Sig Unit Unit := Sig.plain fun _ => ()

/-- the scheme a run answers, if it answers one -/
def answerQ (e : E) : Option (QScheme Unit) :=
  match runF σU 50 e with
  | .ok (σ, _) => some σ
  | _ => none

theorem answerQ_sound {e : E} {σ : QScheme Unit} (h : answerQ e = some σ) :
    (∀ τ, σ.Inst τ → QTyped σU QCtx.empty e τ) ∧ ∃ τ, σ.Inst τ := by
  unfold answerQ at h
  split at h
  · next σ' S hr => cases h; exact runF_typed hr
  · cases h

/-- ⊢  an instance of σ read off one of σ′ that every constraint and the body
agree with. Used to move instances between schemes that differ in the names of
their binders. -/
theorem QScheme.Inst.of_reading {σ σ' : QScheme Unit} {θ θ' : TySubst Unit} {τ : Ty Unit}
    (hfix : θ'.FixedOutside σ'.vars)
    (hc : ∀ s' ∈ σ'.constraints, ∃ s ∈ σ.constraints, s'.at θ' = s.at θ)
    (hb : σ'.body.applySubst θ' = σ.body.applySubst θ)
    (hd : ∀ s ∈ σ.constraints, s.Discharge θ) (he : σ.body.applySubst θ = τ) :
    σ'.Inst τ := by
  refine ⟨θ', hfix, fun s' hs' => ?_, hb.trans he⟩
  obtain ⟨s, hs, hat⟩ := hc s' hs'
  exact Stump.dischargeEquiv_iff_holds.mpr (hat ▸ Stump.dischargeEquiv_iff_holds.mp (hd s hs))

def b : Ty Unit := .base ()

-- ## λx. x.l answers selQ, up to the names of its binders: principal
def selA : QScheme Unit :=
  ⟨["aa", "aaa"], [⟨.var "aa", .lit "l", .var "aaa"⟩], .fn (.rcd (.var "aa")) (.var "aaa")⟩

theorem selEx_answerQ : answerQ (selEx Unit) = some selA := by decide

/-- the renaming aa ↦ β, aaa ↦ δ, and back, applied to an instance -/
def selRen (θ : TySubst Unit) (r d r' d' : TyVar) : TySubst Unit :=
  ⟨fun x => if x = d' then θ.ty d else .var x, fun x => if x = r' then θ.row r else .var x,
   fun x => .var x⟩

theorem selA_inst_iff (τ : Ty Unit) : selA.Inst τ ↔ (selQ Unit).Inst τ := by
  constructor
  · rintro ⟨θ, -, hd, he⟩
    refine QScheme.Inst.of_reading (θ := θ) (θ' := selRen θ "aa" "aaa" "β" "δ")
      ⟨fun α h => ?_, fun α h => ?_, fun _ _ => rfl⟩ (fun s' hs' => ?_) ?_ hd he
    · have : α ≠ "δ" := fun e => h (by simp [selQ, e])
      simp [selRen, this]
    · have : α ≠ "β" := fun e => h (by simp [selQ, e])
      simp [selRen, this]
    · simp only [selQ, List.mem_singleton] at hs'
      subst hs'
      exact ⟨_, List.mem_singleton_self _, by simp [Stump.at, selRen, selA, Row.applySubst,
        Ty.applySubst, Key.applySubst]⟩
    · simp [selQ, selA, selRen, Ty.applySubst, Row.applySubst]
  · rintro ⟨θ, -, hd, he⟩
    refine QScheme.Inst.of_reading (θ := θ) (θ' := selRen θ "β" "δ" "aa" "aaa")
      ⟨fun α h => ?_, fun α h => ?_, fun _ _ => rfl⟩ (fun s' hs' => ?_) ?_ hd he
    · have : α ≠ "aaa" := fun e => h (by simp [selA, e])
      simp [selRen, this]
    · have : α ≠ "aa" := fun e => h (by simp [selA, e])
      simp [selRen, this]
    · simp only [selA, List.mem_singleton] at hs'
      subst hs'
      exact ⟨_, List.mem_singleton_self _, by simp [Stump.at, selRen, selQ, Row.applySubst,
        Ty.applySubst, Key.applySubst]⟩
    · simp [selQ, selA, selRen, Ty.applySubst, Row.applySubst]

/-- ⊢  **the first principal answer of the algorithm**: what a run answers for
λx. x.l is a principal scheme for it. The old entry answered `{β} → ★`, which
`no_plain_principal_scheme` says no plain scheme improves on. -/
theorem runF_selEx_principal :
    ∃ σ, answerQ (selEx Unit) = some σ ∧ QScheme.Principal σU QCtx.empty (selEx Unit) σ := by
  refine ⟨selA, selEx_answerQ, ?_⟩
  obtain ⟨h₁, h₂, h₃⟩ := selQ_principal (B := Unit) σU
  refine ⟨fun τ h => h₁ τ ((selA_inst_iff τ).mp h), ?_, fun τ h => ?_⟩
  · obtain ⟨τ, h⟩ := h₂; exact ⟨τ, (selA_inst_iff τ).mpr h⟩
  · obtain ⟨τ', hi, hp⟩ := h₃ τ h; exact ⟨τ', (selA_inst_iff τ').mpr hi, hp⟩

/-- ⊢  …and the old answer `{β} → ★` is its D-? instance. -/
theorem selA_inst_star : selA.Inst (.fn (.rcd (.var "β")) .unk) :=
  ⟨⟨fun x => if x = "aaa" then .unk else .var x, fun x => if x = "aa" then .var "β" else .var x,
     fun x => .var x⟩,
   ⟨fun α h => if_neg (fun e => h (by simp [selA, e])),
    fun α h => if_neg (fun e => h (by simp [selA, e])), fun _ _ => rfl⟩,
   fun st hst => by
     simp only [selA, List.mem_singleton] at hst
     subst hst
     exact .unk (lookupQF_sound (r := .blocked "β") rfl).toLookupQ rfl,
   rfl⟩

-- ## the F-hit witness: x: (({l = y: y} ‖ x).l) c. The old entry put the field
-- into x's row and answered `{l: 𝓫 → α | ρ} → α`, which `{ε} → 𝓫` (the field
-- comes from the literal) is not an instance of. Both are instances now.
def fhitEx : E := .lam "x" (.app (.sel (.cat (rec1 "l" (.lam "y" (v "y"))) (v "x")) "l") c)

def fhitA : QScheme Unit :=
  ⟨["aaaa", "aaaaaaa", "aa"],
   [⟨.cat (.var "aaaa") (.cat (.sing "l" (.fn (.var "aa") (.var "aa"))) .empty), .lit "l",
     .fn b (.var "aaaaaaa")⟩],
   .fn (.rcd (.var "aaaa")) (.var "aaaaaaa")⟩

theorem fhitEx_answerQ : answerQ fhitEx = some fhitA := by decide

/-- the field supplied by x: the old answer -/
theorem fhitA_inst_row :
    fhitA.Inst (.fn (.rcd (.cat (.sing "l" (.fn b (.var "α"))) (.var "ρ"))) (.var "α")) :=
  ⟨⟨fun x => if x = "aaaaaaa" then .var "α" else .var x,
    fun x => if x = "aaaa" then .cat (.sing "l" (.fn b (.var "α"))) (.var "ρ") else .var x,
    fun x => .var x⟩,
   ⟨fun α h => if_neg (fun e => h (by simp [fhitA, e])),
    fun α h => if_neg (fun e => h (by simp [fhitA, e])), fun _ _ => rfl⟩,
   fun st hst => by
     simp only [fhitA, List.mem_singleton] at hst
     subst hst
     exact .hit (lookupQF_sound (r := .found (.fn b (.var "α"))) rfl) (.refl _),
   rfl⟩

/-- the field supplied by the literal: what the old answer missed -/
theorem fhitA_inst_lit : fhitA.Inst (.fn (.rcd .empty) b) :=
  ⟨⟨fun x => if x = "aaaaaaa" ∨ x = "aa" then b else .var x,
    fun x => if x = "aaaa" then .empty else .var x, fun x => .var x⟩,
   ⟨fun α h => if_neg (fun e => h (by rcases e with e | e <;> simp [fhitA, e])),
    fun α h => if_neg (fun e => h (by simp [fhitA, e])), fun _ _ => rfl⟩,
   fun st hst => by
     simp only [fhitA, List.mem_singleton] at hst
     subst hst
     exact .hit (lookupQF_sound (r := .found (.fn b b)) rfl) (.refl _),
   rfl⟩

-- ## the spent promise x: (x.l) c: the old answer (F-hit) is the fill instance
def spentA : QScheme Unit :=
  ⟨["aa", "aaaa"], [⟨.var "aa", .lit "l", .fn b (.var "aaaa")⟩], .fn (.rcd (.var "aa")) (.var "aaaa")⟩

theorem spentEx_answerQ : answerQ (.lam "x" (.app (.sel (v "x") "l") c)) = some spentA := by
  decide

theorem spentA_inst_fill :
    spentA.Inst (.fn (.rcd (.cat (.sing "l" (.fn b (.var "α"))) (.var "ρ"))) (.var "α")) :=
  ⟨⟨fun x => if x = "aaaa" then .var "α" else .var x,
    fun x => if x = "aa" then .cat (.sing "l" (.fn b (.var "α"))) (.var "ρ") else .var x,
    fun x => .var x⟩,
   ⟨fun α h => if_neg (fun e => h (by simp [spentA, e])),
    fun α h => if_neg (fun e => h (by simp [spentA, e])), fun _ _ => rfl⟩,
   fun st hst => by
     simp only [spentA, List.mem_singleton] at hst
     subst hst
     exact .hit (lookupQF_sound (r := .found (.fn b (.var "α"))) rfl) (.refl _),
   rfl⟩

-- ## the spent promise under a variable key, r: a: (r.${a}) c. The old entry
-- failed ("spent promise", above). The answer keeps the stump; the witness
-- guesses the key (`Guesses`), and `{foo: 𝓫 → 𝓫} → ⌊foo⌋ → 𝓫` is an instance.
def keyEx : E := .lam "r" (.lam "a" (.app (sd (v "r") (v "a")) c))

def keyA : QScheme Unit :=
  ⟨["aaa", "aaaa", "aaaaaa"], [⟨.var "aaa", .var "aaaa", .fn b (.var "aaaaaa")⟩],
   .fn (.rcd (.var "aaa")) (.fn (.lab (.var "aaaa")) (.var "aaaaaa"))⟩

theorem keyEx_answerQ : answerQ keyEx = some keyA := by decide

theorem keyA_inst : keyA.Inst (.fn (.rcd (.sing "foo" (.fn b b))) (.fn (.lab (.lit "foo")) b)) :=
  ⟨⟨fun x => if x = "aaaaaa" then b else .var x,
    fun x => if x = "aaa" then .sing "foo" (.fn b b) else .var x,
    fun x => if x = "aaaa" then .lit "foo" else .var x⟩,
   ⟨fun α h => if_neg (fun e => h (by simp [keyA, e])),
    fun α h => if_neg (fun e => h (by simp [keyA, e])),
    fun α h => if_neg (fun e => h (by simp [keyA, e]))⟩,
   fun st hst => by
     simp only [keyA, List.mem_singleton] at hst
     subst hst
     exact .hit (lookupQF_sound (r := .found (.fn b b)) rfl) (.refl _),
   rfl⟩

/-- ⊢  …so the program has that type, which the old entry could not show. -/
theorem keyEx_typed :
    QTyped σU QCtx.empty keyEx (.fn (.rcd (.sing "foo" (.fn b b))) (.fn (.lab (.lit "foo")) b)) :=
  (answerQ_sound keyEx_answerQ).1 _ keyA_inst

end InferRuns
end MinimalCalculus
