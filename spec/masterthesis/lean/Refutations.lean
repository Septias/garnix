-- WHY THE STUCK LEG CANNOT BE PROVED AT THE ALGORITHM LEVEL.
--
-- Two results, in increasing order of consequence:
--   1. the parked `hbase` and friends are false in the SHAPE they are stated in
--      (an unconstrained `Q` conjunct), and
--   2. `.stuck` ⟹ no mgu is false OUTRIGHT — the verdict is CONSERVATIVE, in
--      the same way `.occurs` is (occurs_allVar_hasMgu). See stuck_masks_mgu.
--
-- `unifyM_stuck_no_mgu` (Trichotomy.lean) reduces the stuck leg to four named
-- hypotheses, each of the shape "for EVERY predicate Q, the configuration has
-- no mgu once Q is conjoined". This file shows that shape is refutable, so the
-- next step on the stuck leg is to RESTATE the leg, not to prove `hbase`.
--
-- Both refutations are guarded in Axioms.lean, so a change in what they rest on
-- breaks the build.
--
import RowUnify

namespace MinimalCalculus

private def uB : Ty Unit := .base ()

-- Any SINGLETON predicate has an mgu: itself. InstanceOf is reflexive, and a
-- one-element unifier set is trivially maximal.
-- ⊢  HasMguP (· = θ₀)
theorem hasMguP_singleton {B : Type} (θ₀ : TySubst B) :
    HasMguP (fun θ => θ = θ₀) := by
  refine ⟨θ₀, rfl, ?_⟩
  intro θ' h
  subst h
  exact ⟨TySubst.id B, fun x => by rw [Row.applySubst_id]; exact RowEquiv.refl _,
                       fun x => by rw [Ty.applySubst_id]; exact TyEquiv.refl _⟩

-- The Wand spine (β | α) ≐ᵣ (l:𝓫), the canonical terminal configuration.
private def ws₁ : List (Atom Unit) := [.var "b", .var "a"]
private def ws₂ : List (Atom Unit) := [.field "l" uB]

-- Every one of hbase's thirteen premises holds on it, kernel-checked.
example : stripL ws₁ ws₂ = none := rfl
example : stripR ws₁ ws₂ = none := rfl
example : solveVarM ⟨9⟩ ws₁ ws₂ = none := rfl
example : solveVarM ⟨9⟩ ws₂ ws₁ = none := rfl
example : matchL ws₁ ws₂ = none := rfl
example : matchL ws₂ ws₁ = none := rfl
example : matchR ws₁ ws₂ = none := rfl
example : matchR ws₂ ws₁ = none := rfl
example : groundMatch ws₁ ws₂ = none := rfl
example : groundMatch ws₂ ws₁ = none := rfl
example : expandL ⟨9⟩ ws₁ ws₂ = none := rfl
example : expandL ⟨9⟩ ws₂ ws₁ = none := rfl
example : projClash ws₁ ws₂ = false := rfl

-- …and it HAS a unifier (that is what makes it stuck rather than a clash).
private def wθ : TySubst Unit :=
  ⟨fun x => .var x,
   fun x => if x = "b" then .sing "l" uB else if x = "a" then .empty else .var x⟩

theorem wθ_unifies : Unifies wθ (ofSpine ws₁) (ofSpine ws₂) := by
  unfold Unifies ws₁ ws₂ ofSpine wθ
  simp [Row.applySubst, Ty.applySubst, uB, ofSpine]
  exact RowEquiv.cat (.refl _) RowEquiv.unitR

-- DEFECT 1: `Q` is unconstrained, so instantiate it at the singleton {wθ}.
-- Conjoining a singleton collapses ANY unifier set to a set with an mgu, so
-- hbase's conclusion is false at a configuration whose premises all hold.
-- ⊢  ¬ (∀ Q. ¬ HasMguP (θ ⊨ Wand ∧ Q θ))
theorem hbase_shape_false :
    ¬ (∀ (Q : TySubst Unit → Prop),
        ¬ HasMguP (fun θ => Unifies θ (ofSpine ws₁) (ofSpine ws₂) ∧ Q θ)) := by
  intro hall
  refine hall (fun θ => θ = wθ) ⟨wθ, ⟨wθ_unifies, rfl⟩, ?_⟩
  intro θ' hθ'
  cases hθ'.2
  exact ⟨TySubst.id Unit, fun x => by rw [Row.applySubst_id]; exact RowEquiv.refl _,
                          fun x => by rw [Ty.applySubst_id]; exact TyEquiv.refl _⟩

-- DEFECT 2, the deeper one: restricting Q to the shape an emitted type equation
-- actually has does NOT save the statement. Qeq below is `TyUnifies · τ τ'`,
-- which IS stable under post-composition — and the Wand configuration under it
-- has an mgu. This is eq_rescued_solved seen at the residual: the equation
-- pins β, which forces α ≔ ε, so the ambiguity is gone. The driver avoids this
-- by SOLVING the equation and applying it before recursing; threading it as an
-- unsolved conjunct throws that away.
private def Qeq (θ : TySubst Unit) : Prop :=
  TyUnifies θ (.rcd (.var "b")) (.rcd (.sing "l" uB))

private def wθ' : TySubst Unit :=
  ⟨fun x => .var x,
   fun x => if x = "b" then .sing "l" uB else if x = "a" then .empty else .var x⟩

-- ⊢  HasMguP (θ ⊨ Wand ∧ θ ⊨ {β} ≐ {l:𝓫})
theorem hbase_stableQ_false :
    HasMguP (fun θ => Unifies θ (ofSpine ws₁) (ofSpine ws₂) ∧ Qeq θ) := by
  refine ⟨wθ', ⟨wθ_unifies, ?_⟩, ?_⟩
  · show TyEquiv _ _
    simp only [Ty.applySubst, Row.applySubst, wθ', uB]
    exact TyEquiv.refl _
  · rintro θ' ⟨hu, hq⟩
    -- the equation pins θ' β ≈ (l : 𝓫)
    have hb : RowEquiv ((Row.var "b").applySubst θ') (Row.sing "l" uB) := by
      have := hq
      unfold Qeq TyUnifies at this
      simp only [Ty.applySubst] at this
      have h2 := TyEquiv.rcd_inv_both this
      simpa [Row.applySubst, Ty.applySubst, uB] using h2
    -- …hence θ' α ≈ ε, by left-cancelling the field
    have ha : RowEquiv ((Row.var "a").applySubst θ') Row.empty := by
      have hu' : RowEquiv ((Row.var "b").applySubst θ'
                            |>.cat (((Row.var "a").applySubst θ').cat Row.empty))
                          (Row.sing "l" uB |>.cat Row.empty) := by
        simpa [ws₁, ws₂, ofSpine, Row.applySubst, Ty.applySubst, uB, Unifies] using hu
      have : RowEquiv ((Row.sing "l" uB).cat
                        (((Row.var "a").applySubst θ').cat Row.empty))
                      ((Row.sing "l" uB).cat Row.empty) :=
        (RowEquiv.cat hb.symm (.refl _)).trans hu'
      have := RowEquiv.cancel_cat_left this
      exact RowEquiv.unitR.symm.trans this
    -- both bindings of wθ' are var-free, so θ' itself is the factoring σ
    refine ⟨θ', fun x => ?_, fun x => ?_⟩
    · by_cases hx : x = "b"
      · subst hx; simpa [wθ', Row.applySubst, Ty.applySubst, uB] using hb
      · by_cases hy : x = "a"
        · subst hy; simpa [wθ', Row.applySubst] using ha
        · simp [wθ', hx, hy, Row.applySubst]
          exact RowEquiv.refl _
    · simp [wθ', Ty.applySubst]
      exact TyEquiv.refl _


-- ## `.stuck` IS A CONSERVATIVE VERDICT (the sharp result)
--
-- The driver evaluates an emitted type equation FIRST and `UResM.seq`
-- propagates its `.stuck` immediately — so the residual row problem, which may
-- well disambiguate the whole thing, is never looked at.
--
--     (k:{β|α} | β)  ≐ᵣ  (k:{l:𝓫} | l:𝓫)
--
-- matchL peels the shared k-field and emits {β|α} ≐ {l:𝓫}, which IS Wand and
-- IS stuck, so the driver answers `.stuck`. But the residual β ≐ᵣ (l:𝓫) pins
-- β ≔ (l:𝓫), which forces α ≔ ε — the problem has a UNIQUE mgu. Compare
-- eq_rescued_solved, where the equation is solvable and the driver does exactly
-- the right thing; the difference here is only that the sub-call is ambiguous.
--
-- CONSEQUENCE. "stuck ⟹ no mgu" is not a theorem waiting to be proved, at any
-- formulation. The honest fourth leg is about TERMINAL configurations — those
-- where no move fires at all — and `.stuck` must be presented, like `.occurs`,
-- as a conservative give-up. The natural repair to the ALGORITHM is to defer an
-- ambiguous equation rather than fail on it (the thesis's parked stumps), which
-- would let the residual run first; whether that is confluent is open.
private def mρ₁ : Row Unit :=
  .cat (.sing "k" (.rcd (.cat (.var "b") (.var "a")))) (.var "b")
private def mρ₂ : Row Unit :=
  .cat (.sing "k" (.rcd (.sing "l" uB))) (.sing "l" uB)

-- ⊢  the algorithm answers stuck (kernel-checked: it RUNS)
theorem stuck_masks_mgu_reported :
    unifyRowM (B := Unit) 20 mρ₁ mρ₂ = .stuck := rfl

private def mθ : TySubst Unit :=
  ⟨fun x => .var x,
   fun x => if x = "b" then .sing "l" uB else if x = "a" then .empty else .var x⟩

-- ⊢  …yet the problem has an mgu:  β ≔ (l:𝓫),  α ≔ ε
theorem stuck_masks_mgu : HasMgu mρ₁ mρ₂ := by
  refine ⟨mθ, ?_, ?_⟩
  · show RowEquiv _ _
    simp only [mρ₁, mρ₂, mθ, Row.applySubst, Ty.applySubst, uB, if_true, if_neg
      (by decide : ¬ ("a" : TyVar) = "b")]
    exact RowEquiv.cat (RowEquiv.sing (TyEquiv.rcd RowEquiv.unitR)) (.refl _)
  · intro θ' hu
    have hu' : RowEquiv
        (Row.cat (Row.sing "k" (.rcd (Row.cat (θ'.row "b") (θ'.row "a")))) (θ'.row "b"))
        (Row.cat (Row.sing "k" (.rcd (Row.sing "l" uB))) (Row.sing "l" uB)) := by
      simpa [mρ₁, mρ₂, Row.applySubst, Ty.applySubst, uB, Unifies] using hu
    -- the shared k-field cancels, handing back the equation AND the residual
    obtain ⟨hty, hb⟩ := RowEquiv.field_cancel_left hu'
    have hin : RowEquiv (Row.cat (θ'.row "b") (θ'.row "a")) (Row.sing "l" uB) := by
      obtain ⟨ρ', heq, hr⟩ := (TyEquiv.rcd_inv_both hty).1 rfl
      rw [Ty.rcd.injEq] at heq
      subst heq
      exact hr
    -- the residual pins θ' β, and the equation then forces θ' α ≈ ε
    have ha : RowEquiv (θ'.row "a") (Row.empty : Row Unit) := by
      have h1 : RowEquiv (Row.cat (Row.sing "l" uB) (θ'.row "a"))
                         (Row.cat (Row.sing "l" uB) Row.empty) :=
        ((RowEquiv.cat hb.symm (.refl _)).trans hin).trans RowEquiv.unitR.symm
      exact RowEquiv.cancel_cat_left h1
    -- both bindings are var-free, so θ' itself is the factoring σ
    refine ⟨θ', fun x => ?_, fun x => ?_⟩
    · by_cases hx : x = "b"
      · subst hx; simpa [mθ, Row.applySubst, Ty.applySubst, uB] using hb
      · by_cases hy : x = "a"
        · subst hy; simpa [mθ, Row.applySubst] using ha
        · simp [mθ, hx, hy, Row.applySubst]; exact RowEquiv.refl _
    · simp [mθ, Ty.applySubst]; exact TyEquiv.refl _


-- ## …AND SO IS THE TERMINAL-CONFIGURATION LEG (the sharpest one)
--
-- The natural retreat from stuck_masks_mgu is to state the leg about TERMINAL
-- configurations — every move dead — rather than about the `.stuck` verdict.
-- That is false too.
--
--     (l : {w})  ≐ᵣ  (w | v)
--
-- Terminal: all twelve moves return `none` and projClash is false, each by
-- `rfl` (terminal_masks_mgu_terminal). U-expand refuses because the l-field has
-- TWO candidate hosts, w and v — which is exactly the Wand shape, and would
-- normally mean two incomparable placements.
--
-- But hosting in w is impossible: it would force θw ≈ (l : {θw}), an OCCURS
-- violation. Field counts cannot see it — both sides have l-count 1, the
-- recursion passing under a record constructor — so the algorithm's guards miss
-- it entirely. `Ty.rcdDepth` (NoMgu.lean) is the ≈-invariant that does see it.
-- With w ruled out the placement is FORCED, the unifier is unique, and a unique
-- unifier is trivially most general.
--
-- CONSEQUENCE. Terminality says "no move fires", which is a fact about the
-- MOVES, not about the problem. It does not imply the two placements are both
-- realizable. So there is no general converse to prove here at all: the honest
-- content of the fourth leg is the specific no-mgu theorems (Wand /
-- vars_vs_field, two-sided, all-variable-swap) plus these conservativity
-- examples. See proof-plan.md.

private def tρ₁ : Row Unit := .sing "l" (.rcd (.var "w"))
private def tρ₂ : Row Unit := .cat (.var "w") (.var "v")

theorem terminal_masks_mgu_terminal :
    Terminal (B := Unit) ⟨9⟩ tρ₁.toSpine tρ₂.toSpine :=
  ⟨rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl, rfl⟩

theorem terminal_masks_mgu_reported :
    unifyRowM (B := Unit) 20 tρ₁ tρ₂ = .stuck := rfl

private def tθ : TySubst Unit :=
  ⟨fun x => .var x,
   fun x => if x = "w" then .empty
            else if x = "v" then .sing "l" (.rcd .empty) else .var x⟩

theorem terminal_masks_mgu : HasMgu tρ₁ tρ₂ := by
  refine ⟨tθ, ?_, ?_⟩
  · show RowEquiv _ _
    simp only [tρ₁, tρ₂, tθ, Row.applySubst, Ty.applySubst, if_true,
      if_neg (by decide : ¬ ("v" : TyVar) = "w")]
    exact RowEquiv.unitL.symm
  · intro θ' hu
    have hu' : RowEquiv (Row.sing "l" (.rcd (θ'.row "w")))
                        (Row.cat (θ'.row "w") (θ'.row "v")) := by
      simpa [tρ₁, tρ₂, Row.applySubst, Ty.applySubst, Unifies] using hu
    -- the two components are var-free, and the l-field sits in exactly one
    obtain ⟨hvar, -⟩ := hu'.char
    have hspine : (Row.cat (θ'.row "w") (θ'.row "v")).toSpine
        = (θ'.row "w").toSpine ++ (θ'.row "v").toSpine := rfl
    rw [hspine, sVarSeq_append] at hvar
    have hvnil : sVarSeq (θ'.row "w").toSpine = [] ∧
                 sVarSeq (θ'.row "v").toSpine = [] :=
      List.append_eq_nil_iff.mp hvar.symm
    have hcount : ∀ m, (if ("l" : Label) = m then 1 else 0)
        = sFieldCount m (θ'.row "w").toSpine + sFieldCount m (θ'.row "v").toSpine := by
      intro m
      have h := rowEquiv_fieldCount_eq m hu'
      rw [hspine, sFieldCount_append] at h
      simpa only [Row.toSpine, sFieldCount, Nat.add_zero] using h
    have hforeign : ∀ m, m ≠ "l" →
        sFieldCount m (θ'.row "w").toSpine = 0 ∧
        sFieldCount m (θ'.row "v").toSpine = 0 := by
      intro m hm
      have h := hcount m
      rw [if_neg (fun hh => hm hh.symm)] at h
      omega
    have hl := hcount "l"
    rw [if_pos rfl] at hl
    by_cases hw0 : sFieldCount "l" (θ'.row "w").toSpine = 0
    · -- θ'w ≈ ε: the RHS collapses to θ'v and the LHS to (l : {ε})
      have hW : RowEquiv (θ'.row "w") Row.empty :=
        rowEquiv_empty_of_no_vars_no_fields hvnil.1 (fun m => by
          by_cases hm : m = "l"
          · rw [hm]; exact hw0
          · exact (hforeign m hm).1)
      have hV : RowEquiv (θ'.row "v") (Row.sing "l" (.rcd (Row.empty : Row Unit))) := by
        have h1 : RowEquiv (Row.sing "l" (Ty.rcd (Row.empty : Row Unit)))
                           (Row.sing "l" (Ty.rcd (θ'.row "w"))) :=
          RowEquiv.sing (TyEquiv.rcd hW.symm)
        have h2 : RowEquiv (Row.cat (θ'.row "w") (θ'.row "v")) (θ'.row "v") :=
          (RowEquiv.cat hW (.refl _)).trans RowEquiv.unitL
        exact ((h1.trans hu').trans h2).symm
      refine ⟨θ', fun x => ?_, fun x => ?_⟩
      · by_cases hx : x = "w"
        · subst hx; simpa [tθ, Row.applySubst] using hW
        · by_cases hy : x = "v"
          · subst hy; simpa [tθ, Row.applySubst, Ty.applySubst] using hV
          · simp [tθ, hx, hy, Row.applySubst]; exact RowEquiv.refl _
      · simp [tθ, Ty.applySubst]; exact TyEquiv.refl _
    · -- θ'w would host the l-field, forcing θ'w ≈ (l : {θ'w}) — an occurs
      -- violation the row-level count cannot see, but rcdDepth can
      have hv0 : sFieldCount "l" (θ'.row "v").toSpine = 0 := by omega
      have hV : RowEquiv (θ'.row "v") Row.empty :=
        rowEquiv_empty_of_no_vars_no_fields hvnil.2 (fun m => by
          by_cases hm : m = "l"
          · rw [hm]; exact hv0
          · exact (hforeign m hm).2)
      exact absurd (hu'.trans ((RowEquiv.cat (.refl _) hV).trans RowEquiv.unitR)).symm
        no_rcd_self_reference

-- ⊢  so the terminal-configuration leg is FALSE too
theorem terminalNoMgu_false : ¬ TerminalNoMgu Unit := by
  intro h
  refine h ⟨9⟩ (.field "l" (.rcd (.var "w"))) (.var "w") [] [Atom.var "v"]
    terminal_masks_mgu_terminal ?_
  exact (hasMgu_rowEquiv (Row.toSpine_equiv tρ₁) (Row.toSpine_equiv tρ₂)).mp
    terminal_masks_mgu

end MinimalCalculus
