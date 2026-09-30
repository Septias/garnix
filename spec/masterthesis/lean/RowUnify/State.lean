-- ⟦S⟧ — THE SOLVER STATE READ AS A SUBSTITUTION.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- ## The gap this module used to close, and why it is gone
-- Every algorithmic rule that performs a field selection — A-sel, A-sel-⊥,
-- A-sel-? — and every wake-up rule — K-hit, K-⊥, K-repark — has a premise of
-- the shape
--     ⟦S⟧ ⊢ ρ.l ↓ r
-- a lookup performed UNDER the current solution.  The declarative lookup
-- relation used to read row-solutions out of a CONTEXT (`L-α` consulted
-- `Γ.rowEnv`) while the algorithm keeps them in a SUBSTITUTION (`Sol.row`), so
-- the premise was not even a proposition until the two were reconciled.  That
-- was this module's job: `Sol.toCtx` (read the solution as a context),
-- `Sol.rowWF_toCtx` (so `lookup_total` applies and the premise HAS a
-- derivation), `Sol.lookup_toCtx` / `Sol.lookup_toCtx_iff` (the θ ↦ rowEnv
-- bridge).
--
-- With `L-α` removed there is one discipline instead of two.  The premise reads
-- `ρ.applySubst ⟦S⟧` and the lookup is syntactic, so:
--
--   * the premise HAS a derivation unconditionally — `lookup_total` needs no
--     `RowWF`, hence no `Sol.Acyclic`.  `Sol.rowWF_toCtx` and
--     `Sol.lookup_total_toCtx` are deleted, and A-sel can no longer fail to
--     fire on a legal state for want of well-formedness.
--   * there is nothing to bridge.  `Sol.lookup_toCtx` said "reading S as a
--     context and applying it as a substitution AGREE"; only the second
--     reading survives, so the theorem has become the identity.  What it
--     actually bought downstream is `lookup_applySubst` (minimal.lean) plus
--     `Sol.lookup_sat` (InferSound.lean), both of which are about refining a
--     substitution and neither of which mentions a context.
--
-- `Sol.Closes`, `Sol.Acyclic`, `Sol.Ranked` and `Sol.closure` all STAY: they
-- are what makes ⟦S⟧-as-a-closure meaningful, which is what `algorithmic.typ`
-- specifies the rules to read and what `InferSound` takes σ := S′.subst
-- against at its last step.
--
-- ## The invariant both need
-- `L-α` chases a solution chain RECURSIVELY; a substitution substitutes ONCE.
-- The two agree when the substitution is the solution's CLOSURE (`Sol.Closes`),
-- and the chase terminates when no bound row-variable is reachable at a spine
-- position of a binding (`Sol.Acyclic`).  A legal solver state (`Sol.WF`) is
-- therefore: acyclic, plus ranked — the sorted well-foundedness that makes the
-- closure exist at all (`Sol.Ranked`, `Sol.closes_closure`).
--
-- ## Why NOT `Sol.Applied`
-- The first version of this module asked the driver for a FULLY APPLIED
-- solution, and that is the wrong demand twice over.
--
--   * It is false.  The driver returns a TRIANGULAR solution — U-expand emits
--     `δ ≔ τ` at the type sort and `β ≔ (l:δ | β′)` at the row sort in the same
--     solution, and `Sol.comp` pushes the residual through a stage's bindings,
--     not the stage's own.  Splitting the fuzzer's tripwire settles it: across
--     ~100k successes in three universes the ACYCLIC half fails 0 times and the
--     APPLIED half fails hundreds of times, every one of them triangular.
--   * It was never asked for.  `algorithmic.typ` specifies ⟦S⟧ as applying the
--     solution AS A CLOSURE.  A closure is a substitution satisfying its own
--     unfolding equation, `Sol.Closes`, and that is exactly what the `L-α` case
--     of the bridge induction consumes.
--
-- So `Applied` is discharged by CONSTRUCTION rather than demanded: a ranked
-- solution has a closure (`Sol.closes_closure`), and an applied solution is its
-- own (`Sol.closes_toSubst_of_applied`), so the old results are the special
-- case.  `Sol.NoCapture` — the syntactic "no bound variable occurs in any
-- binding" — remains a sufficient condition for everything
-- (`Sol.wf_of_noCapture`), but it is NOT an invariant of the driver: the
-- fuzzer refutes it in one move,
--     a ≐ᵣ (l: a)   succeeds with   a ≔ (l: a | ε)
-- where `a` is bound as a ROW variable while the payload `a` is a TYPE
-- variable.  They are different variables and collide only because `ftv` spans
-- one untagged namespace — which is also why the closure construction below
-- needs the SORTED occurrence list `Ty.sortedFtv` / `Row.sortedFtv` rather than
-- `ftv`: a variable bound at one sort and occurring at the other is never
-- moved, so the sort-blind measure does not decrease.
--
-- ## What is NOT proved here
-- That the driver returns a well-formed solution (`UnifyWF`, bottom of this
-- file).  Nothing above depends on it.  PROVED since, in RowUnify/Applied.lean
-- (`unifyWF`, `unifyAcyclic`): with U-expand gone every success is `Applied`,
-- so the "Why NOT `Sol.Applied`" argument above is historical.  The remainder
-- of this paragraph records the state before that.  Its ACYCLIC half has ~100k sweep
-- successes behind it and no counterexample; its RANKED half is the payload
-- version of the same question, and Stage 3 of the occurs work is what makes it
-- plausible — `δ ≔ {β}` together with `β ≔ (l:δ | β′)` is exactly the cycle
-- U-expand's self-reference filter now refuses to create.

import RowUnify.Trichotomy

namespace MinimalCalculus

---------------------------- ⟦S⟧ AS A CONTEXT ----------------------------------

-- The variables S binds, at either sort (one namespace, minimal.lean:649).
def Sol.dom {B : Type} (s : Sol B) : List TyVar :=
  s.ty.map Prod.fst ++ s.row.map Prod.fst ++ s.lab.map Prod.fst

-- S is FULLY APPLIED: applying S to one of its own bindings is the identity.
-- This is what reconciles `L-α` (chases the chain) with `applySubst`
-- (substitutes once) — on an applied solution, one step IS the chain.
def Sol.Applied {B : Type} (s : Sol B) : Prop :=
  (∀ p ∈ s.ty,  p.2.applySubst s.toSubst = p.2) ∧
  (∀ p ∈ s.row, p.2.applySubst s.toSubst = p.2) ∧
  (∀ p ∈ s.lab, p.2.applySubst s.toSubst = p.2)

-- S is ACYCLIC: no bound row-variable is reachable at a SPINE position of a
-- binding.  Payloads are irrelevant — `L-α` never descends into a field — which
-- is why this is stated on `sVarSeq ∘ toSpine` and not on `ftv`.
def Sol.Acyclic {B : Type} (s : Sol B) : Prop :=
  ∀ p ∈ s.row, ∀ β ∈ sVarSeq p.2.toSpine, β ∉ s.row.map Prod.fst

-- The syntactic sufficient condition: no bound variable occurs in any binding
-- at all.  Strictly stronger than `Sol.WF` and NOT an invariant of the driver —
-- see the header.
def Sol.NoCapture {B : Type} (s : Sol B) : Prop :=
  (∀ p ∈ s.ty,  ∀ β ∈ p.2.ftv, β ∉ s.dom) ∧
  (∀ p ∈ s.row, ∀ β ∈ p.2.ftv, β ∉ s.dom) ∧
  (∀ p ∈ s.lab, ∀ β ∈ p.2.ftv, β ∉ s.dom)

------------------------- ASSOCIATION-LIST PLUMBING ----------------------------

theorem tyLookup_not_mem {B : Type} {α : TyVar} :
    (σ : List (TyVar × Ty B)) → α ∉ σ.map Prod.fst → tyLookup α σ = .var α
  | [], _ => rfl
  | (β, τ) :: t, h => by
      have hne : ¬ β = α := by
        intro he; exact h (by simp [he])
      have ht : α ∉ t.map Prod.fst := by
        intro hm; exact h (by simp [hm])
      simp only [tyLookup, if_neg hne]
      exact tyLookup_not_mem t ht

theorem rowLookup_not_mem {B : Type} {α : TyVar} :
    (σ : List (TyVar × Row B)) → α ∉ σ.map Prod.fst → rowLookup α σ = .var α
  | [], _ => rfl
  | (β, ρ) :: t, h => by
      have hne : ¬ β = α := by
        intro he; exact h (by simp [he])
      have ht : α ∉ t.map Prod.fst := by
        intro hm; exact h (by simp [hm])
      simp only [rowLookup, if_neg hne]
      exact rowLookup_not_mem t ht

theorem labLookup_not_mem {α : TyVar} :
    (σ : List (TyVar × Key)) → α ∉ σ.map Prod.fst → labLookup α σ = .var α
  | [], _ => rfl
  | (β, k) :: t, h => by
      have hne : ¬ β = α := by
        intro he; exact h (by simp [he])
      have ht : α ∉ t.map Prod.fst := by
        intro hm; exact h (by simp [hm])
      simp only [labLookup, if_neg hne]
      exact labLookup_not_mem t ht

theorem labLookup_cases : (σ : List (TyVar × Key)) → (α : TyVar) →
    (α ∉ σ.map Prod.fst ∧ labLookup α σ = .var α) ∨
    (∃ p ∈ σ, p.1 = α ∧ labLookup α σ = p.2)
  | [], _ => .inl ⟨by simp, rfl⟩
  | (β, k) :: t, α => by
      by_cases h : β = α
      · exact .inr ⟨(β, k), List.mem_cons_self, h, by simp only [labLookup, if_pos h]⟩
      · rcases labLookup_cases t α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
        · refine .inl ⟨?_, by simp only [labLookup, if_neg h]; exact he⟩
          intro hmem
          simp only [List.map_cons, List.mem_cons] at hmem
          rcases hmem with he' | he'
          · exact h he'.symm
          · exact hnm he'
        · exact .inr ⟨p, List.mem_cons_of_mem _ hm, h1,
            by simp only [labLookup, if_neg h]; exact h2⟩

-- The association lists, as a DETERMINED case split: either the key is absent
-- and the lookup is the variable itself, or it names a binding.
theorem tyLookup_cases {B : Type} : (σ : List (TyVar × Ty B)) → (α : TyVar) →
    (α ∉ σ.map Prod.fst ∧ tyLookup α σ = .var α) ∨
    (∃ p ∈ σ, p.1 = α ∧ tyLookup α σ = p.2)
  | [], _ => .inl ⟨by simp, rfl⟩
  | (β, τ) :: t, α => by
      by_cases h : β = α
      · exact .inr ⟨(β, τ), List.mem_cons_self, h, by simp only [tyLookup, if_pos h]⟩
      · rcases tyLookup_cases t α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
        · refine .inl ⟨?_, by simp only [tyLookup, if_neg h]; exact he⟩
          intro hmem
          simp only [List.map_cons, List.mem_cons] at hmem
          rcases hmem with he' | he'
          · exact h he'.symm
          · exact hnm he'
        · exact .inr ⟨p, List.mem_cons_of_mem _ hm, h1,
            by simp only [tyLookup, if_neg h]; exact h2⟩

theorem rowLookup_cases {B : Type} : (σ : List (TyVar × Row B)) → (α : TyVar) →
    (α ∉ σ.map Prod.fst ∧ rowLookup α σ = .var α) ∨
    (∃ p ∈ σ, p.1 = α ∧ rowLookup α σ = p.2)
  | [], _ => .inl ⟨by simp, rfl⟩
  | (β, ρ) :: t, α => by
      by_cases h : β = α
      · exact .inr ⟨(β, ρ), List.mem_cons_self, h, by simp only [rowLookup, if_pos h]⟩
      · rcases rowLookup_cases t α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
        · refine .inl ⟨?_, by simp only [rowLookup, if_neg h]; exact he⟩
          intro hmem
          simp only [List.map_cons, List.mem_cons] at hmem
          rcases hmem with he' | he'
          · exact h he'.symm
          · exact hnm he'
        · exact .inr ⟨p, List.mem_cons_of_mem _ hm, h1,
            by simp only [rowLookup, if_neg h]; exact h2⟩

-- ⊢ what it means for σ to BE ⟦S⟧: it unfolds S at every bound variable and
--   invents nothing at the others.
def Sol.Closes {B : Type} (s : Sol B) (σ : TySubst B) : Prop :=
  (∀ α, σ.ty  α = (s.toSubst.ty  α).applySubst σ) ∧
  (∀ α, σ.row α = (s.toSubst.row α).applySubst σ) ∧
  (∀ α, α ∉ s.ty.map  Prod.fst → σ.ty  α = .var α) ∧
  (∀ α, α ∉ s.row.map Prod.fst → σ.row α = .var α) ∧
  (∀ α, σ.lab α = (s.toSubst.lab α).applySubst σ) ∧
  (∀ α, α ∉ s.lab.map Prod.fst → σ.lab α = .var α)

-- An already-applied solution is its own closure — so every result stated
-- against `Sol.Closes` covers the old `Sol.Applied` hypothesis.
theorem Sol.closes_toSubst_of_applied {B : Type} {s : Sol B} (h : s.Applied) :
    s.Closes s.toSubst := by
  refine ⟨fun α => ?_, fun α => ?_, fun α hα => tyLookup_not_mem _ hα,
          fun α hα => rowLookup_not_mem _ hα, fun α => ?_, fun α hα => labLookup_not_mem _ hα⟩
  · have hu : s.toSubst.ty α = tyLookup α s.ty := rfl
    rcases tyLookup_cases s.ty α with ⟨-, he⟩ | ⟨p, hmem, -, he⟩
    · rw [hu, he]
      show Ty.var α = s.toSubst.ty α
      rw [hu, he]
    · rw [hu, he]
      exact (h.1 p hmem).symm
  · have hu : s.toSubst.row α = rowLookup α s.row := rfl
    rcases rowLookup_cases s.row α with ⟨-, he⟩ | ⟨p, hmem, -, he⟩
    · rw [hu, he]
      show Row.var α = s.toSubst.row α
      rw [hu, he]
    · rw [hu, he]
      exact (h.2.1 p hmem).symm
  · have hu : s.toSubst.lab α = labLookup α s.lab := rfl
    rcases labLookup_cases s.lab α with ⟨-, he⟩ | ⟨p, hmem, -, he⟩
    · rw [hu, he]
      show Key.var α = s.toSubst.lab α
      rw [hu, he]
    · rw [hu, he]
      exact (h.2.2 p hmem).symm

------------------------- CONSTRUCTING THE CLOSURE -----------------------------

-- ## Sorted occurrences
-- Building the closure needs a measure that STRICTLY drops at every unfolding
-- step, and the sort-blind `ftv` does not give one: a variable bound at the row
-- sort but occurring at a TYPE position is not moved by the substitution and
-- stays there forever (`a ≐ᵣ (l:a)` is the smallest instance).  Tagging each
-- occurrence with the sort it occurs at fixes that, and it is the sorted `ftv`
-- the thesis already wants for `bindTy`'s occurs check — `Row.allRowVars`
-- (Defs.lean) is its row half. Label variables (in keys) are the third sort.
def Key.sortedFtv : Key → List (Srt × TyVar)
  | .lit _ => []
  | .var α => [(.lab, α)]

mutual
def Ty.sortedFtv {B : Type} : Ty B → List (Srt × TyVar)
  | .var α    => [(.ty, α)]
  | .base _   => []
  | .lab k    => Key.sortedFtv k
  | .unk      => []
  | .fn a b   => Ty.sortedFtv a ++ Ty.sortedFtv b
  | .rcd ρ    => Row.sortedFtv ρ

def Row.sortedFtv {B : Type} : Row B → List (Srt × TyVar)
  | .empty     => []
  | .var α     => [(.row, α)]
  | .sing _ τ  => Ty.sortedFtv τ
  | .cat ρ₁ ρ₂ => Row.sortedFtv ρ₁ ++ Row.sortedFtv ρ₂
  | .dsing q τ => Key.sortedFtv q ++ Ty.sortedFtv τ
end

-- What θ puts at a tagged occurrence.
def TySubst.ftvAt {B : Type} (θ : TySubst B) : Srt × TyVar → List (Srt × TyVar)
  | (.ty, α)  => Ty.sortedFtv  (θ.ty  α)
  | (.row, α) => Row.sortedFtv (θ.row α)
  | (.lab, α) => Key.sortedFtv (θ.lab α)

theorem Key.mem_sortedFtv_applySubst {B : Type} {θ : TySubst B} {x : Srt × TyVar} :
    (k : Key) → x ∈ Key.sortedFtv (k.applySubst θ) →
    ∃ y ∈ Key.sortedFtv k, x ∈ θ.ftvAt y
  | .lit _, h => by simp [Key.sortedFtv] at h
  | .var α, h => ⟨(.lab, α), by simp [Key.sortedFtv], h⟩

-- ⊢ every occurrence of the substituted term comes from one of the subject's,
--   through what θ puts there
mutual
theorem Ty.mem_sortedFtv_applySubst {B : Type} {θ : TySubst B} {x : Srt × TyVar} :
    (τ : Ty B) → x ∈ Ty.sortedFtv (τ.applySubst θ) →
    ∃ y ∈ Ty.sortedFtv τ, x ∈ θ.ftvAt y
  | .var α,  h => ⟨(.ty, α), by simp [Ty.sortedFtv], h⟩
  | .base _, h => by simp [Ty.applySubst, Ty.sortedFtv] at h
  | .lab k, h => by
      simp only [Ty.applySubst, Ty.sortedFtv] at h
      exact Key.mem_sortedFtv_applySubst k h
  | .unk,    h => by simp [Ty.applySubst, Ty.sortedFtv] at h
  | .fn a b, h => by
      simp only [Ty.applySubst, Ty.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · obtain ⟨y, hy, hx⟩ := Ty.mem_sortedFtv_applySubst a h
        exact ⟨y, by simp only [Ty.sortedFtv, List.mem_append]; exact .inl hy, hx⟩
      · obtain ⟨y, hy, hx⟩ := Ty.mem_sortedFtv_applySubst b h
        exact ⟨y, by simp only [Ty.sortedFtv, List.mem_append]; exact .inr hy, hx⟩
  | .rcd ρ,  h => by
      simp only [Ty.applySubst, Ty.sortedFtv] at h
      obtain ⟨y, hy, hx⟩ := Row.mem_sortedFtv_applySubst ρ h
      exact ⟨y, hy, hx⟩

theorem Row.mem_sortedFtv_applySubst {B : Type} {θ : TySubst B} {x : Srt × TyVar} :
    (ρ : Row B) → x ∈ Row.sortedFtv (ρ.applySubst θ) →
    ∃ y ∈ Row.sortedFtv ρ, x ∈ θ.ftvAt y
  | .empty,     h => by simp [Row.applySubst, Row.sortedFtv] at h
  | .var α,     h => ⟨(.row, α), by simp [Row.sortedFtv], h⟩
  | .sing _ τ,  h => by
      simp only [Row.applySubst, Row.sortedFtv] at h
      obtain ⟨y, hy, hx⟩ := Ty.mem_sortedFtv_applySubst τ h
      exact ⟨y, hy, hx⟩
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.applySubst, Row.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · obtain ⟨y, hy, hx⟩ := Row.mem_sortedFtv_applySubst ρ₁ h
        exact ⟨y, by simp only [Row.sortedFtv, List.mem_append]; exact .inl hy, hx⟩
      · obtain ⟨y, hy, hx⟩ := Row.mem_sortedFtv_applySubst ρ₂ h
        exact ⟨y, by simp only [Row.sortedFtv, List.mem_append]; exact .inr hy, hx⟩
  | .dsing q τ, h => by
      simp only [Row.applySubst, Row.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · obtain ⟨y, hy, hx⟩ := Key.mem_sortedFtv_applySubst q h
        exact ⟨y, by simp only [Row.sortedFtv, List.mem_append]; exact .inl hy, hx⟩
      · obtain ⟨y, hy, hx⟩ := Ty.mem_sortedFtv_applySubst τ h
        exact ⟨y, by simp only [Row.sortedFtv, List.mem_append]; exact .inr hy, hx⟩
end

-- ⊢ a substitution that is the identity at every occurring SORT is the identity
--   — the sorted refinement of `applySubst_fixed_ftv`
mutual
theorem Key.applySubst_fixed_sorted {B : Type} {θ : TySubst B} :
    (k : Key) → (∀ α, (.lab, α) ∈ Key.sortedFtv k → θ.lab α = .var α) →
    k.applySubst θ = k
  | .lit _, _ => rfl
  | .var α, hl => hl α (by simp [Key.sortedFtv])

theorem Ty.applySubst_fixed_sorted {B : Type} {θ : TySubst B} :
    (τ : Ty B) →
    (∀ α, (.ty, α) ∈ Ty.sortedFtv τ → θ.ty α = .var α) →
    (∀ α, (.row, α) ∈ Ty.sortedFtv τ → θ.row α = .var α) →
    (∀ α, (.lab, α) ∈ Ty.sortedFtv τ → θ.lab α = .var α) →
    τ.applySubst θ = τ
  | .var α,  ht, _, _  => by simp only [Ty.applySubst]; exact ht α (by simp [Ty.sortedFtv])
  | .base _, _,  _, _  => rfl
  | .lab k,  _,  _, hl => by
      simp only [Ty.applySubst, Key.applySubst_fixed_sorted k (fun α hα => hl α hα)]
  | .unk,    _,  _, _  => rfl
  | .fn a b, ht, hr, hl => by
      have L : ∀ {x}, x ∈ Ty.sortedFtv a → x ∈ Ty.sortedFtv (.fn a b) := fun h =>
        by simp only [Ty.sortedFtv, List.mem_append]; exact .inl h
      have R : ∀ {x}, x ∈ Ty.sortedFtv b → x ∈ Ty.sortedFtv (.fn a b) := fun h =>
        by simp only [Ty.sortedFtv, List.mem_append]; exact .inr h
      simp only [Ty.applySubst,
        Ty.applySubst_fixed_sorted a (fun α hα => ht α (L hα)) (fun α hα => hr α (L hα))
          (fun α hα => hl α (L hα)),
        Ty.applySubst_fixed_sorted b (fun α hα => ht α (R hα)) (fun α hα => hr α (R hα))
          (fun α hα => hl α (R hα))]
  | .rcd ρ,  ht, hr, hl => by
      simp only [Ty.applySubst, Row.applySubst_fixed_sorted ρ ht hr hl]

theorem Row.applySubst_fixed_sorted {B : Type} {θ : TySubst B} :
    (ρ : Row B) →
    (∀ α, (.ty, α) ∈ Row.sortedFtv ρ → θ.ty α = .var α) →
    (∀ α, (.row, α) ∈ Row.sortedFtv ρ → θ.row α = .var α) →
    (∀ α, (.lab, α) ∈ Row.sortedFtv ρ → θ.lab α = .var α) →
    ρ.applySubst θ = ρ
  | .empty,     _,  _,  _  => rfl
  | .var α,     _,  hr, _  => by simp only [Row.applySubst]; exact hr α (by simp [Row.sortedFtv])
  | .sing _ τ,  ht, hr, hl => by
      simp only [Row.applySubst, Ty.applySubst_fixed_sorted τ ht hr hl]
  | .cat ρ₁ ρ₂, ht, hr, hl => by
      have L : ∀ {x}, x ∈ Row.sortedFtv ρ₁ → x ∈ Row.sortedFtv (.cat ρ₁ ρ₂) := fun h =>
        by simp only [Row.sortedFtv, List.mem_append]; exact .inl h
      have R : ∀ {x}, x ∈ Row.sortedFtv ρ₂ → x ∈ Row.sortedFtv (.cat ρ₁ ρ₂) := fun h =>
        by simp only [Row.sortedFtv, List.mem_append]; exact .inr h
      simp only [Row.applySubst,
        Row.applySubst_fixed_sorted ρ₁ (fun α hα => ht α (L hα)) (fun α hα => hr α (L hα))
          (fun α hα => hl α (L hα)),
        Row.applySubst_fixed_sorted ρ₂ (fun α hα => ht α (R hα)) (fun α hα => hr α (R hα))
          (fun α hα => hl α (R hα))]
  | .dsing q τ, ht, hr, hl => by
      have L : ∀ {x}, x ∈ Key.sortedFtv q → x ∈ Row.sortedFtv (.dsing q τ) := fun h =>
        by simp only [Row.sortedFtv, List.mem_append]; exact .inl h
      have R : ∀ {x}, x ∈ Ty.sortedFtv τ → x ∈ Row.sortedFtv (.dsing q τ) := fun h =>
        by simp only [Row.sortedFtv, List.mem_append]; exact .inr h
      simp only [Row.applySubst,
        Key.applySubst_fixed_sorted q (fun α hα => hl α (L hα)),
        Ty.applySubst_fixed_sorted τ (fun α hα => ht α (R hα)) (fun α hα => hr α (R hα))
          (fun α hα => hl α (R hα))]
end

-- ## The acyclicity closure application needs
-- `Sol.Acyclic` is about SPINE positions, because that is all the lookup
-- relation descends into.  Closure application descends everywhere, so it needs
-- the stronger, sorted statement: the bindings are well-founded as a dependency
-- order, with the rank bounded by the number of bindings.  Stage 3 of the
-- occurs work is what makes this plausible — the payload-level cycle
-- (`δ ≔ {β}`, `β ≔ (l:δ | β′)`) is exactly what U-expand's self-reference
-- filter now refuses.
def Sol.domS {B : Type} (s : Sol B) : List (Srt × TyVar) :=
  s.ty.map (fun p => (.ty, p.1)) ++ s.row.map (fun p => (.row, p.1)) ++
    s.lab.map (fun p => (.lab, p.1))

def Sol.Ranked {B : Type} (s : Sol B) : Prop :=
  ∃ rank : Srt × TyVar → Nat,
    (∀ x ∈ s.domS, rank x < s.domS.length) ∧
    (∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (.ty, p.1)) ∧
    (∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (.row, p.1)) ∧
    (∀ p ∈ s.lab, ∀ x ∈ Key.sortedFtv p.2, x ∈ s.domS → rank x < rank (.lab, p.1))

theorem Sol.mem_domS {B : Type} {s : Sol B} {x : Srt × TyVar} :
    x ∈ s.domS ↔ (x.1 = .ty ∧ x.2 ∈ s.ty.map Prod.fst) ∨
      (x.1 = .row ∧ x.2 ∈ s.row.map Prod.fst) ∨ (x.1 = .lab ∧ x.2 ∈ s.lab.map Prod.fst) := by
  obtain ⟨b, α⟩ := x
  simp only [Sol.domS, List.mem_append, List.mem_map]
  constructor
  · rintro ((⟨p, hp, he⟩ | ⟨p, hp, he⟩) | ⟨p, hp, he⟩) <;> cases he
    · exact .inl ⟨rfl, p, hp, rfl⟩
    · exact .inr (.inl ⟨rfl, p, hp, rfl⟩)
    · exact .inr (.inr ⟨rfl, p, hp, rfl⟩)
  · rintro (⟨rfl, p, hp, rfl⟩ | ⟨rfl, p, hp, rfl⟩ | ⟨rfl, p, hp, rfl⟩)
    · exact .inl (.inl ⟨p, hp, rfl⟩)
    · exact .inl (.inr ⟨p, hp, rfl⟩)
    · exact .inr ⟨p, hp, rfl⟩

theorem Sol.mem_domS_ty {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∈ s.ty.map Prod.fst) : (.ty, α) ∈ s.domS :=
  Sol.mem_domS.mpr (.inl ⟨rfl, h⟩)

theorem Sol.mem_domS_row {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∈ s.row.map Prod.fst) : (.row, α) ∈ s.domS :=
  Sol.mem_domS.mpr (.inr (.inl ⟨rfl, h⟩))

theorem Sol.mem_domS_lab {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∈ s.lab.map Prod.fst) : (.lab, α) ∈ s.domS :=
  Sol.mem_domS.mpr (.inr (.inr ⟨rfl, h⟩))

theorem Sol.domS_ty_of_mem {B : Type} {s : Sol B} {α : TyVar}
    (h : (.ty, α) ∈ s.domS) : α ∈ s.ty.map Prod.fst := by
  rcases Sol.mem_domS.mp h with ⟨-, h⟩ | ⟨h, -⟩ | ⟨h, -⟩
  · exact h
  · cases h
  · cases h

theorem Sol.domS_row_of_mem {B : Type} {s : Sol B} {α : TyVar}
    (h : (.row, α) ∈ s.domS) : α ∈ s.row.map Prod.fst := by
  rcases Sol.mem_domS.mp h with ⟨h, -⟩ | ⟨-, h⟩ | ⟨h, -⟩
  · cases h
  · exact h
  · cases h

theorem Sol.domS_lab_of_mem {B : Type} {s : Sol B} {α : TyVar}
    (h : (.lab, α) ∈ s.domS) : α ∈ s.lab.map Prod.fst := by
  rcases Sol.mem_domS.mp h with ⟨h, -⟩ | ⟨h, -⟩ | ⟨-, h⟩
  · cases h
  · cases h
  · exact h

-- ⊢ an unbound occurrence is left exactly where it is
theorem Sol.ftvAt_not_mem {B : Type} {s : Sol B} :
    (y : Srt × TyVar) → y ∉ s.domS → s.toSubst.ftvAt y = [y]
  | (.ty, α), hy => by
      show Ty.sortedFtv (tyLookup α s.ty) = _
      rw [tyLookup_not_mem _ (fun hm => hy (Sol.mem_domS_ty hm))]
      rfl
  | (.row, α), hy => by
      show Row.sortedFtv (rowLookup α s.row) = _
      rw [rowLookup_not_mem _ (fun hm => hy (Sol.mem_domS_row hm))]
      rfl
  | (.lab, α), hy => by
      show Key.sortedFtv (labLookup α s.lab) = _
      rw [labLookup_not_mem _ (fun hm => hy (Sol.mem_domS_lab hm))]
      rfl

-- ⊢ one unfolding step strictly drops the rank of every BOUND occurrence
theorem Sol.ftvAt_rank {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    (hty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (.ty, p.1))
    (hrow : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (.row, p.1))
    (hlab : ∀ p ∈ s.lab, ∀ x ∈ Key.sortedFtv p.2, x ∈ s.domS → rank x < rank (.lab, p.1))
    {x y : Srt × TyVar} (hx : x ∈ s.toSubst.ftvAt y) (hxd : x ∈ s.domS) :
    rank x < rank y := by
  obtain ⟨b, α⟩ := y
  cases b with
  | ty =>
      have hx' : x ∈ Ty.sortedFtv (tyLookup α s.ty) := hx
      rcases tyLookup_cases s.ty α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
      · rw [he] at hx'
        simp only [Ty.sortedFtv, List.mem_singleton] at hx'
        subst hx'
        exact absurd (Sol.domS_ty_of_mem hxd) hnm
      · rw [h2] at hx'
        rw [← h1]
        exact hty p hm x hx' hxd
  | row =>
      have hx' : x ∈ Row.sortedFtv (rowLookup α s.row) := hx
      rcases rowLookup_cases s.row α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
      · rw [he] at hx'
        simp only [Row.sortedFtv, List.mem_singleton] at hx'
        subst hx'
        exact absurd (Sol.domS_row_of_mem hxd) hnm
      · rw [h2] at hx'
        rw [← h1]
        exact hrow p hm x hx' hxd
  | lab =>
      have hx' : x ∈ Key.sortedFtv (labLookup α s.lab) := hx
      rcases labLookup_cases s.lab α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
      · rw [he] at hx'
        simp only [Key.sortedFtv, List.mem_singleton] at hx'
        subst hx'
        exact absurd (Sol.domS_lab_of_mem hxd) hnm
      · rw [h2] at hx'
        rw [← h1]
        exact hlab p hm x hx' hxd

-- ## The closure, as an iterate
-- `closure n` applies S n times; `closure 0` is the identity.  On a ranked
-- solution |dom| rounds are enough, because each round retires one rank level.
def Sol.closure {B : Type} (s : Sol B) : Nat → TySubst B
  | 0     => TySubst.id B
  | n + 1 => TySubst.comp (s.closure n) s.toSubst

def Sol.appRowN {B : Type} (s : Sol B) : Nat → Row B → Row B
  | 0,     ρ => ρ
  | n + 1, ρ => s.appRowN n (ρ.applySubst s.toSubst)

def Sol.appTyN {B : Type} (s : Sol B) : Nat → Ty B → Ty B
  | 0,     τ => τ
  | n + 1, τ => s.appTyN n (τ.applySubst s.toSubst)

def Sol.appKeyN {B : Type} (s : Sol B) : Nat → Key → Key
  | 0,     k => k
  | n + 1, k => s.appKeyN n (k.applySubst s.toSubst)

theorem Sol.applySubst_closure_row {B : Type} {s : Sol B} :
    (n : Nat) → (ρ : Row B) → ρ.applySubst (s.closure n) = s.appRowN n ρ
  | 0,     ρ => Row.applySubst_id ρ
  | n + 1, ρ => by
      show ρ.applySubst (TySubst.comp (s.closure n) s.toSubst) = _
      rw [← Row.applySubst_applySubst s.toSubst (s.closure n) ρ,
          Sol.applySubst_closure_row n (ρ.applySubst s.toSubst)]
      rfl

theorem Sol.applySubst_closure_ty {B : Type} {s : Sol B} :
    (n : Nat) → (τ : Ty B) → τ.applySubst (s.closure n) = s.appTyN n τ
  | 0,     τ => Ty.applySubst_id τ
  | n + 1, τ => by
      show τ.applySubst (TySubst.comp (s.closure n) s.toSubst) = _
      rw [← Ty.applySubst_applySubst s.toSubst (s.closure n) τ,
          Sol.applySubst_closure_ty n (τ.applySubst s.toSubst)]
      rfl

theorem Sol.applySubst_closure_key {B : Type} {s : Sol B} :
    (n : Nat) → (k : Key) → k.applySubst (s.closure n) = s.appKeyN n k
  | 0,     k => Key.applySubst_id k
  | n + 1, k => by
      show k.applySubst (TySubst.comp (s.closure n) s.toSubst) = _
      rw [← Key.applySubst_applySubst s.toSubst (s.closure n) k,
          Sol.applySubst_closure_key n (k.applySubst s.toSubst)]
      rfl

theorem Sol.appRowN_succ {B : Type} {s : Sol B} :
    (n : Nat) → (ρ : Row B) →
    s.appRowN (n + 1) ρ = (s.appRowN n ρ).applySubst s.toSubst
  | 0,     _ => rfl
  | n + 1, ρ => Sol.appRowN_succ n (ρ.applySubst s.toSubst)

theorem Sol.appTyN_succ {B : Type} {s : Sol B} :
    (n : Nat) → (τ : Ty B) →
    s.appTyN (n + 1) τ = (s.appTyN n τ).applySubst s.toSubst
  | 0,     _ => rfl
  | n + 1, τ => Sol.appTyN_succ n (τ.applySubst s.toSubst)

theorem Sol.appKeyN_succ {B : Type} {s : Sol B} :
    (n : Nat) → (k : Key) →
    s.appKeyN (n + 1) k = (s.appKeyN n k).applySubst s.toSubst
  | 0,     _ => rfl
  | n + 1, k => Sol.appKeyN_succ n (k.applySubst s.toSubst)

theorem Sol.appRowN_var_fixed {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∉ s.row.map Prod.fst) :
    (n : Nat) → s.appRowN n (.var α) = .var α
  | 0     => rfl
  | n + 1 => by
      show s.appRowN n ((Row.var α).applySubst s.toSubst) = _
      rw [show (Row.var α).applySubst s.toSubst = Row.var α from rowLookup_not_mem _ h]
      exact Sol.appRowN_var_fixed h n

theorem Sol.appTyN_var_fixed {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∉ s.ty.map Prod.fst) :
    (n : Nat) → s.appTyN n (.var α) = .var α
  | 0     => rfl
  | n + 1 => by
      show s.appTyN n ((Ty.var α).applySubst s.toSubst) = _
      rw [show (Ty.var α).applySubst s.toSubst = Ty.var α from tyLookup_not_mem _ h]
      exact Sol.appTyN_var_fixed h n

theorem Sol.appKeyN_var_fixed {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∉ s.lab.map Prod.fst) :
    (n : Nat) → s.appKeyN n (.var α) = .var α
  | 0     => rfl
  | n + 1 => by
      show s.appKeyN n ((Key.var α).applySubst s.toSubst) = _
      rw [show (Key.var α).applySubst s.toSubst = Key.var α from labLookup_not_mem _ h]
      exact Sol.appKeyN_var_fixed h n

-- The measure: every BOUND occurrence still left has rank below k.
def Sol.BelowL {B : Type} (s : Sol B) (rank : Srt × TyVar → Nat) (k : Nat)
    (xs : List (Srt × TyVar)) : Prop :=
  ∀ x ∈ xs, x ∈ s.domS → rank x < k

/-- the three rank-descent clauses of `Sol.Ranked`, bundled -/
structure Sol.RankDesc {B : Type} (s : Sol B) (rank : Srt × TyVar → Nat) : Prop where
  ty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (.ty, p.1)
  row : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (.row, p.1)
  lab : ∀ p ∈ s.lab, ∀ x ∈ Key.sortedFtv p.2, x ∈ s.domS → rank x < rank (.lab, p.1)

theorem Sol.below_step {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    (hd : s.RankDesc rank) {k : Nat} {xs ys : List (Srt × TyVar)}
    (hmem : ∀ x ∈ ys, ∃ y ∈ xs, x ∈ s.toSubst.ftvAt y)
    (h : s.BelowL rank (k + 1) xs) : s.BelowL rank k ys := by
  intro x hx hxd
  obtain ⟨y, hy, hxy⟩ := hmem x hx
  by_cases hyd : y ∈ s.domS
  · have hlt := Sol.ftvAt_rank hd.ty hd.row hd.lab hxy hxd
    have := h y hy hyd
    omega
  · rw [Sol.ftvAt_not_mem y hyd] at hxy
    simp only [List.mem_singleton] at hxy
    exact absurd (hxy ▸ hxd) hyd

theorem Sol.below_appRowN {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    (hd : s.RankDesc rank) :
    (k : Nat) → (ρ : Row B) → s.BelowL rank k (Row.sortedFtv ρ) →
    s.BelowL rank 0 (Row.sortedFtv (s.appRowN k ρ))
  | 0,     _, h => h
  | k + 1, ρ, h =>
      Sol.below_appRowN hd k (ρ.applySubst s.toSubst)
        (Sol.below_step hd (fun _ hx => Row.mem_sortedFtv_applySubst ρ hx) h)

theorem Sol.below_appTyN {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    (hd : s.RankDesc rank) :
    (k : Nat) → (τ : Ty B) → s.BelowL rank k (Ty.sortedFtv τ) →
    s.BelowL rank 0 (Ty.sortedFtv (s.appTyN k τ))
  | 0,     _, h => h
  | k + 1, τ, h =>
      Sol.below_appTyN hd k (τ.applySubst s.toSubst)
        (Sol.below_step hd (fun _ hx => Ty.mem_sortedFtv_applySubst τ hx) h)

theorem Sol.below_appKeyN {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    (hd : s.RankDesc rank) :
    (k : Nat) → (q : Key) → s.BelowL rank k (Key.sortedFtv q) →
    s.BelowL rank 0 (Key.sortedFtv (s.appKeyN k q))
  | 0,     _, h => h
  | k + 1, q, h =>
      Sol.below_appKeyN hd k (q.applySubst s.toSubst)
        (Sol.below_step hd (fun _ hx => Key.mem_sortedFtv_applySubst q hx) h)

theorem Sol.fixed_row_of_below_zero {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    {ρ : Row B} (h : s.BelowL rank 0 (Row.sortedFtv ρ)) :
    ρ.applySubst s.toSubst = ρ := by
  refine Row.applySubst_fixed_sorted ρ (fun α hα => ?_) (fun α hα => ?_) (fun α hα => ?_)
  · exact tyLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_ty hm); omega)
  · exact rowLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_row hm); omega)
  · exact labLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_lab hm); omega)

theorem Sol.fixed_ty_of_below_zero {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    {τ : Ty B} (h : s.BelowL rank 0 (Ty.sortedFtv τ)) :
    τ.applySubst s.toSubst = τ := by
  refine Ty.applySubst_fixed_sorted τ (fun α hα => ?_) (fun α hα => ?_) (fun α hα => ?_)
  · exact tyLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_ty hm); omega)
  · exact rowLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_row hm); omega)
  · exact labLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_lab hm); omega)

theorem Sol.fixed_key_of_below_zero {B : Type} {s : Sol B} {rank : Srt × TyVar → Nat}
    {q : Key} (h : s.BelowL rank 0 (Key.sortedFtv q)) :
    q.applySubst s.toSubst = q :=
  Key.applySubst_fixed_sorted q (fun α hα =>
    labLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_lab hm); omega))

-- ⊢ A RANKED SOLUTION HAS A CLOSURE, and |dom| rounds compute it.
-- This is what discharges `Sol.Applied`: it is no longer demanded of the
-- driver's output, it is a property of ⟦S⟧ that holds by construction.
theorem Sol.closes_closure {B : Type} {s : Sol B} (hR : s.Ranked) :
    s.Closes (s.closure s.domS.length) := by
  obtain ⟨rank, hbound, hty, hrow, hlab⟩ := hR
  have hd : s.RankDesc rank := ⟨hty, hrow, hlab⟩
  have hrowfix : ∀ ρ : Row B,
      (s.appRowN s.domS.length ρ).applySubst s.toSubst = s.appRowN s.domS.length ρ :=
    fun ρ => Sol.fixed_row_of_below_zero
      (Sol.below_appRowN hd s.domS.length ρ (fun x _ hxd => hbound x hxd))
  have htyfix : ∀ τ : Ty B,
      (s.appTyN s.domS.length τ).applySubst s.toSubst = s.appTyN s.domS.length τ :=
    fun τ => Sol.fixed_ty_of_below_zero
      (Sol.below_appTyN hd s.domS.length τ (fun x _ hxd => hbound x hxd))
  have hkeyfix : ∀ q : Key,
      (s.appKeyN s.domS.length q).applySubst s.toSubst = s.appKeyN s.domS.length q :=
    fun q => Sol.fixed_key_of_below_zero
      (Sol.below_appKeyN hd s.domS.length q (fun x _ hxd => hbound x hxd))
  refine ⟨fun α => ?_, fun α => ?_, fun α hα => ?_, fun α hα => ?_, fun α => ?_,
    fun α hα => ?_⟩
  · show (Ty.var α).applySubst (s.closure s.domS.length) = _
    rw [Sol.applySubst_closure_ty, Sol.applySubst_closure_ty]
    show s.appTyN s.domS.length (Ty.var α) = s.appTyN (s.domS.length + 1) (Ty.var α)
    rw [Sol.appTyN_succ, htyfix]
  · show (Row.var α).applySubst (s.closure s.domS.length) = _
    rw [Sol.applySubst_closure_row, Sol.applySubst_closure_row]
    show s.appRowN s.domS.length (Row.var α) = s.appRowN (s.domS.length + 1) (Row.var α)
    rw [Sol.appRowN_succ, hrowfix]
  · show (Ty.var α).applySubst (s.closure s.domS.length) = _
    rw [Sol.applySubst_closure_ty]
    exact Sol.appTyN_var_fixed hα _
  · show (Row.var α).applySubst (s.closure s.domS.length) = _
    rw [Sol.applySubst_closure_row]
    exact Sol.appRowN_var_fixed hα _
  · show (Key.var α).applySubst (s.closure s.domS.length) = _
    rw [Sol.applySubst_closure_key, Sol.applySubst_closure_key]
    show s.appKeyN s.domS.length (Key.var α) = s.appKeyN (s.domS.length + 1) (Key.var α)
    rw [Sol.appKeyN_succ, hkeyfix]
  · show (Key.var α).applySubst (s.closure s.domS.length) = _
    rw [Sol.applySubst_closure_key]
    exact Sol.appKeyN_var_fixed hα _


-- ⊢ a tagged occurrence is a free variable, tag forgotten
theorem Key.mem_ftv_of_mem_sortedFtv {b : Srt} {α : TyVar} :
    (k : Key) → (b, α) ∈ Key.sortedFtv k → α ∈ k.ftv
  | .lit _, h => by simp [Key.sortedFtv] at h
  | .var β, h => by
      simp only [Key.sortedFtv, List.mem_singleton, Prod.mk.injEq] at h
      simp [Key.ftv, h.2]

mutual
theorem Ty.mem_ftv_of_mem_sortedFtv {B : Type} {b : Srt} {α : TyVar} :
    (τ : Ty B) → (b, α) ∈ Ty.sortedFtv τ → α ∈ τ.ftv
  | .var β,  h => by
      simp only [Ty.sortedFtv, List.mem_singleton, Prod.mk.injEq] at h
      simp [Ty.ftv, h.2]
  | .base _, h => by simp [Ty.sortedFtv] at h
  | .lab k, h => Key.mem_ftv_of_mem_sortedFtv k h
  | .unk,    h => by simp [Ty.sortedFtv] at h
  | .fn x y, h => by
      simp only [Ty.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · exact List.mem_append_left  _ (Ty.mem_ftv_of_mem_sortedFtv x h)
      · exact List.mem_append_right _ (Ty.mem_ftv_of_mem_sortedFtv y h)
  | .rcd ρ,  h => Row.mem_ftv_of_mem_sortedFtv ρ h

theorem Row.mem_ftv_of_mem_sortedFtv {B : Type} {b : Srt} {α : TyVar} :
    (ρ : Row B) → (b, α) ∈ Row.sortedFtv ρ → α ∈ ρ.ftv
  | .empty,     h => by simp [Row.sortedFtv] at h
  | .var β,     h => by
      simp only [Row.sortedFtv, List.mem_singleton, Prod.mk.injEq] at h
      simp [Row.ftv, h.2]
  | .sing _ τ,  h => Ty.mem_ftv_of_mem_sortedFtv τ h
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · exact List.mem_append_left  _ (Row.mem_ftv_of_mem_sortedFtv ρ₁ h)
      · exact List.mem_append_right _ (Row.mem_ftv_of_mem_sortedFtv ρ₂ h)
  | .dsing q τ, h => by
      simp only [Row.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · exact List.mem_append_left  _ (Key.mem_ftv_of_mem_sortedFtv q h)
      · exact List.mem_append_right _ (Ty.mem_ftv_of_mem_sortedFtv τ h)
end

-- ## …and its fibres are the guards the driver actually runs
-- `Ty.tyFtv` / `Ty.allRowVars` (Defs.lean) are `sortedFtv` split by tag. This
-- is what connects the closure measure here to `bindTy`'s occurs check:
-- `bindTy` binds at the TYPE sort and guards on the `.ty` fibre, so a variable
-- it lets through occurs only at `.row`/`.lab` positions — where θ.ty does not
-- reach it.
mutual
theorem Ty.mem_tyFtv_of_sortedFtv {B : Type} {α : TyVar} :
    (τ : Ty B) → (.ty, α) ∈ Ty.sortedFtv τ → α ∈ τ.tyFtv
  | .var _,  h => by simp [Ty.sortedFtv] at h; simp [Ty.tyFtv, h]
  | .base _, h => by simp [Ty.sortedFtv] at h
  | .lab k,  h => by cases k <;> simp [Ty.sortedFtv, Key.sortedFtv] at h
  | .unk,    h => by simp [Ty.sortedFtv] at h
  | .fn a b, h => by
      simp only [Ty.sortedFtv, List.mem_append] at h
      simp only [Ty.tyFtv, List.mem_append]
      rcases h with h | h
      · exact .inl (Ty.mem_tyFtv_of_sortedFtv a h)
      · exact .inr (Ty.mem_tyFtv_of_sortedFtv b h)
  | .rcd ρ,  h => Row.mem_tyFtv_of_sortedFtv ρ h

theorem Row.mem_tyFtv_of_sortedFtv {B : Type} {α : TyVar} :
    (ρ : Row B) → (.ty, α) ∈ Row.sortedFtv ρ → α ∈ ρ.tyFtv
  | .empty,     h => by simp [Row.sortedFtv] at h
  | .var _,     h => by simp [Row.sortedFtv] at h
  | .sing _ τ,  h => Ty.mem_tyFtv_of_sortedFtv τ h
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.sortedFtv, List.mem_append] at h
      simp only [Row.tyFtv, List.mem_append]
      rcases h with h | h
      · exact .inl (Row.mem_tyFtv_of_sortedFtv ρ₁ h)
      · exact .inr (Row.mem_tyFtv_of_sortedFtv ρ₂ h)
  | .dsing q τ, h => by
      simp only [Row.sortedFtv, List.mem_append] at h
      simp only [Row.tyFtv]
      rcases h with h | h
      · cases q <;> simp [Key.sortedFtv] at h
      · exact Ty.mem_tyFtv_of_sortedFtv τ h
end

mutual
theorem Ty.mem_allRowVars_of_sortedFtv {B : Type} {α : TyVar} :
    (τ : Ty B) → (.row, α) ∈ Ty.sortedFtv τ → α ∈ τ.allRowVars
  | .var _,  h => by simp [Ty.sortedFtv] at h
  | .base _, h => by simp [Ty.sortedFtv] at h
  | .lab k,  h => by cases k <;> simp [Ty.sortedFtv, Key.sortedFtv] at h
  | .unk,    h => by simp [Ty.sortedFtv] at h
  | .fn a b, h => by
      simp only [Ty.sortedFtv, List.mem_append] at h
      simp only [Ty.allRowVars, List.mem_append]
      rcases h with h | h
      · exact .inl (Ty.mem_allRowVars_of_sortedFtv a h)
      · exact .inr (Ty.mem_allRowVars_of_sortedFtv b h)
  | .rcd ρ,  h => Row.mem_allRowVars_of_sortedFtv ρ h

theorem Row.mem_allRowVars_of_sortedFtv {B : Type} {α : TyVar} :
    (ρ : Row B) → (.row, α) ∈ Row.sortedFtv ρ → α ∈ ρ.allRowVars
  | .empty,     h => by simp [Row.sortedFtv] at h
  | .var _,     h => by simp [Row.sortedFtv] at h; simp [Row.allRowVars, h]
  | .sing _ τ,  h => Ty.mem_allRowVars_of_sortedFtv τ h
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.sortedFtv, List.mem_append] at h
      simp only [Row.allRowVars, List.mem_append]
      rcases h with h | h
      · exact .inl (Row.mem_allRowVars_of_sortedFtv ρ₁ h)
      · exact .inr (Row.mem_allRowVars_of_sortedFtv ρ₂ h)
  | .dsing q τ, h => by
      simp only [Row.sortedFtv, List.mem_append] at h
      simp only [Row.allRowVars]
      rcases h with h | h
      · cases q <;> simp [Key.sortedFtv] at h
      · exact Ty.mem_allRowVars_of_sortedFtv τ h
end

-- ⊢ …and a tagged binding is a binding
theorem Sol.dom_of_mem_domS {B : Type} {s : Sol B} :
    {x : Srt × TyVar} → x ∈ s.domS → x.2 ∈ s.dom
  | (.ty, α), h => List.mem_append_left _ (List.mem_append_left _ (Sol.domS_ty_of_mem h))
  | (.row, α), h => List.mem_append_left _ (List.mem_append_right _ (Sol.domS_row_of_mem h))
  | (.lab, α), h => List.mem_append_right _ (Sol.domS_lab_of_mem h)

-- A legal solver state, as far as the lookup relation is concerned: ⟦S⟧ is a
-- well-formed CONTEXT (`acyclic`) and ⟦S⟧ EXISTS as a closure (`ranked`).
-- `Sol.Applied` is deliberately NOT here — see the closure section above.
structure Sol.WF {B : Type} (s : Sol B) : Prop where
  acyclic : s.Acyclic
  ranked  : s.Ranked

------------------------- IDEMPOTENCE, UNPACKED --------------------------------

-- A variable S does not bind is fixed by ⟦S⟧ at both sorts.
theorem Sol.toSubst_fixed {B : Type} {s : Sol B} {α : TyVar} (h : α ∉ s.dom) :
    s.toSubst.ty α = .var α ∧ s.toSubst.row α = .var α ∧ s.toSubst.lab α = .var α := by
  simp only [Sol.dom, List.mem_append, not_or] at h
  exact ⟨tyLookup_not_mem _ h.1.1, rowLookup_not_mem _ h.1.2, labLookup_not_mem _ h.2⟩

-- A spine variable of a row is one of its free variables.
theorem mem_ftv_of_mem_sVarSeq {B : Type} {β : TyVar} :
    (ρ : Row B) → β ∈ sVarSeq ρ.toSpine → β ∈ ρ.ftv
  | .empty,    h => nomatch h
  | .var α,    h => by
      simp only [Row.toSpine, sVarSeq, List.mem_singleton] at h
      simp [Row.ftv, h]
  | .sing _ _, h => by simp only [Row.toSpine, sVarSeq] at h; exact nomatch h
  | .dsing q _, h => by cases q <;> simp [Row.toSpine, Atom.ofKey, sVarSeq] at h
  | .cat ρ₁ ρ₂, h => by
      rw [Row.toSpine, sVarSeq_append, List.mem_append] at h
      rcases h with h | h
      · exact List.mem_append_left _ (mem_ftv_of_mem_sVarSeq ρ₁ h)
      · exact List.mem_append_right _ (mem_ftv_of_mem_sVarSeq ρ₂ h)

-- ⊢  no bound variable occurs in any binding  ⟹  S is fully applied
theorem Sol.applied_of_noCapture {B : Type} {s : Sol B} (h : s.NoCapture) :
    s.Applied :=
  ⟨fun p hp => Ty.applySubst_fixed_ftv p.2
      (fun β hβ => Sol.toSubst_fixed (h.1 p hp β hβ)),
   fun p hp => Row.applySubst_fixed_ftv p.2
      (fun β hβ => Sol.toSubst_fixed (h.2.1 p hp β hβ)),
   fun p hp => by
      obtain ⟨α, k⟩ := p
      cases k with
      | lit _ => rfl
      | var γ => exact (Sol.toSubst_fixed (h.2.2 _ hp γ (by simp [Key.ftv]))).2.2⟩

-- ⊢  …and a legal solver state.  The rank is constant: `NoCapture` says no
-- binding mentions a bound variable at all, so the descent condition is
-- vacuous.
theorem Sol.wf_of_noCapture {B : Type} {s : Sol B} (h : s.NoCapture) : s.WF where
  acyclic := by
    intro p hp β hβ hmem
    have := h.2.1 p hp β (mem_ftv_of_mem_sVarSeq p.2 hβ)
    exact this (List.mem_append_left _ (List.mem_append_right _ hmem))
  ranked := by
    refine ⟨fun _ => 0, fun x hx => ?_, fun p hp x hx hxd => ?_, fun p hp x hx hxd => ?_,
      fun p hp x hx hxd => ?_⟩
    · cases hlen : s.domS with
      | nil => rw [hlen] at hx; cases hx
      | cons _ _ => simp [hlen]
    · obtain ⟨b, α⟩ := x
      exact absurd (Sol.dom_of_mem_domS hxd)
        (h.1 p hp α (Ty.mem_ftv_of_mem_sortedFtv p.2 hx))
    · obtain ⟨b, α⟩ := x
      exact absurd (Sol.dom_of_mem_domS hxd)
        (h.2.1 p hp α (Row.mem_ftv_of_mem_sortedFtv p.2 hx))
    · obtain ⟨b, α⟩ := x
      exact absurd (Sol.dom_of_mem_domS hxd)
        (h.2.2 p hp α (Key.mem_ftv_of_mem_sortedFtv p.2 hx))

--------------- ⟦S⟧-AS-A-SUBSTITUTION: WHAT USED TO NEED A BRIDGE --------------
--
-- This section held `Row.rankUnder_eq_zero`, `Sol.rowWF_toCtx`,
-- `Sol.lookup_total_toCtx`, `Sol.lookup_toCtx` and `Sol.lookup_toCtx_iff`.
-- All five are gone, and each for the same reason: they existed to reconcile a
-- lookup that chased `Γ.rowEnv` with one that substituted.  Concretely —
--
--   * `Row.rankUnder_eq_zero` / `Sol.rowWF_toCtx` built the rank function that
--     made ⟦S⟧-as-a-context well-formed, so that `lookup_total` (which needed
--     `RowWF`) applied.  `lookup_total` is now unconditional.
--   * `Sol.lookup_total_toCtx` was the corollary A-sel consumed; A-sel now
--     consumes `lookup_total` itself, with no `Sol.Acyclic` hypothesis.
--   * `Sol.lookup_toCtx` / `_iff` were the bridge.  With one discipline there
--     is nothing to bridge; the statement it proved is below, as a corollary of
--     `lookup_applySubst`, and says only that a definite lookup under a
--     solution survives closing that solution.
--
-- NOTE the net movement of hypotheses. `Sol.Acyclic` was needed HERE (to make
-- the chase terminate) and is no longer; `Sol.Closes`/`Sol.Ranked` are still
-- needed, but only where they were always going to be needed — at the point
-- `InferSound` takes σ := S′.subst and claims it IS ⟦S⟧. So the acyclicity
-- obligation is not relocated, it is discharged.

-- ⊢  a DEFINITE lookup under a solution survives closing it
-- The residue of `Sol.lookup_toCtx`: no context, no `Closes`, no `Acyclic` —
-- substitution is the only thing left in the statement.
theorem Sol.lookup_applySubst_closure {B : Type} {s : Sol B} (n : Nat)
    {ρ : Row B} {l : Label} {r : LookupRes B}
    (h : Lookup ρ l r) (hr : r ≠ .unknown) :
    Lookup (ρ.applySubst (s.closure n)) l (r.applySubst (s.closure n)) :=
  lookup_applySubst _ h hr

------------------------------ WHAT REMAINS ------------------------------------

-- The obligation this module leaves open, as a STATEMENT — not an axiom, and
-- nothing above uses it.  Everything else in the inference layer is now
-- expressible; this is what makes it apply to the states the driver actually
-- produces.
--
-- Both halves are about COMPOSITION: `Sol.comp` pushes the second stage through
-- the first stage's bindings but appends the second stage's own, so a
-- composite is acyclic (and ranked) as long as the second stage invents no name
-- the first one binds — which is what the `Supply`/`Avoids` discipline of
-- `unifyM_bounded` is for.  `Sol.Applied` is NOT part of this any more: it is
-- discharged by reading ⟦S⟧ as a closure (`Sol.closes_of_wf`).
--------------------- TOWARDS `Acyclic`: THE TWO ALGEBRAIC STEPS --------------
-- `Sol.Acyclic` is the SPINE-only half of well-formedness: no bound row
-- variable occurs at a spine position of any row binding. It is the half the
-- fuzzer has never refuted, and the half `Sol.rowWF_toCtx` /
-- `lookup_total_toCtx` actually consume — so proving it alone already makes
-- `A-sel`'s premise guaranteed to have a derivation, without any rank function.

/-- `s` is acyclic AND its row bindings keep their spine clear of `V` as well.
`V` is what the ENCLOSING stages have already bound: composition needs the later
stage to avoid the earlier stage's domain, and that fact has to be carried
inductively rather than recovered afterwards. -/
def Sol.AcyclicAvoiding {B : Type} (V : List TyVar) (s : Sol B) : Prop :=
  ∀ p ∈ s.row, ∀ β ∈ sVarSeq p.2.toSpine,
    β ∉ s.row.map Prod.fst ∧ β ∉ V

theorem Sol.AcyclicAvoiding.toAcyclic {B : Type} {V : List TyVar} {s : Sol B}
    (h : s.AcyclicAvoiding V) : s.Acyclic :=
  fun p hp β hβ => (h p hp β hβ).1

-- ⊢  COMPOSITION. `Sol.comp s₂ s₁` is "first s₁, then s₂": s₁'s bindings pushed
--    through s₂, then s₂'s own. Both halves stay spine-clean provided the LATER
--    stage avoids the earlier stage's domain — which is what `AcyclicAvoiding`
--    carries.
theorem Sol.acyclic_comp {B : Type} {s₁ s₂ : Sol B} {V : List TyVar}
    (h₁ : s₁.AcyclicAvoiding V)
    (h₂ : s₂.AcyclicAvoiding (V ++ s₁.row.map Prod.fst)) :
    (s₂.comp s₁).AcyclicAvoiding V := by
  intro p hp β hβ
  have hdom : ∀ γ, γ ∈ (s₂.comp s₁).row.map Prod.fst →
      γ ∈ s₁.row.map Prod.fst ∨ γ ∈ s₂.row.map Prod.fst := by
    intro γ hγ
    simp only [Sol.comp, List.map_append, List.map_map, List.mem_append] at hγ
    rcases hγ with hh | hh
    · exact .inl (by simpa using hh)
    · exact .inr hh
  simp only [Sol.comp, List.mem_append] at hp
  rcases hp with hp | hp
  · -- a binding of the EARLIER stage, pushed through the later solution
    obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hp
    rw [sVarSeq_applySubst, List.mem_flatMap] at hβ
    obtain ⟨γ, hγ, hβγ⟩ := hβ
    rcases rowLookup_cases s₂.row γ with ⟨hnm, he⟩ | ⟨r, hr, -, he⟩
    · -- γ is untouched by the later stage: it stands for itself, and the
      -- earlier stage had already cleared it
      have hlk : s₂.toSubst.row γ = .var γ := he
      rw [hlk] at hβγ
      simp only [Row.toSpine, sVarSeq, List.mem_singleton] at hβγ
      subst hβγ
      obtain ⟨hnd, hnv⟩ := h₁ q hq β hγ
      refine ⟨fun hc => ?_, hnv⟩
      rcases hdom β hc with hh | hh
      · exact hnd hh
      · exact hnm hh
    · -- γ is solved by the later stage: read the answer off s₂
      have hlk : s₂.toSubst.row γ = r.2 := he
      rw [hlk] at hβγ
      obtain ⟨hnd, hnv⟩ := h₂ r hr β hβγ
      refine ⟨fun hc => ?_, fun hc => hnv (List.mem_append_left _ hc)⟩
      rcases hdom β hc with hh | hh
      · exact hnv (List.mem_append_right _ hh)
      · exact hnd hh
  · -- a binding of the LATER stage
    obtain ⟨hnd, hnv⟩ := h₂ p hp β hβ
    refine ⟨fun hc => ?_, fun hc => hnv (List.mem_append_left _ hc)⟩
    rcases hdom β hc with hh | hh
    · exact hnv (List.mem_append_right _ hh)
    · exact hnd hh

/-- PROVED: `unifyAcyclic` (RowUnify/Applied.lean), as a corollary of every
success being `Applied`. The status notes below predate the removal of U-expand.

The SPINE half of `UnifyWF`, on its own. Worth separating because it is
strictly cheaper. It used to buy what inference needed of lookups (with
`Acyclic`, ⟦S⟧ was a well-formed context and A-sel's premise had a
derivation); with ↓ context-free, lookup totality no longer depends on it.

STATUS. The two algebraic steps are DONE above: `sVarSeq_applySubst` (spine
variables transform by flatMap, so payloads — and with them the cross-sort leak
that refutes `NoCapture` — cannot reach this property) and `Sol.acyclic_comp`
(composition preserves it, given that the later stage avoids the earlier
stage's domain, which `AcyclicAvoiding` carries). That is exactly the step that
defeats `Ranked`, and here it closes.

WHAT REMAINS, and it is NOT a missing definition. The measure for the type
pass already exists: `Ty.allRowVars` / `Row.allRowVars` (Defs.lean) is exactly
"row variables at spine positions, at any nesting depth", and it is sort-aware
— a TYPE variable in payload position contributes nothing, which is why the
`NoCapture` leak cannot reach it.

The obstruction is that no "values avoid the domain" invariant over it
survives. Three were swept over all three universes:
  * every binding's `allRowVars` avoids the row domain — 40 / 2336 / 24 ✘
  * ROW bindings only                                  — 40 / 2160 / 24 ✘
  * TYPE bindings only (what the `.fn` arm needs)      —  0 /  176 /  0 ✘
and the witnesses show why it is false BY DESIGN rather than by accident:
    (l:𝓫 | b) ≐ᵣ (m:{a} | a)   ⟹   b ≔ (m:{a} | aaa | ε)
puts the bound row variable `a` inside a PAYLOAD, which `Acyclic` permits
because it reads only top-level spines; and
    (l:𝓫 | b | l:𝓫) ≐ᵣ (m:{a} | a)   ⟹   aaaa ≔ {a}
does the same at the type sort, `aaaa` being a δ an expansion invented with the
payload captured before `a` was solved.

So the `.fn` arm cannot carry "the problem's row variables avoid V": that
property is not preserved by `applySubst`, because a triangular solution
legitimately holds bound variables under record constructors. What IS true is
that such a variable never reaches a top-level spine — the driver applies θ
before recursing — but that is a fact about solve-and-apply, not about the
syntax of the problem, and stating it needs the applied form, which brings the
closure (and hence `Ranked`) back into play. A different induction is needed;
`sVarSeq_applySubst` and `Sol.acyclic_comp` above stand and are reusable in it. -/
def UnifyAcyclic (B : Type) [DecidableEq B] : Prop :=
  ∀ (fuel : Nat) (ρ₁ ρ₂ : Row B) (s : Sol B) (S' : Supply),
    unifyRowM fuel ρ₁ ρ₂ = .success s S' → s.Acyclic

/-- PROVED: `unifyWF` (RowUnify/Applied.lean). -/
def UnifyWF (B : Type) [DecidableEq B] : Prop :=
  ∀ (fuel : Nat) (ρ₁ ρ₂ : Row B) (s : Sol B) (S' : Supply),
    unifyRowM fuel ρ₁ ρ₂ = .success s S' → s.WF

-- The base case, for free.
theorem Sol.nil_wf {B : Type} : (Sol.nil : Sol B).WF where
  acyclic := by intro p hp; simp [Sol.nil] at hp
  ranked := ⟨fun _ => 0, fun x hx => by simp [Sol.domS, Sol.nil] at hx,
             fun p hp => by simp [Sol.nil] at hp, fun p hp => by simp [Sol.nil] at hp,
             fun p hp => by simp [Sol.nil] at hp⟩

-- ⊢  a legal solver state HAS its ⟦S⟧, and |dom| rounds of substitution are it.
theorem Sol.closes_of_wf {B : Type} {s : Sol B} (hwf : s.WF) :
    s.Closes (s.closure s.domS.length) :=
  Sol.closes_closure hwf.ranked

end MinimalCalculus
