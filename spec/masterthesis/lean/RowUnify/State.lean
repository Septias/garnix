-- ⟦S⟧ — THE SOLVER STATE READ AS A CONTEXT.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- ## The gap this closes
-- Every algorithmic rule that performs a field selection — A-sel, A-sel-⊥,
-- A-sel-? — and every wake-up rule — K-hit, K-⊥, K-repark — has a premise of
-- the shape
--     ⟦S⟧ ⊢ ρ.l ↓ r
-- a lookup performed UNDER the current solution.  But the declarative lookup
-- relation reads row-solutions out of a CONTEXT (`L-α` consults `Γ.rowEnv`)
-- while the algorithm keeps them in a SUBSTITUTION (`Sol.row`).  The coercion
-- between the two is never defined in `algorithmic.typ`, and without it none of
-- those premises is a proposition.  This module defines it and proves the two
-- facts that make the premise meaningful:
--
--   * ⟦S⟧ is a WELL-FORMED context (`Sol.rowWF_toCtx`), so `lookup_total`
--     applies and the premise is guaranteed to HAVE a derivation.  Without
--     this, A-sel could simply fail to fire on a legal state.
--   * reading S as a context and applying it as a substitution AGREE
--     (`Sol.lookup_toCtx`, `Sol.lookup_toCtx_iff`) — the θ ↦ rowEnv bridge.
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
-- file).  Nothing above depends on it.  Its ACYCLIC half has ~100k sweep
-- successes behind it and no counterexample; its RANKED half is the payload
-- version of the same question, and Stage 3 of the occurs work is what makes it
-- plausible — `δ ≔ {β}` together with `β ≔ (l:δ | β′)` is exactly the cycle
-- U-expand's self-reference filter now refuses to create.

import RowUnify.Trichotomy

namespace MinimalCalculus

---------------------------- ⟦S⟧ AS A CONTEXT ----------------------------------

-- The variables S binds, at either sort (one namespace, minimal.lean:649).
def Sol.dom {B : Type} (s : Sol B) : List TyVar :=
  s.ty.map Prod.fst ++ s.row.map Prod.fst

-- S is FULLY APPLIED: applying S to one of its own bindings is the identity.
-- This is what reconciles `L-α` (chases the chain) with `applySubst`
-- (substitutes once) — on an applied solution, one step IS the chain.
def Sol.Applied {B : Type} (s : Sol B) : Prop :=
  (∀ p ∈ s.ty,  p.2.applySubst s.toSubst = p.2) ∧
  (∀ p ∈ s.row, p.2.applySubst s.toSubst = p.2)

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
  (∀ p ∈ s.row, ∀ β ∈ p.2.ftv, β ∉ s.dom)

-- ⟦S⟧ as a context: a solution's row component IS a row environment.  No term
-- bindings — a solver state says nothing about λ- or let-bound variables.
def Sol.toCtx {B : Type} (s : Sol B) : Ctx B := ⟨[], s.row⟩

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

-- `Ctx.lookupRow` (find?-based) and `rowLookup` (if-based) agree, and a hit
-- exhibits the pair.
theorem Sol.lookupRow_some {B : Type} {s : Sol B} {α : TyVar} {ρ : Row B}
    (h : s.toCtx.lookupRow α = some ρ) : rowLookup α s.row = ρ ∧ (α, ρ) ∈ s.row := by
  simp only [Sol.toCtx, Ctx.lookupRow] at h
  revert h
  induction s.row with
  | nil => intro h; simp at h
  | cons p t ih =>
      obtain ⟨a, ρ'⟩ := p
      intro h
      by_cases hp : a = α
      · subst hp
        have hb : (a == a) = true := by simp
        simp only [List.find?, hb, Option.map_some] at h
        cases h
        exact ⟨by simp [rowLookup], List.Mem.head _⟩
      · have hb : (a == α) = false := by simp [hp]
        simp only [List.find?, hb] at h
        obtain ⟨h₁, h₂⟩ := ih h
        exact ⟨by simp only [rowLookup, if_neg hp]; exact h₁, List.Mem.tail _ h₂⟩

theorem Sol.lookupRow_none {B : Type} {s : Sol B} {α : TyVar}
    (h : s.toCtx.lookupRow α = none) : α ∉ s.row.map Prod.fst := by
  simp only [Sol.toCtx, Ctx.lookupRow] at h
  revert h
  induction s.row with
  | nil => intro _; simp
  | cons p t ih =>
      intro h
      by_cases hp : p.1 = α
      · have hb : (p.1 == α) = true := by simp [hp]
        simp only [List.find?, hb, Option.map_some] at h
        simp at h
      · have hb : (p.1 == α) = false := by simp [hp]
        simp only [List.find?, hb] at h
        intro hm
        simp only [List.map_cons, List.mem_cons] at hm
        cases hm with
        | inl he => exact hp he.symm
        | inr ht => exact ih h ht

---------------------------- ⟦S⟧ AS A CLOSURE ----------------------------------

-- ## Why `Sol.Applied` is the wrong demand, and what replaces it
-- The driver returns a TRIANGULAR solution — U-expand emits `δ ≔ τ` at the type
-- sort and `β ≔ (l:δ | β′)` at the row sort in the same solution, and `Sol.comp`
-- pushes the residual through a stage's bindings, not the stage's own — so
-- `Sol.toSubst` is a ONE-STEP substitution that does not reach a fixpoint.  The
-- fuzzer measures this: the tripwire's `Applied` half fails on hundreds of
-- successes while its `Acyclic` half fails on none.
--
-- `algorithmic.typ` never asked for a one-step substitution.  It specifies ⟦S⟧
-- as applying the solution AS A CLOSURE, and a closure is exactly a
-- substitution satisfying its own unfolding equation:
--
--     σ α  =  (S α) σ        at every variable, at either sort
--
-- That is `Sol.Closes` below.  On it the bridge theorems go through with no
-- idempotence hypothesis at all: what the `L-α` case of the induction needs is
-- precisely "σ at a bound variable = σ applied to its binding", which is the
-- unfolding equation read left to right.

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
  (∀ α, α ∉ s.row.map Prod.fst → σ.row α = .var α)

-- An already-applied solution is its own closure — so every result stated
-- against `Sol.Closes` covers the old `Sol.Applied` hypothesis.
theorem Sol.closes_toSubst_of_applied {B : Type} {s : Sol B} (h : s.Applied) :
    s.Closes s.toSubst := by
  refine ⟨fun α => ?_, fun α => ?_, fun α hα => tyLookup_not_mem _ hα,
          fun α hα => rowLookup_not_mem _ hα⟩
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
      exact (h.2 p hmem).symm

------------------------- CONSTRUCTING THE CLOSURE -----------------------------

-- ## Sorted occurrences
-- Building the closure needs a measure that STRICTLY drops at every unfolding
-- step, and the sort-blind `ftv` does not give one: a variable bound at the row
-- sort but occurring at a TYPE position is not moved by the substitution and
-- stays there forever (`a ≐ᵣ (l:a)` is the smallest instance).  Tagging each
-- occurrence with the sort it occurs at fixes that, and it is the sorted `ftv`
-- the thesis already wants for `bindTy`'s occurs check — `Row.allRowVars`
-- (Defs.lean) is its row half.  `true` = row sort.
mutual
def Ty.sortedFtv {B : Type} : Ty B → List (Bool × TyVar)
  | .var α    => [(false, α)]
  | .base _   => []
  | .unk      => []
  | .fn a b   => Ty.sortedFtv a ++ Ty.sortedFtv b
  | .rcd ρ    => Row.sortedFtv ρ

def Row.sortedFtv {B : Type} : Row B → List (Bool × TyVar)
  | .empty     => []
  | .var α     => [(true, α)]
  | .sing _ τ  => Ty.sortedFtv τ
  | .cat ρ₁ ρ₂ => Row.sortedFtv ρ₁ ++ Row.sortedFtv ρ₂
end

-- What θ puts at a tagged occurrence.
def TySubst.ftvAt {B : Type} (θ : TySubst B) : Bool × TyVar → List (Bool × TyVar)
  | (false, α) => Ty.sortedFtv  (θ.ty  α)
  | (true,  α) => Row.sortedFtv (θ.row α)

-- ⊢ every occurrence of the substituted term comes from one of the subject's,
--   through what θ puts there
mutual
theorem Ty.mem_sortedFtv_applySubst {B : Type} {θ : TySubst B} {x : Bool × TyVar} :
    (τ : Ty B) → x ∈ Ty.sortedFtv (τ.applySubst θ) →
    ∃ y ∈ Ty.sortedFtv τ, x ∈ θ.ftvAt y
  | .var α,  h => ⟨(false, α), by simp [Ty.sortedFtv], h⟩
  | .base _, h => by simp [Ty.applySubst, Ty.sortedFtv] at h
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

theorem Row.mem_sortedFtv_applySubst {B : Type} {θ : TySubst B} {x : Bool × TyVar} :
    (ρ : Row B) → x ∈ Row.sortedFtv (ρ.applySubst θ) →
    ∃ y ∈ Row.sortedFtv ρ, x ∈ θ.ftvAt y
  | .empty,     h => by simp [Row.applySubst, Row.sortedFtv] at h
  | .var α,     h => ⟨(true, α), by simp [Row.sortedFtv], h⟩
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
end

-- ⊢ a substitution that is the identity at every occurring SORT is the identity
--   — the sorted refinement of `applySubst_fixed_ftv`
mutual
theorem Ty.applySubst_fixed_sorted {B : Type} {θ : TySubst B} :
    (τ : Ty B) →
    (∀ α, (false, α) ∈ Ty.sortedFtv τ → θ.ty α = .var α) →
    (∀ α, (true, α) ∈ Ty.sortedFtv τ → θ.row α = .var α) →
    τ.applySubst θ = τ
  | .var α,  ht, _  => by simp only [Ty.applySubst]; exact ht α (by simp [Ty.sortedFtv])
  | .base _, _,  _  => rfl
  | .unk,    _,  _  => rfl
  | .fn a b, ht, hr => by
      simp only [Ty.applySubst,
        Ty.applySubst_fixed_sorted a
          (fun α hα => ht α (by simp only [Ty.sortedFtv, List.mem_append]; exact .inl hα))
          (fun α hα => hr α (by simp only [Ty.sortedFtv, List.mem_append]; exact .inl hα)),
        Ty.applySubst_fixed_sorted b
          (fun α hα => ht α (by simp only [Ty.sortedFtv, List.mem_append]; exact .inr hα))
          (fun α hα => hr α (by simp only [Ty.sortedFtv, List.mem_append]; exact .inr hα))]
  | .rcd ρ,  ht, hr => by
      simp only [Ty.applySubst, Row.applySubst_fixed_sorted ρ ht hr]

theorem Row.applySubst_fixed_sorted {B : Type} {θ : TySubst B} :
    (ρ : Row B) →
    (∀ α, (false, α) ∈ Row.sortedFtv ρ → θ.ty α = .var α) →
    (∀ α, (true, α) ∈ Row.sortedFtv ρ → θ.row α = .var α) →
    ρ.applySubst θ = ρ
  | .empty,     _,  _  => rfl
  | .var α,     _,  hr => by simp only [Row.applySubst]; exact hr α (by simp [Row.sortedFtv])
  | .sing _ τ,  ht, hr => by
      simp only [Row.applySubst, Ty.applySubst_fixed_sorted τ ht hr]
  | .cat ρ₁ ρ₂, ht, hr => by
      simp only [Row.applySubst,
        Row.applySubst_fixed_sorted ρ₁
          (fun α hα => ht α (by simp only [Row.sortedFtv, List.mem_append]; exact .inl hα))
          (fun α hα => hr α (by simp only [Row.sortedFtv, List.mem_append]; exact .inl hα)),
        Row.applySubst_fixed_sorted ρ₂
          (fun α hα => ht α (by simp only [Row.sortedFtv, List.mem_append]; exact .inr hα))
          (fun α hα => hr α (by simp only [Row.sortedFtv, List.mem_append]; exact .inr hα))]
end

-- ## The acyclicity closure application needs
-- `Sol.Acyclic` is about SPINE positions, because that is all the lookup
-- relation descends into.  Closure application descends everywhere, so it needs
-- the stronger, sorted statement: the bindings are well-founded as a dependency
-- order, with the rank bounded by the number of bindings.  Stage 3 of the
-- occurs work is what makes this plausible — the payload-level cycle
-- (`δ ≔ {β}`, `β ≔ (l:δ | β′)`) is exactly what U-expand's self-reference
-- filter now refuses.
def Sol.domS {B : Type} (s : Sol B) : List (Bool × TyVar) :=
  s.ty.map (fun p => (false, p.1)) ++ s.row.map (fun p => (true, p.1))

def Sol.Ranked {B : Type} (s : Sol B) : Prop :=
  ∃ rank : Bool × TyVar → Nat,
    (∀ x ∈ s.domS, rank x < s.domS.length) ∧
    (∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (false, p.1)) ∧
    (∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (true,  p.1))

theorem Sol.mem_domS_ty {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∈ s.ty.map Prod.fst) : (false, α) ∈ s.domS := by
  obtain ⟨p, hp, he⟩ := List.mem_map.mp h
  exact List.mem_append_left _ (List.mem_map.2 ⟨p, hp, by rw [he]⟩)

theorem Sol.mem_domS_row {B : Type} {s : Sol B} {α : TyVar}
    (h : α ∈ s.row.map Prod.fst) : (true, α) ∈ s.domS := by
  obtain ⟨p, hp, he⟩ := List.mem_map.mp h
  exact List.mem_append_right _ (List.mem_map.2 ⟨p, hp, by rw [he]⟩)

theorem Sol.domS_ty_of_mem {B : Type} {s : Sol B} {α : TyVar}
    (h : (false, α) ∈ s.domS) : α ∈ s.ty.map Prod.fst := by
  rcases List.mem_append.mp h with hm | hm
  · obtain ⟨p, hp, he⟩ := List.mem_map.mp hm
    exact List.mem_map.2 ⟨p, hp, by have := congrArg Prod.snd he; simpa using this⟩
  · obtain ⟨p, hp, he⟩ := List.mem_map.mp hm
    exact absurd (congrArg Prod.fst he) (by simp)

theorem Sol.domS_row_of_mem {B : Type} {s : Sol B} {α : TyVar}
    (h : (true, α) ∈ s.domS) : α ∈ s.row.map Prod.fst := by
  rcases List.mem_append.mp h with hm | hm
  · obtain ⟨p, hp, he⟩ := List.mem_map.mp hm
    exact absurd (congrArg Prod.fst he) (by simp)
  · obtain ⟨p, hp, he⟩ := List.mem_map.mp hm
    exact List.mem_map.2 ⟨p, hp, by have := congrArg Prod.snd he; simpa using this⟩

-- ⊢ an unbound occurrence is left exactly where it is
theorem Sol.ftvAt_not_mem {B : Type} {s : Sol B} :
    (y : Bool × TyVar) → y ∉ s.domS → s.toSubst.ftvAt y = [y]
  | (false, α), hy => by
      show Ty.sortedFtv (tyLookup α s.ty) = _
      rw [tyLookup_not_mem _ (fun hm => hy (Sol.mem_domS_ty hm))]
      rfl
  | (true, α), hy => by
      show Row.sortedFtv (rowLookup α s.row) = _
      rw [rowLookup_not_mem _ (fun hm => hy (Sol.mem_domS_row hm))]
      rfl

-- ⊢ one unfolding step strictly drops the rank of every BOUND occurrence
theorem Sol.ftvAt_rank {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    (hty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (false, p.1))
    (hrow : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (true,  p.1))
    {x y : Bool × TyVar} (hx : x ∈ s.toSubst.ftvAt y) (hxd : x ∈ s.domS) :
    rank x < rank y := by
  obtain ⟨b, α⟩ := y
  cases b with
  | false =>
      have hx' : x ∈ Ty.sortedFtv (tyLookup α s.ty) := hx
      rcases tyLookup_cases s.ty α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
      · rw [he] at hx'
        simp only [Ty.sortedFtv, List.mem_singleton] at hx'
        subst hx'
        exact absurd (Sol.domS_ty_of_mem hxd) hnm
      · rw [h2] at hx'
        rw [← h1]
        exact hty p hm x hx' hxd
  | true =>
      have hx' : x ∈ Row.sortedFtv (rowLookup α s.row) := hx
      rcases rowLookup_cases s.row α with ⟨hnm, he⟩ | ⟨p, hm, h1, h2⟩
      · rw [he] at hx'
        simp only [Row.sortedFtv, List.mem_singleton] at hx'
        subst hx'
        exact absurd (Sol.domS_row_of_mem hxd) hnm
      · rw [h2] at hx'
        rw [← h1]
        exact hrow p hm x hx' hxd

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

-- The measure: every BOUND occurrence still left has rank below k.
def Sol.BelowL {B : Type} (s : Sol B) (rank : Bool × TyVar → Nat) (k : Nat)
    (xs : List (Bool × TyVar)) : Prop :=
  ∀ x ∈ xs, x ∈ s.domS → rank x < k

theorem Sol.below_step_row {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    (hty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (false, p.1))
    (hrow : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (true,  p.1))
    {k : Nat} {ρ : Row B}
    (h : s.BelowL rank (k + 1) (Row.sortedFtv ρ)) :
    s.BelowL rank k (Row.sortedFtv (ρ.applySubst s.toSubst)) := by
  intro x hx hxd
  obtain ⟨y, hy, hxy⟩ := Row.mem_sortedFtv_applySubst ρ hx
  by_cases hyd : y ∈ s.domS
  · have hlt := Sol.ftvAt_rank hty hrow hxy hxd
    have := h y hy hyd
    omega
  · rw [Sol.ftvAt_not_mem y hyd] at hxy
    simp only [List.mem_singleton] at hxy
    exact absurd (hxy ▸ hxd) hyd

theorem Sol.below_step_ty {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    (hty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (false, p.1))
    (hrow : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (true,  p.1))
    {k : Nat} {τ : Ty B}
    (h : s.BelowL rank (k + 1) (Ty.sortedFtv τ)) :
    s.BelowL rank k (Ty.sortedFtv (τ.applySubst s.toSubst)) := by
  intro x hx hxd
  obtain ⟨y, hy, hxy⟩ := Ty.mem_sortedFtv_applySubst τ hx
  by_cases hyd : y ∈ s.domS
  · have hlt := Sol.ftvAt_rank hty hrow hxy hxd
    have := h y hy hyd
    omega
  · rw [Sol.ftvAt_not_mem y hyd] at hxy
    simp only [List.mem_singleton] at hxy
    exact absurd (hxy ▸ hxd) hyd

theorem Sol.below_appRowN {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    (hty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (false, p.1))
    (hrow : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (true,  p.1)) :
    (k : Nat) → (ρ : Row B) → s.BelowL rank k (Row.sortedFtv ρ) →
    s.BelowL rank 0 (Row.sortedFtv (s.appRowN k ρ))
  | 0,     _, h => h
  | k + 1, ρ, h =>
      Sol.below_appRowN hty hrow k (ρ.applySubst s.toSubst) (Sol.below_step_row hty hrow h)

theorem Sol.below_appTyN {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    (hty  : ∀ p ∈ s.ty,  ∀ x ∈ Ty.sortedFtv  p.2, x ∈ s.domS → rank x < rank (false, p.1))
    (hrow : ∀ p ∈ s.row, ∀ x ∈ Row.sortedFtv p.2, x ∈ s.domS → rank x < rank (true,  p.1)) :
    (k : Nat) → (τ : Ty B) → s.BelowL rank k (Ty.sortedFtv τ) →
    s.BelowL rank 0 (Ty.sortedFtv (s.appTyN k τ))
  | 0,     _, h => h
  | k + 1, τ, h =>
      Sol.below_appTyN hty hrow k (τ.applySubst s.toSubst) (Sol.below_step_ty hty hrow h)

theorem Sol.fixed_row_of_below_zero {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    {ρ : Row B} (h : s.BelowL rank 0 (Row.sortedFtv ρ)) :
    ρ.applySubst s.toSubst = ρ := by
  refine Row.applySubst_fixed_sorted ρ (fun α hα => ?_) (fun α hα => ?_)
  · exact tyLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_ty hm); omega)
  · exact rowLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_row hm); omega)

theorem Sol.fixed_ty_of_below_zero {B : Type} {s : Sol B} {rank : Bool × TyVar → Nat}
    {τ : Ty B} (h : s.BelowL rank 0 (Ty.sortedFtv τ)) :
    τ.applySubst s.toSubst = τ := by
  refine Ty.applySubst_fixed_sorted τ (fun α hα => ?_) (fun α hα => ?_)
  · exact tyLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_ty hm); omega)
  · exact rowLookup_not_mem _ (fun hm => by have := h _ hα (Sol.mem_domS_row hm); omega)

-- ⊢ A RANKED SOLUTION HAS A CLOSURE, and |dom| rounds compute it.
-- This is what discharges `Sol.Applied`: it is no longer demanded of the
-- driver's output, it is a property of ⟦S⟧ that holds by construction.
theorem Sol.closes_closure {B : Type} {s : Sol B} (hR : s.Ranked) :
    s.Closes (s.closure s.domS.length) := by
  obtain ⟨rank, hbound, hty, hrow⟩ := hR
  have hrowfix : ∀ ρ : Row B,
      (s.appRowN s.domS.length ρ).applySubst s.toSubst = s.appRowN s.domS.length ρ :=
    fun ρ => Sol.fixed_row_of_below_zero
      (Sol.below_appRowN hty hrow s.domS.length ρ (fun x _ hxd => hbound x hxd))
  have htyfix : ∀ τ : Ty B,
      (s.appTyN s.domS.length τ).applySubst s.toSubst = s.appTyN s.domS.length τ :=
    fun τ => Sol.fixed_ty_of_below_zero
      (Sol.below_appTyN hty hrow s.domS.length τ (fun x _ hxd => hbound x hxd))
  refine ⟨fun α => ?_, fun α => ?_, fun α hα => ?_, fun α hα => ?_⟩
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


-- ⊢ a tagged occurrence is a free variable, tag forgotten
mutual
theorem Ty.mem_ftv_of_mem_sortedFtv {B : Type} {b : Bool} {α : TyVar} :
    (τ : Ty B) → (b, α) ∈ Ty.sortedFtv τ → α ∈ τ.ftv
  | .var β,  h => by
      simp only [Ty.sortedFtv, List.mem_singleton, Prod.mk.injEq] at h
      simp [Ty.ftv, h.2]
  | .base _, h => by simp [Ty.sortedFtv] at h
  | .unk,    h => by simp [Ty.sortedFtv] at h
  | .fn x y, h => by
      simp only [Ty.sortedFtv, List.mem_append] at h
      rcases h with h | h
      · exact List.mem_append_left  _ (Ty.mem_ftv_of_mem_sortedFtv x h)
      · exact List.mem_append_right _ (Ty.mem_ftv_of_mem_sortedFtv y h)
  | .rcd ρ,  h => Row.mem_ftv_of_mem_sortedFtv ρ h

theorem Row.mem_ftv_of_mem_sortedFtv {B : Type} {b : Bool} {α : TyVar} :
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
end

-- ⊢ …and a tagged binding is a binding
theorem Sol.dom_of_mem_domS {B : Type} {s : Sol B} :
    {x : Bool × TyVar} → x ∈ s.domS → x.2 ∈ s.dom
  | (false, α), h => List.mem_append_left  _ (Sol.domS_ty_of_mem  h)
  | (true,  α), h => List.mem_append_right _ (Sol.domS_row_of_mem h)

-- A legal solver state, as far as the lookup relation is concerned: ⟦S⟧ is a
-- well-formed CONTEXT (`acyclic`) and ⟦S⟧ EXISTS as a closure (`ranked`).
-- `Sol.Applied` is deliberately NOT here — see the closure section above.
structure Sol.WF {B : Type} (s : Sol B) : Prop where
  acyclic : s.Acyclic
  ranked  : s.Ranked

------------------------- IDEMPOTENCE, UNPACKED --------------------------------

-- A variable S does not bind is fixed by ⟦S⟧ at both sorts.
theorem Sol.toSubst_fixed {B : Type} {s : Sol B} {α : TyVar} (h : α ∉ s.dom) :
    s.toSubst.ty α = .var α ∧ s.toSubst.row α = .var α := by
  simp only [Sol.dom, List.mem_append, not_or] at h
  exact ⟨tyLookup_not_mem _ h.1, rowLookup_not_mem _ h.2⟩

-- A spine variable of a row is one of its free variables.
theorem mem_ftv_of_mem_sVarSeq {B : Type} {β : TyVar} :
    (ρ : Row B) → β ∈ sVarSeq ρ.toSpine → β ∈ ρ.ftv
  | .empty,    h => nomatch h
  | .var α,    h => by
      simp only [Row.toSpine, sVarSeq, List.mem_singleton] at h
      simp [Row.ftv, h]
  | .sing _ _, h => by simp only [Row.toSpine, sVarSeq] at h; exact nomatch h
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
      (fun β hβ => Sol.toSubst_fixed (h.2 p hp β hβ))⟩

-- ⊢  …and a legal solver state.  The rank is constant: `NoCapture` says no
-- binding mentions a bound variable at all, so the descent condition is
-- vacuous.
theorem Sol.wf_of_noCapture {B : Type} {s : Sol B} (h : s.NoCapture) : s.WF where
  acyclic := by
    intro p hp β hβ hmem
    have := h.2 p hp β (mem_ftv_of_mem_sVarSeq p.2 hβ)
    exact this (List.mem_append_right _ hmem)
  ranked := by
    refine ⟨fun _ => 0, fun x hx => ?_, fun p hp x hx hxd => ?_, fun p hp x hx hxd => ?_⟩
    · cases hlen : s.domS with
      | nil => rw [hlen] at hx; cases hx
      | cons _ _ => simp [hlen]
    · obtain ⟨b, α⟩ := x
      exact absurd (Sol.dom_of_mem_domS hxd)
        (h.1 p hp α (Ty.mem_ftv_of_mem_sortedFtv p.2 hx))
    · obtain ⟨b, α⟩ := x
      exact absurd (Sol.dom_of_mem_domS hxd)
        (h.2 p hp α (Row.mem_ftv_of_mem_sortedFtv p.2 hx))

------------------------- ⟦S⟧ IS A WELL-FORMED CONTEXT -------------------------

-- If every SPINE variable of ρ has rank 0 then so does ρ.  `rankUnder` is the
-- max over spine positions only — lookup never descends into a payload — so
-- this is the exact hypothesis, not an approximation of one.
theorem Row.rankUnder_eq_zero {B : Type} {rank : TyVar → Nat} :
    (ρ : Row B) → (∀ β ∈ sVarSeq ρ.toSpine, rank β = 0) → ρ.rankUnder rank = 0
  | .empty,    _ => rfl
  | .var α,    h => h α (by simp [Row.toSpine, sVarSeq])
  | .sing _ _, _ => rfl
  | .cat ρ₁ ρ₂, h => by
      have hsplit : ∀ (ρ' : Row B), (∀ β ∈ sVarSeq ρ'.toSpine, β ∈ sVarSeq (Row.cat ρ₁ ρ₂).toSpine) →
          ∀ β ∈ sVarSeq ρ'.toSpine, rank β = 0 := fun _ hin β hβ => h β (hin β hβ)
      have h1 := hsplit ρ₁ (fun β hβ => by
        rw [Row.toSpine, sVarSeq_append]; exact List.mem_append_left _ hβ)
      have h2 := hsplit ρ₂ (fun β hβ => by
        rw [Row.toSpine, sVarSeq_append]; exact List.mem_append_right _ hβ)
      simp only [Row.rankUnder, Row.rankUnder_eq_zero ρ₁ h1, Row.rankUnder_eq_zero ρ₂ h2]
      omega

-- ⊢  S acyclic  ⟹  ⟦S⟧ is RowWF
-- The rank is the two-level one: a bound variable outranks everything, and an
-- acyclic solution's bindings reach no bound variable.  This is what
-- `lookup_total` consumes, so A-sel's premise always has a derivation.
theorem Sol.rowWF_toCtx {B : Type} {s : Sol B} (hac : s.Acyclic) :
    s.toCtx.RowWF := by
  refine ⟨fun α => if α ∈ s.row.map Prod.fst then 1 else 0, ?_⟩
  intro α ρ hα
  obtain ⟨-, hmem⟩ := Sol.lookupRow_some hα
  have hkey : α ∈ s.row.map Prod.fst := List.mem_map.2 ⟨(α, ρ), hmem, rfl⟩
  have hzero : ρ.rankUnder (fun α => if α ∈ s.row.map Prod.fst then 1 else 0) = 0 := by
    refine Row.rankUnder_eq_zero ρ (fun β hβ => ?_)
    simp [hac (α, ρ) hmem β hβ]
  simp [hzero, hkey]

-- ⟦S⟧ is total for lookup: the corollary A-sel actually uses.
theorem Sol.lookup_total_toCtx {B : Type} {s : Sol B} (hac : s.Acyclic)
    (ρ : Row B) (l : Label) : ∃ r, Lookup s.toCtx ρ l r :=
  lookup_total (Sol.rowWF_toCtx hac) ρ l

--------------------------- THE θ ↦ rowEnv BRIDGE ------------------------------

-- ⊢  S idempotent,  Γ′ has no row-solutions  ⟹
--      ⟦S⟧ ⊢ ρ.l ↓ r   implies   Γ′ ⊢ (⟦S⟧ρ).l ↓ ⟦S⟧r
--
-- Reading a solution as a CONTEXT and applying it as a SUBSTITUTION give the
-- same lookup, the result transported by the same substitution.  The result
-- must move too: `L-hit` inside a binding returns the STORED payload, and the
-- substituted row returns the substituted one.
--
-- The idempotence hypothesis is exactly what reconciles the two disciplines:
-- L-α chases the chain, applySubst substitutes once, and on a fully applied
-- solution one step IS the chain.
theorem Sol.lookup_toCtx {B : Type} {s : Sol B} {σ : TySubst B} (hcl : s.Closes σ)
    {Γ' : Ctx B} (hrow : Γ'.rowEnv = []) {ρ : Row B} {l : Label} {r : LookupRes B}
    (h : Lookup s.toCtx ρ l r) :
    Lookup Γ' (ρ.applySubst σ) l (r.applySubst σ) := by
  induction h with
  | emp => exact .emp
  | hit => exact .hit
  | miss hne => exact .miss hne
  | @var α ρ₀ _ _ hα _ ih =>
      obtain ⟨hrl, -⟩ := Sol.lookupRow_some hα
      have hstep : (Row.var α).applySubst σ = ρ₀.applySubst σ := by
        show σ.row α = _
        rw [hcl.2.1 α, show s.toSubst.row α = ρ₀ from hrl]
      rw [hstep]
      exact ih
  | @varFree α _ hα =>
      have hne : σ.row α = .var α := hcl.2.2.2 α (Sol.lookupRow_none hα)
      simp only [Row.applySubst, LookupRes.applySubst]
      rw [hne]
      exact .varFree (by simp [Ctx.lookupRow, hrow])
  | catHit _ ih => exact .catHit ih
  | catSkip _ _ ih₁ ih₂ => exact .catSkip ih₁ ih₂
  | catUnk _ ih => exact .catUnk ih

-- The same fact as an equivalence: every lookup on the SUBSTITUTED row is the
-- image of one performed in ⟦S⟧.  Backward direction by totality of ⟦S⟧
-- (`Sol.rowWF_toCtx`) plus `lookup_det`.
theorem Sol.lookup_toCtx_iff {B : Type} {s : Sol B} {σ : TySubst B}
    (hac : s.Acyclic) (hcl : s.Closes σ) {Γ' : Ctx B}
    (hrow : Γ'.rowEnv = []) (ρ : Row B) (l : Label) (r' : LookupRes B) :
    (∃ r, Lookup s.toCtx ρ l r ∧ r.applySubst σ = r') ↔
      Lookup Γ' (ρ.applySubst σ) l r' := by
  constructor
  · rintro ⟨r, hr, rfl⟩
    exact Sol.lookup_toCtx hcl hrow hr
  · intro h
    obtain ⟨r, hr⟩ := Sol.lookup_total_toCtx hac ρ l
    exact ⟨r, hr, lookup_det (Sol.lookup_toCtx hcl hrow hr) h⟩

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
def UnifyWF (B : Type) [DecidableEq B] : Prop :=
  ∀ (fuel : Nat) (ρ₁ ρ₂ : Row B) (s : Sol B) (S' : Supply),
    unifyRowM fuel ρ₁ ρ₂ = .success s S' → s.WF

-- The base case, for free.
theorem Sol.nil_wf {B : Type} : (Sol.nil : Sol B).WF where
  acyclic := by intro p hp; simp [Sol.nil] at hp
  ranked := ⟨fun _ => 0, fun x hx => by simp [Sol.domS, Sol.nil] at hx,
             fun p hp => by simp [Sol.nil] at hp, fun p hp => by simp [Sol.nil] at hp⟩

-- ⊢  a legal solver state HAS its ⟦S⟧, and |dom| rounds of substitution are it.
theorem Sol.closes_of_wf {B : Type} {s : Sol B} (hwf : s.WF) :
    s.Closes (s.closure s.domS.length) :=
  Sol.closes_closure hwf.ranked

end MinimalCalculus
