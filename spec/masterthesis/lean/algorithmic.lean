-- Lean 4 formalization of masterthesis/algorithmic.typ — the L2 layer:
-- qualified (stump-carrying) schemes and instantiation-with-discharge.
--
-- Builds on minimal.lean: the declarative system, the lookup
-- metatheory and the L1 refutation (no_plain_principal_scheme) are imported,
-- never touched. QScheme is defined afresh instead of retrofitting a
-- constraint field into Scheme — the declarative system never carries
-- constraints; only the algorithmic layer does. The seam between the files
-- is Scheme.toQ (a plain scheme is a qualified scheme with Q = ∅).
--
-- Compile:  lean minimal.lean -o minimal.olean
--           LEAN_PATH=. lean algorithmic.lean
--
-- Contents:
--   Stump, QScheme       σ := ∀ᾱ. Q ⇒ τ,   Q := { ⟨ρ.l ↓ δ⟩, … }
--   Stump.Discharge      D-hit / D-⊥ / D-?  (declarative: still-? stays ★)
--   QScheme.Inst         σ ≥_Γ τ — instantiation-with-discharge
--   discharge metatheory determinism (via lookup_det) and definite-stability
--                        (via lookup_mono): "a resolved stump never re-checks"
--   principality bookend selQ = ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ is instance-closed
--                        while covering both the found- and the ⊥-typing of
--                        λx. x.l — exactly what no_plain_principal_scheme
--                        proved impossible for plain schemes; the manufactured
--                        mixed instance {ε} → {ε} is blocked by discharge.
--   ≈-characterization   rows mod ≈ ARE spines factored by vars into
--                        segments: ρ₁ ≈ ρ₂ iff same var sequence ∧ all
--                        l-projections pointwise-≈ at equal segment indices
--                        (rowEquiv_iff_char — the trace-monoid presentation
--                        behind ≐ᵣ).
--   cancellativity       shared prefixes/suffixes cancel — end-vars
--                        (cancel_var_left/right, U-var-refl) and in full
--                        generality (cancel_cat_left/right, the trace-monoid
--                        theorem the trichotomy builds on); ground rows
--                        degenerate to the projections alone (ground_char,
--                        ≐ᵣ's ground-completeness workhorse).
--   ≐ᵣ regressions       the shared-tail pitfall has NO unifier
--                        (shared_tail_no_unifier), the LUtail example's
--                        unifiers are exactly θα ≈ ε (lutail_unifier_iff),
--                        and Wand's ambiguity (β | α) ≐ᵣ (l: 𝓫) is solvable
--                        (wand_unifiable) yet has NO mgu (wand_no_mgu) —
--                        trichotomy case (c) is real. The three worked
--                        examples of algorithmic.typ, mechanized.
--   QTyped               the L2 typing relation: QCtx binds qualified
--                        schemes, qVar instantiates via ≥_Γ, qLet closes
--                        over discharged instances. Typed embeds (Typed.toQ
--                        — L2 extends the declarative system), and the
--                        two-use program (qtyped_two_use) types one binding
--                        at found- AND ⊥-instances simultaneously — beyond
--                        any plain scheme.
--   unifyRow (≐ᵣ)        the row unification algorithm, executable: forced
--                        steps only (end-var stripping, occurs-checked var
--                        solving, two-ended window matching, U-ε-var,
--                        U-ground counting, global projection clash, stuck).
--                        The worked examples run as kernel-checked rfl
--                        regressions (unify_shared_tail → clash,
--                        unify_lutail → mgu α ≔ ε, unify_wand → stuck, …).

import minimal

namespace MinimalCalculus

----------------------------------- STUMPS -----------------------------------
-- A stump ⟨ρ.l ↓ δ⟩ is a parked selection: the lookup of l in ρ blocked on a
-- row-variable, and δ is the *result variable* standing for whatever the
-- lookup will turn out to be. Algorithmically δ keeps the selection's result
-- position writable; declaratively a stump is a constraint on the scheme.

structure Stump (B : Type) where
  row   : Row B
  label : Label
  res   : TyVar

-- ## Qualified schemes  σ := ∀ᾱ. Q ⇒ τ
-- A plain HM scheme is the special case Q = ∅ (Scheme.toQ below). The result
-- variables δ are drawn from vars like every other quantified variable; the
-- constraint pins their image at instantiation time instead of freezing them
-- at generalization time (which is L1, refuted in minimal.lean).

structure QScheme (B : Type) where
  vars        : List TyVar
  constraints : List (Stump B)
  body        : Ty B

def Scheme.toQ {B : Type} (σ : Scheme B) : QScheme B :=
  ⟨σ.vars, [], σ.body⟩


---------------------------------- DISCHARGE ----------------------------------
-- Γ ⊢ (θρ).l ↓ r  replayed per instantiation θ [algorithmic.typ, D-rules]:
--
--   D-hit    r = τ_r  ⟹  θδ = τ_r     (the T-sel moment)
--   D-⊥      r = ⊥    ⟹  θδ = ★      (T-sel-⊥; W-flag on the algo side)
--   D-?      r = ?    ⟹  θδ = ★      (T-sel-★: still-unknown stays blurred;
--                                       algorithmically this case re-parks
--                                       instead — only finalization commits ★)

inductive Stump.Discharge {B : Type} (Γ : Ctx B) (θ : TySubst B) (s : Stump B) :
    Prop where
  | hit {τ : Ty B} :
      Lookup Γ (s.row.applySubst θ) s.label (.found τ) →
      θ.ty s.res = τ → Discharge Γ θ s
  | abs :
      Lookup Γ (s.row.applySubst θ) s.label .absent →
      θ.ty s.res = .unk → Discharge Γ θ s
  | unk :
      Lookup Γ (s.row.applySubst θ) s.label .unknown →
      θ.ty s.res = .unk → Discharge Γ θ s

-- ## Instantiation-with-discharge  σ ≥_Γ τ
-- Replaces Scheme.Inst at (a future qualified) T-var. Γ-relative — the price
-- of cross-instantiation refinement: discharge reads Γ's row-solutions.

def QScheme.Inst {B : Type} (Γ : Ctx B) (σ : QScheme B) (τ : Ty B) : Prop :=
  ∃ θ : TySubst B, θ.FixedOutside σ.vars ∧
    (∀ s ∈ σ.constraints, s.Discharge Γ θ) ∧
    σ.body.applySubst θ = τ

-- Plain schemes embed: with Q = ∅ the discharge condition is vacuous and
-- ≥_Γ degenerates to the Γ-independent Scheme.Inst. This is the seam between
-- the two files — everything minimal.lean knows about plain schemes lifts
-- across this equivalence.
theorem QScheme.inst_toQ {B : Type} {Γ : Ctx B} {σ : Scheme B} {τ : Ty B} :
    QScheme.Inst Γ σ.toQ τ ↔ σ.Inst τ := by
  constructor
  · rintro ⟨θ, hfix, -, hbody⟩
    exact ⟨θ, hfix, hbody⟩
  · rintro ⟨θ, hfix, hbody⟩
    exact ⟨θ, hfix, fun s hs => absurd hs List.not_mem_nil, hbody⟩

-- Monotype qualified schemes instantiate only to themselves.
theorem QScheme.Inst.mono {B : Type} {Γ : Ctx B} {τ₁ τ : Ty B}
    (h : QScheme.Inst Γ ⟨[], [], τ₁⟩ τ) : τ = τ₁ :=
  Scheme.Inst.mono ((QScheme.inst_toQ (σ := ⟨[], τ₁⟩)).mp h)


---------------------------- DISCHARGE METATHEORY -----------------------------

-- Determinism: two discharges of the same stump that substitute the row the
-- same way pin the result variable to the same type (lookup_det lifted).
-- This is what makes ≥_Γ a well-defined relation per θ rather than a choice.
theorem Stump.Discharge.det {B : Type} {Γ : Ctx B} {θ₁ θ₂ : TySubst B}
    {s : Stump B} (h₁ : s.Discharge Γ θ₁) (h₂ : s.Discharge Γ θ₂)
    (hrow : s.row.applySubst θ₁ = s.row.applySubst θ₂) :
    θ₁.ty s.res = θ₂.ty s.res := by
  cases h₁ with
  | hit hl₁ hδ₁ =>
      rw [hrow] at hl₁
      cases h₂ with
      | hit hl₂ hδ₂ => cases lookup_det hl₁ hl₂; rw [hδ₁, hδ₂]
      | abs hl₂ _   => cases lookup_det hl₁ hl₂
      | unk hl₂ _   => cases lookup_det hl₁ hl₂
  | abs hl₁ hδ₁ =>
      rw [hrow] at hl₁
      cases h₂ with
      | hit hl₂ _   => cases lookup_det hl₁ hl₂
      | abs _ hδ₂   => rw [hδ₁, hδ₂]
      | unk _ hδ₂   => rw [hδ₁, hδ₂]
  | unk hl₁ hδ₁ =>
      rw [hrow] at hl₁
      cases h₂ with
      | hit hl₂ _   => cases lookup_det hl₁ hl₂
      | abs _ hδ₂   => rw [hδ₁, hδ₂]
      | unk _ hδ₂   => rw [hδ₁, hδ₂]

-- Definite-stability: a discharge whose lookup came out definite (τ/⊥)
-- transports to every row-extension of Γ (lookup_mono lifted). This is the
-- algorithmic "a resolved stump NEVER needs re-checking" — wake-up lists
-- never contain resolved stumps, no fixpoint iteration. The ?-case is
-- deliberately NOT stable: wake-up exists precisely to improve it.
theorem Stump.Discharge.mono_of_definite {B : Type} {Γ Γ' : Ctx B}
    {θ : TySubst B} {s : Stump B} (hext : Ctx.RowExt Γ Γ')
    (h : s.Discharge Γ θ)
    (hdef : ¬ Lookup Γ (s.row.applySubst θ) s.label .unknown) :
    s.Discharge Γ' θ := by
  cases h with
  | hit hl hδ => exact .hit (lookup_mono hext hl (by intro h; cases h)) hδ
  | abs hl hδ => exact .abs (lookup_mono hext hl (by intro h; cases h)) hδ
  | unk hl _  => exact absurd hl hdef

-- Collapse of a lookup result into the discharged type: found τ ↦ τ, both
-- ⊥ and ? ↦ ★. The ?-arm is the declarative face of *finalization*.
def LookupRes.collapse {B : Type} : LookupRes B → Ty B
  | .found τ => τ
  | .absent  => .unk
  | .unknown => .unk


------------------- THE PRINCIPAL QUALIFIED SCHEME OF λx. x.l ------------------
-- minimal.lean proved (no_plain_principal_scheme) that NO plain ∀ᾱ.τ scheme
-- is instance-closed while covering both
--     λx. x.l : {(l: τ₀)} → τ₀        (found-typing, every τ₀)
--     λx. x.l : {ε} → ★              (⊥-typing)
-- The qualified scheme  ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ  does exactly that: the
-- result position δ stays writable per instance and discharge pins it to the
-- lookup's verdict, so the mixed instance {ε} → {ε} that broke every plain
-- candidate is not an instance here.

-- λx. x.l  (selEx is private in minimal.lean; restated)
def selEx (C : Type) : Expr C := .lam "x" (.sel (.var "x") "l")

-- ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
def selQ (B : Type) : QScheme B :=
  ⟨["β", "δ"],
   [⟨.var "β", "l", "δ"⟩],
   .fn (.rcd (.var "β")) (.var "δ")⟩

-- The worked-example table of algorithmic.typ in one statement: ANY lookup
-- verdict on the argument row yields the corresponding instance —
--   Γ ⊢ ρ.l ↓ found τ_r  ⟹  {ρ} → τ_r      (what L1 loses)
--   Γ ⊢ ρ.l ↓ ⊥          ⟹  {ρ} → ★
--   Γ ⊢ ρ.l ↓ ?          ⟹  {ρ} → ★
theorem selQ_inst_of_lookup {B : Type} {Γ : Ctx B} {ρ : Row B}
    {r : LookupRes B} (h : Lookup Γ ρ "l" r) :
    QScheme.Inst Γ (selQ B) (.fn (.rcd ρ) r.collapse) := by
  refine ⟨⟨fun γ => if γ = "δ" then r.collapse else .var γ,
           fun γ => if γ = "β" then ρ else .var γ⟩,
          ⟨fun γ hγ => ?_, fun γ hγ => ?_⟩, fun s hs => ?_, ?_⟩
  · have hne : γ ≠ "δ" := by rintro rfl; exact hγ (by simp [selQ])
    simp [hne]
  · have hne : γ ≠ "β" := by rintro rfl; exact hγ (by simp [selQ])
    simp [hne]
  · simp only [selQ, List.mem_singleton] at hs
    subst hs
    cases r with
    | found τ => exact .hit (by simpa [Row.applySubst] using h)
                            (by simp [LookupRes.collapse])
    | absent  => exact .abs (by simpa [Row.applySubst] using h)
                            (by simp [LookupRes.collapse])
    | unknown => exact .unk (by simpa [Row.applySubst] using h)
                            (by simp [LookupRes.collapse])
  · simp [selQ, Ty.applySubst, Row.applySubst]

-- The found-typing family is covered (τ₀ arbitrary — the typings L1's frozen
-- ★ could never reach, cf. finalized_no_blur):
theorem selQ_inst_found {B : Type} (Γ : Ctx B) (τ₀ : Ty B) :
    QScheme.Inst Γ (selQ B) (.fn (.rcd (.sing "l" τ₀)) τ₀) :=
  selQ_inst_of_lookup (r := .found τ₀) .hit

-- ... and so is the ⊥-typing:
theorem selQ_inst_absent {B : Type} (Γ : Ctx B) :
    QScheme.Inst Γ (selQ B) (.fn (.rcd .empty) .unk) :=
  selQ_inst_of_lookup (r := .absent) .emp

-- The mixed instance {ε} → {ε} — the one every plain scheme was forced to
-- admit (no_plain_principal_scheme's contradiction) — is NOT an instance:
-- θ must send β to ε, the lookup on ε is definitely ⊥, and discharge then
-- pins δ at ★, never at {ε}. Discharge is exactly the mechanism that plugs
-- the instance-closedness leak.
theorem selQ_no_mixed {B : Type} :
    ¬ QScheme.Inst Ctx.empty (selQ B)
        (.fn (.rcd .empty) (.rcd (.empty : Row B))) := by
  rintro ⟨θ, -, hQ, hbody⟩
  simp only [selQ, Ty.applySubst, Row.applySubst] at hbody
  injection hbody with hdom hres
  injection hdom with hβ
  have hs := hQ ⟨.var "β", "l", "δ"⟩ (by simp [selQ])
  cases hs with
  | hit hl _ =>
      simp only [Row.applySubst] at hl
      rw [hβ] at hl
      cases hl
  | abs _ hδ => rw [hδ] at hres; cases hres
  | unk _ hδ => rw [hδ] at hres; cases hres

-- Instance-closedness: EVERY ≥_Γ-instance of selQ is a declarative typing of
-- λx. x.l — in any context Γ. The three discharge cases replay exactly
-- T-sel / T-sel-⊥ / T-sel-★; the Lean regression proof of minimal.lean was
-- already this case split, per instance.
theorem selQ_instance_closed {B C : Type} (constTy : C → B) (Γ : Ctx B) :
    ∀ τ, QScheme.Inst Γ (selQ B) τ → Typed constTy Γ (selEx C) τ := by
  rintro τ ⟨θ, -, hQ, hbody⟩
  simp only [selQ, Ty.applySubst, Row.applySubst] at hbody
  subst hbody
  have hs := hQ ⟨.var "β", "l", "δ"⟩ (by simp [selQ])
  -- the λ-bound variable types at its annotation …
  have hvar : Typed constTy (Γ.bindTy "x" (.rcd (θ.row "β")))
      (.var "x" : Expr C) (.rcd (θ.row "β")) :=
    .tVar (by simp [Ctx.lookup_bindTy]) (Scheme.Inst.refl _)
  -- … and bindTy leaves the row-solutions alone, so the discharge's lookup
  -- transports under the binder.
  have hrow : ∀ α, Γ.lookupRow α =
      (Γ.bindTy "x" (.rcd (θ.row "β"))).lookupRow α := fun _ => rfl
  cases hs with
  | hit hl hδ =>
      simp only [Row.applySubst] at hl
      rw [hδ]
      exact .tLam (.tSel hvar (Lookup.congr_rowEnv hrow hl))
  | abs hl hδ =>
      simp only [Row.applySubst] at hl
      rw [hδ]
      exact .tLam (.tSelAbs hvar (Lookup.congr_rowEnv hrow hl))
  | unk hl hδ =>
      simp only [Row.applySubst] at hl
      rw [hδ]
      exact .tLam (.tSelUnk hvar (Lookup.congr_rowEnv hrow hl))

-- The bookend, stated in the exact shape whose plain-scheme version
-- no_plain_principal_scheme refutes: a QUALIFIED scheme CAN be
-- simultaneously instance-closed and cover both typings. Plain schemes
-- cannot (minimal.lean); qualified schemes are therefore not an optional
-- refinement but the necessary form of let-generalization for a calculus
-- with lookup-stumps.
theorem qualified_principal_scheme {B C : Type} (constTy : C → B) :
    ∃ σ : QScheme B,
      (∀ τ, QScheme.Inst Ctx.empty σ τ →
        Typed constTy Ctx.empty (selEx C) τ) ∧
      QScheme.Inst Ctx.empty σ
        (.fn (.rcd (.sing "l" (.rcd .empty))) (.rcd .empty)) ∧
      QScheme.Inst Ctx.empty σ (.fn (.rcd .empty) .unk) :=
  ⟨selQ B, selQ_instance_closed constTy Ctx.empty,
   selQ_inst_found Ctx.empty (.rcd .empty), selQ_inst_absent Ctx.empty⟩


------------------------- THE ≈-CHARACTERIZATION ------------------------------
-- [algorithmic.typ, Row unification] Rows mod ≈-assoc/units are SPINES (lists
-- of atoms a := l: τ | α); a spine factors into segments (var-free runs)
-- separated by vars. ≈-comm swaps adjacent DISTINCT labels only, so within a
-- segment distinct labels commute freely while equal labels keep their
-- relative order, and nothing crosses a var. Characterization:
--
--   ρ₁ ≈ ρ₂   iff   same var sequence   ∧   ∀ l, the l-projections agree
--                   pointwise (equal segment index, ≈-equivalent types)
--
-- The l-projection records, for every l-field in row order, its segment index
-- (= number of vars strictly before it) and its type. This is the
-- trace-monoid presentation; cancellativity of shared end-vars — what makes
-- U-var-refl sound AND complete — falls out as a corollary.

inductive Atom (B : Type) : Type where
  | field : Label → Ty B → Atom B
  | var   : TyVar → Atom B

def Row.toSpine {B : Type} : Row B → List (Atom B)
  | .empty     => []
  | .var α     => [.var α]
  | .sing l τ  => [.field l τ]
  | .cat ρ₁ ρ₂ => ρ₁.toSpine ++ ρ₂.toSpine

-- Fold a spine back into a right-nested row (the ≈-normal shape).
def ofSpine {B : Type} : List (Atom B) → Row B
  | []               => .empty
  | .field l τ :: s  => .cat (.sing l τ) (ofSpine s)
  | .var α :: s      => .cat (.var α) (ofSpine s)

-- Invariant 1: the var sequence.
def sVarSeq {B : Type} : List (Atom B) → List TyVar
  | []              => []
  | .field _ _ :: s => sVarSeq s
  | .var α :: s     => α :: sVarSeq s

-- Invariant 2: the l-projection — (segment index, type) per l-field, in order.
def sProj {B : Type} (l : Label) : List (Atom B) → List (Nat × Ty B)
  | []               => []
  | .field l' τ :: s => if l' = l then (0, τ) :: sProj l s else sProj l s
  | .var _ :: s      => (sProj l s).map (fun p => (p.1 + 1, p.2))

theorem sVarSeq_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    sVarSeq (s₁ ++ s₂) = sVarSeq s₁ ++ sVarSeq s₂
  | [], _ => rfl
  | .field _ _ :: s₁, s₂ => sVarSeq_append s₁ s₂
  | .var _ :: s₁, s₂ => congrArg (_ :: ·) (sVarSeq_append s₁ s₂)

theorem sProj_append {B : Type} (l : Label) : (s₁ s₂ : List (Atom B)) →
    sProj l (s₁ ++ s₂) =
      sProj l s₁ ++ (sProj l s₂).map (fun p => (p.1 + (sVarSeq s₁).length, p.2))
  | [], s₂ => by simp [sProj, sVarSeq]
  | .field l' τ :: s₁, s₂ => by
      by_cases h : l' = l <;>
        simp [sProj, sVarSeq, h, sProj_append l s₁ s₂]
  | .var _ :: s₁, s₂ => by
      simp [sProj, sVarSeq, sProj_append l s₁ s₂, List.map_map,
            Function.comp, Nat.add_assoc]

-- Pointwise equivalence of projections: equal segment indices, ≈-equal types.
inductive ProjEquiv {B : Type} : List (Nat × Ty B) → List (Nat × Ty B) → Prop where
  | nil  : ProjEquiv [] []
  | cons : n₁ = n₂ → TyEquiv τ₁ τ₂ → ProjEquiv ps qs →
           ProjEquiv ((n₁, τ₁) :: ps) ((n₂, τ₂) :: qs)

theorem ProjEquiv.refl {B : Type} : (ps : List (Nat × Ty B)) → ProjEquiv ps ps
  | [] => .nil
  | (_, τ) :: ps => .cons rfl (.refl τ) (ProjEquiv.refl ps)

theorem ProjEquiv.of_eq {B : Type} {ps qs : List (Nat × Ty B)} (h : ps = qs) :
    ProjEquiv ps qs := h ▸ ProjEquiv.refl ps

theorem ProjEquiv.symm {B : Type} {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv qs ps
  | .nil => .nil
  | .cons hn hty h => .cons hn.symm hty.symm h.symm

theorem ProjEquiv.trans {B : Type} {ps qs rs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv qs rs → ProjEquiv ps rs
  | .nil, h => h
  | .cons hn hty h, .cons hn' hty' h' =>
      .cons (hn.trans hn') (hty.trans hty') (h.trans h')

theorem ProjEquiv.append {B : Type} {ps qs ps' qs' : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv ps' qs' → ProjEquiv (ps ++ ps') (qs ++ qs')
  | .nil, h => h
  | .cons hn hty h, h' => .cons hn hty (h.append h')

theorem ProjEquiv.mapShift {B : Type} (k : Nat) {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs →
    ProjEquiv (ps.map (fun p => (p.1 + k, p.2))) (qs.map (fun p => (p.1 + k, p.2)))
  | .nil => .nil
  | .cons hn hty h => .cons (by rw [hn]) hty (ProjEquiv.mapShift k h)

theorem ProjEquiv.nil_inv {B : Type} {qs : List (Nat × Ty B)}
    (h : ProjEquiv [] qs) : qs = [] := by cases h; rfl

theorem ProjEquiv.cons_inv {B : Type} {n : Nat} {τ : Ty B}
    {ps qs : List (Nat × Ty B)} (h : ProjEquiv ((n, τ) :: ps) qs) :
    ∃ τ' rest, qs = (n, τ') :: rest ∧ TyEquiv τ τ' ∧ ProjEquiv ps rest := by
  cases h with
  | cons hn hty h' => exact ⟨_, _, by rw [← hn], hty, h'⟩

-- Un-shifting: the segment indices are injective in +1 (used to strip a
-- leading var off both sides).
theorem ProjEquiv.unshift {B : Type} :
    {ps qs : List (Nat × Ty B)} →
    ProjEquiv (ps.map (fun p => (p.1 + 1, p.2))) (qs.map (fun p => (p.1 + 1, p.2))) →
    ProjEquiv ps qs
  | [], [], _ => .nil
  | [], _ :: _, h => nomatch h
  | _ :: _, [], h => nomatch h
  | _ :: _, _ :: _, h => by
      cases h with
      | cons hn hty h' =>
          exact .cons (Nat.add_right_cancel hn) hty (ProjEquiv.unshift h')

-- A shifted projection never starts at segment index 0 (a spine that leads
-- with a var has an empty first segment).
theorem ProjEquiv.no_zero_head {B : Type} {qs : List (Nat × Ty B)} {τ : Ty B} :
    (ps : List (Nat × Ty B)) →
    ¬ ProjEquiv (ps.map (fun p => (p.1 + 1, p.2))) ((0, τ) :: qs)
  | [], h => nomatch h
  | _ :: _, h => by
      cases h with
      | cons hn _ _ => exact Nat.succ_ne_zero _ hn

theorem map_shift_ne_zero_head {B : Type} {ps qs : List (Nat × Ty B)} {τ : Ty B}
    (h : ps.map (fun p => (p.1 + 1, p.2)) = (0, τ) :: qs) : False := by
  cases ps with
  | nil => exact nomatch h
  | cons p ps =>
      injection h with hhead _
      injection hhead with h0 _
      exact Nat.succ_ne_zero _ h0

-- ## The characterization predicate
def Row.Char {B : Type} (ρ₁ ρ₂ : Row B) : Prop :=
  sVarSeq ρ₁.toSpine = sVarSeq ρ₂.toSpine ∧
  ∀ l, ProjEquiv (sProj l ρ₁.toSpine) (sProj l ρ₂.toSpine)

theorem Row.Char.of_eq {B : Type} {ρ₁ ρ₂ : Row B}
    (h : ρ₁.toSpine = ρ₂.toSpine) : Row.Char ρ₁ ρ₂ :=
  ⟨by rw [h], fun _ => .of_eq (by rw [h])⟩

-- ## Soundness: ≈ preserves both invariants
-- Every ≈-axiom is invariant-preserving: assoc/units don't move atoms
-- (spines are EQUAL), comm swaps distinct labels (all projections are equal
-- lists), congruence is pointwise, and no rule crosses a var.
theorem RowEquiv.char {B : Type} : {ρ₁ ρ₂ : Row B} → ρ₁ ≈ᵣ ρ₂ → Row.Char ρ₁ ρ₂
  | _, _, .refl _ => ⟨rfl, fun _ => .refl _⟩
  | _, _, .symm h =>
      ⟨(RowEquiv.char h).1.symm, fun l => ((RowEquiv.char h).2 l).symm⟩
  | _, _, .trans h₁ h₂ =>
      ⟨(RowEquiv.char h₁).1.trans (RowEquiv.char h₂).1,
       fun l => ((RowEquiv.char h₁).2 l).trans ((RowEquiv.char h₂).2 l)⟩
  | .sing l τ₁, .sing _ τ₂, .sing hty =>
      ⟨rfl, fun l' => by
        by_cases h : l = l'
        · simp only [Row.toSpine, sProj, if_pos h]
          exact .cons rfl hty .nil
        · simp only [Row.toSpine, sProj, if_neg h]
          exact .nil⟩
  | .cat ρ₁ ρ₂, .cat ρ₁' ρ₂', .cat h₁ h₂ => by
      obtain ⟨hv₁, hp₁⟩ := RowEquiv.char h₁
      obtain ⟨hv₂, hp₂⟩ := RowEquiv.char h₂
      refine ⟨?_, fun l => ?_⟩
      · simp only [Row.toSpine, sVarSeq_append, hv₁, hv₂]
      · simp only [Row.toSpine, sProj_append, hv₁]
        exact (hp₁ l).append ((hp₂ l).mapShift _)
  | _, _, .assoc => Row.Char.of_eq (List.append_assoc _ _ _)
  | _, _, .unitL => Row.Char.of_eq (List.nil_append _)
  | _, _, .unitR => Row.Char.of_eq (List.append_nil _)
  | .cat (.sing l₁ τ₁) (.sing l₂ τ₂), _, .comm hne =>
      ⟨rfl, fun l => by
        by_cases h₁ : l₁ = l <;> by_cases h₂ : l₂ = l
        · exact absurd (h₁.trans h₂.symm) hne
        · simp only [Row.toSpine, List.cons_append, List.nil_append, sProj,
                     if_pos h₁, if_neg h₂]
          exact .refl _
        · simp only [Row.toSpine, List.cons_append, List.nil_append, sProj,
                     if_neg h₁, if_pos h₂]
          exact .refl _
        · simp only [Row.toSpine, List.cons_append, List.nil_append, sProj,
                     if_neg h₁, if_neg h₂]
          exact .nil⟩

-- ## Refold: every row is ≈ to its right-nested spine form
-- (assoc + units are exactly the axioms this consumes).
theorem ofSpine_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    RowEquiv (ofSpine (s₁ ++ s₂)) (.cat (ofSpine s₁) (ofSpine s₂))
  | [], _ => RowEquiv.unitL.symm
  | .field _ _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm
  | .var _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm

theorem Row.toSpine_equiv {B : Type} : (ρ : Row B) → RowEquiv ρ (ofSpine ρ.toSpine)
  | .empty     => .refl _
  | .var _     => RowEquiv.unitR.symm
  | .sing _ _  => RowEquiv.unitR.symm
  | .cat ρ₁ ρ₂ =>
      (RowEquiv.cat (toSpine_equiv ρ₁) (toSpine_equiv ρ₂)).trans
        (ofSpine_append _ _).symm

-- ## Extraction
-- If the l-projection of a spine starts at segment index 0, the first l-field
-- sits in the leading segment: everything before it is a field with a
-- DIFFERENT label (index 0 ⟹ no preceding var; first l-occurrence ⟹ no
-- preceding l-field), so ≈-comm bubbles it to the front. All other
-- invariants are untouched — removing a field crosses no var.
theorem spine_extract {B : Type} {τ : Ty B} {rest : List (Nat × Ty B)} :
    (s : List (Atom B)) → (l : Label) →
    sProj l s = (0, τ) :: rest →
    ∃ t, RowEquiv (ofSpine s) (.cat (.sing l τ) (ofSpine t)) ∧
         sProj l t = rest ∧
         (∀ l', l' ≠ l → sProj l' t = sProj l' s) ∧
         sVarSeq t = sVarSeq s
  | [], l, h => by simp [sProj] at h
  | .var β :: s', l, h => by
      simp only [sProj] at h
      exact (map_shift_ne_zero_head h).elim
  | .field l₀ τ₀ :: s', l, h => by
      by_cases hl : l₀ = l
      · rw [hl] at h ⊢
        simp [sProj] at h
        obtain ⟨hτ, hrest⟩ := h
        subst hτ
        exact ⟨s', .refl _, hrest,
          fun l' hl' => by
            have hne : ¬ l = l' := fun hh => hl' hh.symm
            simp [sProj, if_neg hne],
          rfl⟩
      · have hEq : sProj l (.field l₀ τ₀ :: s') = sProj l s' := by
          simp [sProj, if_neg hl]
        rw [hEq] at h
        obtain ⟨t', hequiv, hproj, hothers, hvars⟩ := spine_extract s' l h
        refine ⟨.field l₀ τ₀ :: t', ?_, ?_, ?_, ?_⟩
        · exact (RowEquiv.cat (.refl _) hequiv).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (.comm hl) (.refl _)).trans RowEquiv.assoc))
        · simp [sProj, if_neg hl, hproj]
        · intro l' hl'
          by_cases hl₀ : l₀ = l'
          · simp [sProj, hl₀, hothers l' hl']
          · simp [sProj, if_neg hl₀, hothers l' hl']
        · simp [sVarSeq, hvars]

-- A spine with no vars and no fields is empty.
theorem spine_nil_of {B : Type} : (s : List (Atom B)) →
    sVarSeq s = [] → (∀ l, sProj l s = []) → s = []
  | [], _, _ => rfl
  | .var _ :: _, hv, _ => nomatch hv
  | .field l _ :: _, _, hp => by have := hp l; simp [sProj] at this

-- ## Completeness: the invariants pin the spine up to ≈
-- Walk s₁ atom by atom. A leading var forces s₂'s first segment empty (no
-- index-0 projection entry), so s₂ leads with the SAME var (var sequence) —
-- strip both (unshift). A leading field l:τ is s₁'s first l-occurrence at
-- index 0, so s₂'s l-projection also starts (0, τ') with τ ≈ τ' — extract it,
-- recurse on the remainders.
theorem char_complete {B : Type} :
    (s₁ s₂ : List (Atom B)) →
    sVarSeq s₁ = sVarSeq s₂ →
    (∀ l, ProjEquiv (sProj l s₁) (sProj l s₂)) →
    RowEquiv (ofSpine s₁) (ofSpine s₂)
  | [], s₂, hv, hp => by
      rw [spine_nil_of s₂ hv.symm (fun l => (hp l).nil_inv)]
      exact .refl _
  | .var α :: t₁, s₂, hv, hp => by
      cases s₂ with
      | nil => simp [sVarSeq] at hv
      | cons a t₂ =>
          cases a with
          | field l' τ' =>
              have h := hp l'
              simp [sProj] at h
              exact absurd h (ProjEquiv.no_zero_head _)
          | var β =>
              simp only [sVarSeq] at hv
              injection hv with hαβ hv'
              subst hαβ
              exact .cat (.refl _)
                (char_complete t₁ t₂ hv' fun l => (hp l).unshift)
  | .field l τ :: t₁, s₂, hv, hp => by
      have h := hp l
      simp [sProj] at h
      obtain ⟨τ', rest, hs₂, hty, hrest⟩ := h.cons_inv
      obtain ⟨t₂, hequiv, hproj, hothers, hvars⟩ := spine_extract s₂ l hs₂
      have hv' : sVarSeq t₁ = sVarSeq t₂ := by
        simp only [sVarSeq] at hv
        rw [hv, ← hvars]
      have hp' : ∀ l', ProjEquiv (sProj l' t₁) (sProj l' t₂) := by
        intro l'
        by_cases hl : l' = l
        · subst hl
          rw [hproj]
          exact hrest
        · have h' := hp l'
          have hne : ¬ l = l' := fun hh => hl hh.symm
          simp only [sProj, if_neg hne] at h'
          rw [hothers l' hl]
          exact h'
      exact (RowEquiv.cat (.sing hty) (char_complete t₁ t₂ hv' hp')).trans
        hequiv.symm

-- ## The characterization
theorem RowEquiv.ofChar {B : Type} {ρ₁ ρ₂ : Row B} (h : Row.Char ρ₁ ρ₂) :
    ρ₁ ≈ᵣ ρ₂ :=
  ρ₁.toSpine_equiv.trans
    ((char_complete _ _ h.1 h.2).trans ρ₂.toSpine_equiv.symm)

--   ρ₁ ≈ ρ₂   iff   same var sequence ∧ pointwise-≈ l-projections
theorem rowEquiv_iff_char {B : Type} {ρ₁ ρ₂ : Row B} :
    ρ₁ ≈ᵣ ρ₂ ↔ Row.Char ρ₁ ρ₂ :=
  ⟨RowEquiv.char, RowEquiv.ofChar⟩

-- ## Cancellativity of shared end-vars
-- The trace-monoid fact ≐ᵣ's U-var-refl rests on: stripping a shared var off
-- either end is sound (trivially, by ≈-congruence) AND complete (below) —
-- this is what replaces P&X's shared-tail side condition [Δ₂]ρ₁ = [Δ₁]ρ₁.
theorem RowEquiv.cancel_var_left {B : Type} {α : TyVar} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat (.var α) ρ₁) (.cat (.var α) ρ₂)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, List.cons_append, List.nil_append, sVarSeq] at hv
  refine RowEquiv.ofChar ⟨?_, fun l => ?_⟩
  · injection hv
  · have h' := hp l
    simp only [Row.toSpine, List.cons_append, List.nil_append, sProj] at h'
    exact h'.unshift

theorem RowEquiv.cancel_var_right {B : Type} {α : TyVar} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ₁ (.var α)) (.cat ρ₂ (.var α))) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, sVarSeq_append, sVarSeq] at hv
  have hveq : sVarSeq ρ₁.toSpine = sVarSeq ρ₂.toSpine :=
    (List.append_inj' hv rfl).1
  refine RowEquiv.ofChar ⟨hveq, fun l => ?_⟩
  have h' := hp l
  simp only [Row.toSpine, sProj_append, sProj, List.map_nil,
             List.append_nil] at h'
  exact h'

-- ## Full cancellativity (the trace-monoid theorem proper)
-- Not just end-vars: ANY shared prefix or suffix row cancels. This is Levi's
-- lemma territory — a unifier is a factorization, and factorizations against
-- a shared block are unique — and the ingredient the trichotomy's
-- stuck ⟹ no-unique-mgu direction builds on.

theorem ProjEquiv.length {B : Type} :
    {ps qs : List (Nat × Ty B)} → ProjEquiv ps qs → ps.length = qs.length
  | _, _, .nil => rfl
  | _, _, .cons _ _ h => congrArg (· + 1) h.length

theorem ProjEquiv.split {B : Type} :
    {a b c d : List (Nat × Ty B)} → ProjEquiv (a ++ c) (b ++ d) →
    a.length = b.length → ProjEquiv a b ∧ ProjEquiv c d
  | [], [], _, _, h, _ => ⟨.nil, h⟩
  | [], _ :: _, _, _, _, hl => nomatch hl
  | _ :: _, [], _, _, _, hl => nomatch hl
  | _ :: _, _ :: _, _, _, h, hl => by
      cases h with
      | cons hn hty h' =>
          have hsplit := ProjEquiv.split h' (Nat.succ.inj hl)
          exact ⟨.cons hn hty hsplit.1, hsplit.2⟩

theorem ProjEquiv.unshiftK {B : Type} (k : Nat) :
    {ps qs : List (Nat × Ty B)} →
    ProjEquiv (ps.map (fun p => (p.1 + k, p.2))) (qs.map (fun p => (p.1 + k, p.2))) →
    ProjEquiv ps qs
  | [], [], _ => .nil
  | [], _ :: _, h => nomatch h
  | _ :: _, [], h => nomatch h
  | _ :: _, _ :: _, h => by
      cases h with
      | cons hn hty h' =>
          exact .cons (Nat.add_right_cancel hn) hty (ProjEquiv.unshiftK k h')

theorem RowEquiv.cancel_cat_left {B : Type} {ρ ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ ρ₁) (.cat ρ ρ₂)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, sVarSeq_append] at hv
  refine RowEquiv.ofChar ⟨(List.append_inj hv rfl).2, fun l => ?_⟩
  have h' := hp l
  simp only [Row.toSpine, sProj_append] at h'
  exact ((ProjEquiv.split h' rfl).2).unshiftK _

theorem RowEquiv.cancel_cat_right {B : Type} {ρ ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ₁ ρ) (.cat ρ₂ ρ)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, sVarSeq_append] at hv
  have hlen : (sVarSeq ρ₁.toSpine).length = (sVarSeq ρ₂.toSpine).length := by
    have := congrArg List.length hv
    simp only [List.length_append] at this
    exact Nat.add_right_cancel this
  have hveq : sVarSeq ρ₁.toSpine = sVarSeq ρ₂.toSpine :=
    (List.append_inj hv hlen).1
  refine RowEquiv.ofChar ⟨hveq, fun l => ?_⟩
  have h' := hp l
  simp only [Row.toSpine, sProj_append, hveq] at h'
  have hplen : (sProj l ρ₁.toSpine).length = (sProj l ρ₂.toSpine).length := by
    have := h'.length
    simp only [List.length_append, List.length_map] at this
    exact Nat.add_right_cancel this
  exact (ProjEquiv.split h' hplen).1

-- ## Ground rows: the characterization degenerates to the projections
-- (the T-eq workhorse for ≐ᵣ's ground completeness).
theorem spineVarFree_iff_varSeq_nil {B : Type} :
    (ρ : Row B) → (ρ.SpineVarFree ↔ sVarSeq ρ.toSpine = [])
  | .empty => ⟨fun _ => rfl, fun _ => .empty⟩
  | .var α => ⟨(fun h => nomatch h), (fun h => nomatch h)⟩
  | .sing l τ => ⟨fun _ => rfl, fun _ => .sing⟩
  | .cat ρ₁ ρ₂ => by
      rw [show (Row.cat ρ₁ ρ₂).toSpine = ρ₁.toSpine ++ ρ₂.toSpine from rfl,
          sVarSeq_append, List.append_eq_nil_iff]
      constructor
      · intro hv
        match hv with
        | .cat h₁ h₂ =>
          exact ⟨(spineVarFree_iff_varSeq_nil ρ₁).1 h₁,
                 (spineVarFree_iff_varSeq_nil ρ₂).1 h₂⟩
      · rintro ⟨h₁, h₂⟩
        exact .cat ((spineVarFree_iff_varSeq_nil ρ₁).2 h₁)
                   ((spineVarFree_iff_varSeq_nil ρ₂).2 h₂)

theorem ground_char {B : Type} {ρ₁ ρ₂ : Row B}
    (h₁ : ρ₁.SpineVarFree) (h₂ : ρ₂.SpineVarFree) :
    ρ₁ ≈ᵣ ρ₂ ↔ ∀ l, ProjEquiv (sProj l ρ₁.toSpine) (sProj l ρ₂.toSpine) :=
  ⟨fun h => h.char.2,
   fun hp => RowEquiv.ofChar
     ⟨((spineVarFree_iff_varSeq_nil ρ₁).1 h₁).trans
        ((spineVarFree_iff_varSeq_nil ρ₂).1 h₂).symm, hp⟩⟩

-- ## The two ≐ᵣ regression examples, mechanized
-- A unifier equates the θ-images up to ≈:
def Unifies {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) : Prop :=
  RowEquiv (ρ₁.applySubst θ) (ρ₂.applySubst θ)

-- P&X's shared-tail pitfall (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α), l₁ ≠ l₂: NO unifier
-- exists — cancel the shared α, then the l₁-projections clash. U-var-refl's
-- right-cancellation rejects this correctly with no side condition.
theorem shared_tail_no_unifier {B : Type} {b : B} {l₁ l₂ : Label} {α : TyVar}
    (hne : l₁ ≠ l₂) (θ : TySubst B) :
    ¬ Unifies θ (.cat (.sing l₁ (.base b)) (.var α))
                (.cat (.sing l₂ (.base b)) (.var α)) := by
  intro h
  unfold Unifies at h
  simp only [Row.applySubst, Ty.applySubst] at h
  obtain ⟨-, hp⟩ := h.cancel_cat_right.char
  have hcontra := hp l₁
  have hne' : ¬ l₂ = l₁ := fun hh => hne hh.symm
  simp only [Row.toSpine, sProj, if_neg hne'] at hcontra
  cases hcontra

-- The LUtail-loses-solutions example (l: 𝓫) ≐ᵣ (α | l: 𝓫): θ is a unifier
-- IFF θ maps α to (something ≈) ε — the unique mgu that P&X's LUtail misses
-- by committing α to contain l. Forced two-sided processing finds it.
theorem lutail_unifier_iff {B : Type} {b : B} {l : Label} {α : TyVar}
    (θ : TySubst B) :
    Unifies θ (.sing l (.base b)) (.cat (.var α) (.sing l (.base b))) ↔
    RowEquiv (θ.row α) .empty := by
  constructor
  · intro h
    unfold Unifies at h
    simp only [Row.applySubst, Ty.applySubst] at h
    have h' : RowEquiv (.cat (θ.row α) (.sing l (.base b)))
                       (.cat .empty (.sing l (.base b))) :=
      h.symm.trans RowEquiv.unitL.symm
    exact h'.cancel_cat_right
  · intro h
    unfold Unifies
    simp only [Row.applySubst, Ty.applySubst]
    exact RowEquiv.unitL.symm.trans (RowEquiv.cat h.symm (.refl _))

-- ## Wand's ambiguity: the stuck class is real
-- (β | α) ≐ᵣ (l: 𝓫) — Wand's non-principality example in unification
-- clothing. The l-field can come from either side, so solutions EXIST but
-- are incomparable: no mgu. This is trichotomy case (c) — U-stuck fails on
-- exactly this configuration, and correctly so.

/-- θ' factors through θ:  θ' = σ ∘ θ  (mod ≈). An mgu would have every
unifier factor through it. -/
def InstanceOf {B : Type} (θ' θ : TySubst B) : Prop :=
  ∃ σ : TySubst B,
    (∀ x, RowEquiv (θ'.row x) ((θ.row x).applySubst σ)) ∧
    (∀ x, TyEquiv (θ'.ty x) ((θ.ty x).applySubst σ))

-- A ProjEquiv into a singleton splits: the entry comes from exactly one of
-- the two appended sides (per-label counting across a concatenation).
theorem ProjEquiv.append_singleton {B : Type} {p₁ p₂ : List (Nat × Ty B)}
    {n : Nat} {τ : Ty B} (h : ProjEquiv (p₁ ++ p₂) [(n, τ)]) :
    (p₁ = [] ∧ ProjEquiv p₂ [(n, τ)]) ∨ (ProjEquiv p₁ [(n, τ)] ∧ p₂ = []) := by
  cases p₁ with
  | nil => exact .inl ⟨rfl, h⟩
  | cons x rest =>
      cases h with
      | cons hn hty h' =>
          obtain ⟨hr, hp₂⟩ := List.append_eq_nil_iff.mp h'.symm.nil_inv
          subst hr
          exact .inr ⟨.cons hn hty .nil, hp₂⟩

-- A ground row whose only projection entry is (0, 𝓫) at label l IS the
-- singleton (l: 𝓫), mod ≈ (assembled from the characterization).
theorem row_equiv_sing_of_char {B : Type} {ρ : Row B} {l : Label} {b : B}
    (hv : sVarSeq ρ.toSpine = [])
    (hl : ProjEquiv (sProj l ρ.toSpine) [(0, .base b)])
    (hothers : ∀ l', l' ≠ l → sProj l' ρ.toSpine = []) :
    RowEquiv ρ (.sing l (.base b)) := by
  refine RowEquiv.ofChar ⟨?_, fun l' => ?_⟩
  · simpa [Row.toSpine, sVarSeq] using hv
  · by_cases h : l' = l
    · subst h
      simpa [Row.toSpine, sProj] using hl
    · have hne : ¬ l = l' := fun hh => h hh.symm
      rw [hothers l' h,
          show sProj l' (Row.sing l (.base b)).toSpine = [] from by
            simp [Row.toSpine, sProj, if_neg hne]]
      exact .nil

-- Solutions exist …
theorem wand_unifiable {B : Type} (b : B) (l : Label) :
    ∃ θ : TySubst B,
      Unifies θ (.cat (.var "β") (.var "α")) (.sing l (.base b)) :=
  ⟨⟨fun x => .var x,
    fun x => if x = "β" then .sing l (.base b)
             else if x = "α" then .empty else .var x⟩,
   by unfold Unifies
      simp [Row.applySubst, Ty.applySubst]
      exact RowEquiv.unitR⟩

-- … but no mgu does: a would-be mgu θ must put the l-field into θβ or θα;
-- either way, one of the two witness unifiers (field-from-β / field-from-α)
-- cannot factor through θ, because a ground singleton can never be
-- substituted into ε (⟦concrete-atom pairings are substitution-stable⟧).
theorem wand_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ ∃ θ : TySubst B,
        Unifies θ (.cat (.var "β") (.var "α")) (.sing l (.base b)) ∧
        ∀ θ' : TySubst B,
          Unifies θ' (.cat (.var "β") (.var "α")) (.sing l (.base b)) →
          InstanceOf θ' θ := by
  rintro ⟨θ, hu, hmgu⟩
  unfold Unifies at hu
  simp only [Row.applySubst, Ty.applySubst] at hu
  obtain ⟨hv, hp⟩ := hu.char
  simp only [Row.toSpine, sVarSeq_append, sVarSeq] at hv
  obtain ⟨hvβ, hvα⟩ := List.append_eq_nil_iff.mp hv
  have hpl := hp l
  simp [Row.toSpine, sProj_append, hvβ, sProj] at hpl
  have hothers : ∀ l', l' ≠ l →
      sProj l' (θ.row "β").toSpine = [] ∧ sProj l' (θ.row "α").toSpine = [] := by
    intro l' hl'
    have h' := hp l'
    have hne : ¬ l = l' := fun hh => hl' hh.symm
    simp [Row.toSpine, sProj_append, hvβ, sProj, if_neg hne] at h'
    exact List.append_eq_nil_iff.mp h'.symm.nil_inv
  rcases hpl.append_singleton with ⟨hβnil, hαl⟩ | ⟨hβl, hαnil⟩
  · -- the l-field lives in θα: the field-from-β unifier cannot factor
    have hα : RowEquiv (θ.row "α") (.sing l (.base b)) :=
      row_equiv_sing_of_char hvα hαl (fun l' hl' => (hothers l' hl').2)
    obtain ⟨σ, hrow, -⟩ := hmgu
      ⟨fun x => .var x,
       fun x => if x = "β" then .sing l (.base b)
                else if x = "α" then .empty else .var x⟩
      (by unfold Unifies
          simp [Row.applySubst, Ty.applySubst]
          exact RowEquiv.unitR)
    have hcl := hrow "α"
    simp at hcl
    have hcontra : RowEquiv (.empty : Row B) (.sing l (.base b)) :=
      hcl.trans (RowEquiv.applySubst σ hα)
    obtain ⟨-, hpc⟩ := hcontra.char
    have hfalse := hpc l
    simp [Row.toSpine, sProj] at hfalse
    cases hfalse
  · -- the l-field lives in θβ: the field-from-α unifier cannot factor
    have hβ : RowEquiv (θ.row "β") (.sing l (.base b)) :=
      row_equiv_sing_of_char hvβ hβl (fun l' hl' => (hothers l' hl').1)
    obtain ⟨σ, hrow, -⟩ := hmgu
      ⟨fun x => .var x,
       fun x => if x = "β" then .empty
                else if x = "α" then .sing l (.base b) else .var x⟩
      (by unfold Unifies
          simp [Row.applySubst, Ty.applySubst]
          exact RowEquiv.unitL)
    have hcl := hrow "β"
    simp at hcl
    have hcontra : RowEquiv (.empty : Row B) (.sing l (.base b)) :=
      hcl.trans (RowEquiv.applySubst σ hβ)
    obtain ⟨-, hpc⟩ := hcontra.char
    have hfalse := hpc l
    simp [Row.toSpine, sProj] at hfalse
    cases hfalse


--------------------------- THE L2 TYPING RELATION ----------------------------
-- The qualified declarative system [algorithmic.typ, L2]: contexts bind
-- QSchemes, T-var instantiates via ≥_Γ (instantiation-with-discharge), and
-- T-let's instance-closed premise quantifies over DISCHARGED instances only.
-- Everything else mirrors minimal.lean's Typed verbatim.
--
-- Relationship to the plain system: QTyped EXTENDS Typed (Typed.toQ below —
-- plain schemes embed with Q = ∅ and discharge vacuous), and the extension
-- is strict in precision: the two-use program at the bottom types one
-- let-binding at BOTH the found- and the ⊥-instance simultaneously — the
-- exact combination no_plain_principal_scheme proves impossible for any
-- single plain scheme.

structure QCtx (B : Type) where
  tyEnv  : List (Var × QScheme B)
  rowEnv : List (TyVar × Row B)

namespace QCtx

def lookup (Γ : QCtx B) (x : Var) : Option (QScheme B) :=
  (Γ.tyEnv.find? (·.1 == x)).map (·.2)

def bindScheme (Γ : QCtx B) (x : Var) (σ : QScheme B) : QCtx B :=
  { Γ with tyEnv := (x, σ) :: Γ.tyEnv }

def bindTy (Γ : QCtx B) (x : Var) (τ : Ty B) : QCtx B :=
  Γ.bindScheme x ⟨[], [], τ⟩

-- The row-solutions view consumed by Lookup and by discharge.
def ctx (Γ : QCtx B) : Ctx B := ⟨[], Γ.rowEnv⟩

theorem lookup_bindScheme (Γ : QCtx B) (x y : Var) (σ : QScheme B) :
    (Γ.bindScheme x σ).lookup y = if x == y then some σ else Γ.lookup y := by
  simp only [QCtx.lookup, QCtx.bindScheme, List.find?_cons]
  cases hxy : (x == y) <;> simp_all

end QCtx

mutual
  inductive QTyped {B C : Type} (constTy : C → B) :
      QCtx B → Expr C → Ty B → Prop where
    | qCon : QTyped constTy Γ (.con c) (.base (constTy c))
    -- x : σ ∈ Γ   σ ≥_Γ τ           (instantiation-with-discharge)
    | qVar : Γ.lookup x = some σ → QScheme.Inst Γ.ctx σ τ →
             QTyped constTy Γ (.var x) τ
    | qEq  : QTyped constTy Γ e τ₁ → TyEquiv τ₁ τ₂ → QTyped constTy Γ e τ₂
    | qLam : QTyped constTy (Γ.bindTy x τ₁) e τ₂ →
             QTyped constTy Γ (.lam x e) (.fn τ₁ τ₂)
    | qApp : QTyped constTy Γ e₁ (.fn τ₁ τ₂) → QTyped constTy Γ e₂ τ₁ →
             QTyped constTy Γ (.app e₁ e₂) τ₂
    -- ∀ τ₁ ≥_Γ σ.  Γ ⊢ e₁ : τ₁     (instance-closed over DISCHARGED instances)
    | qLet : (∀ τ₁, QScheme.Inst Γ.ctx σ τ₁ → QTyped constTy Γ e₁ τ₁) →
             QTyped constTy (Γ.bindScheme x σ) e₂ τ₂ →
             QTyped constTy Γ (.letE x e₁ e₂) τ₂
    | qCat : QTyped constTy Γ e₁ (.rcd ρ₁) → QTyped constTy Γ e₂ (.rcd ρ₂) →
             QTyped constTy Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))
    | qSel : QTyped constTy Γ e (.rcd ρ) → Lookup Γ.ctx ρ l (.found τ) →
             QTyped constTy Γ (.sel e l) τ
    | qSelUnk : QTyped constTy Γ e (.rcd ρ) → Lookup Γ.ctx ρ l .unknown →
                QTyped constTy Γ (.sel e l) .unk
    | qSelAbs : QTyped constTy Γ e (.rcd ρ) → Lookup Γ.ctx ρ l .absent →
                QTyped constTy Γ (.sel e l) .unk
    | qUnk : QTyped constTy Γ e τ → QTyped constTy Γ e .unk
    | qRcd : QTypedBody constTy Γ b ρ → QTyped constTy Γ (.rcd b) (.rcd ρ)

  inductive QTypedBody {B C : Type} (constTy : C → B) :
      QCtx B → RecBody (Expr C) → Row B → Prop where
    | empty : QTypedBody constTy Γ .empty .empty
    | field : QTyped constTy Γ e τ →
              QTypedBody constTy Γ (.field l e) (.sing l τ)
    | cat : QTypedBody constTy Γ b₁ ρ₁ → QTypedBody constTy Γ b₂ ρ₂ →
            QTypedBody constTy Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)
end

-- ## Embedding: every declarative typing is an L2 typing
-- Plain contexts embed by Q = ∅ everywhere; discharge is vacuous, lookups
-- transport because toQ preserves the row-solutions on the nose.

def Ctx.toQ {B : Type} (Γ : Ctx B) : QCtx B :=
  ⟨Γ.tyEnv.map (fun p => (p.1, p.2.toQ)), Γ.rowEnv⟩

theorem Ctx.toQ_lookup {B : Type} (Γ : Ctx B) (x : Var) :
    Γ.toQ.lookup x = (Γ.lookup x).map Scheme.toQ := by
  simp only [Ctx.toQ, QCtx.lookup, Ctx.lookup, List.find?_map, Option.map_map]
  rfl

mutual
theorem Typed.toQ {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} →
    Typed constTy Γ e τ → QTyped constTy Γ.toQ e τ
  | _, _, _, .tCon => .qCon
  | _, _, _, .tVar h hi =>
      .qVar (by rw [Ctx.toQ_lookup, h]; rfl) (QScheme.inst_toQ.mpr hi)
  | _, _, _, .tEq h he => .qEq (Typed.toQ h) he
  | _, _, _, .tLam h => .qLam (Typed.toQ h)
  | _, _, _, .tApp h₁ h₂ => .qApp (Typed.toQ h₁) (Typed.toQ h₂)
  | _, _, _, .tLet hprem hbody =>
      .qLet (fun τ₁ hq => Typed.toQ (hprem τ₁ (QScheme.inst_toQ.mp hq)))
            (Typed.toQ hbody)
  | _, _, _, .tCat h₁ h₂ => .qCat (Typed.toQ h₁) (Typed.toQ h₂)
  | Γ, _, _, .tSel h hl =>
      .qSel (Typed.toQ h)
        (Lookup.congr_rowEnv (Γ₁ := Γ) (Γ₂ := Γ.toQ.ctx) (fun _ => rfl) hl)
  | Γ, _, _, .tSelUnk h hl =>
      .qSelUnk (Typed.toQ h)
        (Lookup.congr_rowEnv (Γ₁ := Γ) (Γ₂ := Γ.toQ.ctx) (fun _ => rfl) hl)
  | Γ, _, _, .tSelAbs h hl =>
      .qSelAbs (Typed.toQ h)
        (Lookup.congr_rowEnv (Γ₁ := Γ) (Γ₂ := Γ.toQ.ctx) (fun _ => rfl) hl)
  | _, _, _, .tUnk h => .qUnk (Typed.toQ h)
  | _, _, _, .tRcd h => .qRcd (TypedBody.toQ h)

theorem TypedBody.toQ {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    TypedBody constTy Γ b ρ → QTypedBody constTy Γ.toQ b ρ
  | _, _, _, .empty => .empty
  | _, _, _, .field h => .field (Typed.toQ h)
  | _, _, _, .cat h₁ h₂ => .cat (TypedBody.toQ h₁) (TypedBody.toQ h₂)
end

-- ## The two-use program: L2's precision, end to end
--   let f = (x: x.l) in { a = f {l = c} | b = f {} }
--     :  { a: 𝓫_c | b: ★ }
-- ONE binding, TWO uses at incompatible refined instances — the found-typing
-- AND the ⊥-typing of the same scheme. selQ_instance_closed (lifted through
-- Typed.toQ) discharges the instance-closed premise; each use discharges its
-- own copy of the stump. By no_plain_principal_scheme, no single plain
-- scheme could serve both uses at these types.
theorem qtyped_two_use {B C : Type} (constTy : C → B) (c : C) :
    QTyped constTy ⟨[], []⟩
      (.letE "f" (selEx C)
        (.rcd (.cat
          (.field "a" (.app (.var "f") (.rcd (.field "l" (.con c)))))
          (.field "b" (.app (.var "f") (.rcd .empty))))))
      (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" .unk))) := by
  refine .qLet (σ := selQ B) (fun τ₁ hq => ?_) ?_
  · exact (selQ_instance_closed constTy _ τ₁ hq).toQ
  · refine .qRcd (.cat (.field ?_) (.field ?_))
    · exact .qApp
        (.qVar (by simp [QCtx.lookup_bindScheme])
               (selQ_inst_found _ (.base (constTy c))))
        (.qRcd (.field .qCon))
    · exact .qApp
        (.qVar (by simp [QCtx.lookup_bindScheme]) (selQ_inst_absent _))
        (.qRcd .empty)


------------------------ THE ROW UNIFICATION ALGORITHM ------------------------
-- ≐ᵣ [algorithmic.typ, Row unification], executable. Works on spines; every
-- step is FORCED (solution-set preserving):
--   * strip a shared var off either end            (U-var-refl; cancel_cat_*)
--   * solve a whole-var remainder, occurs-checked  (U-var-solve)
--   * match a leading/trailing field against the other side's window
--     (= leading/trailing segment)                 (U-field, both ends)
--   * one side exhausted: remaining vars ≔ ε,
--     remaining fields clash                       (U-ε-var)
--   * ground-side counting: if one side is var-free and a label has EQUAL
--     positive field-counts on both sides, the other side's vars are l-free
--     by counting and the pairing is positional — match first occurrences.
--     (U-ground: this is worked example 2's "var count must collapse" made
--     into an explicit rule; the window rules alone do not cover it, which
--     the mechanization surfaced.)
--   * global projection clash                      (U-clash)
--   * otherwise stuck                              (U-stuck, Wand ambiguity)
--
-- No LUtail: field demands never flow through ≐ᵣ (they park as stumps), so
-- the algorithm never guesses a field into a var. Type equations are EMITTED
-- (τ ≐ τ' pairs), not solved — the type-level driver is future work.
--
-- Presentation uses fuel (structural recursion ⟹ the algorithm computes by
-- rfl; the regressions below are kernel-checked executions). Every recursive
-- call consumes ≥ 2 atoms, so fuel |s₁| + |s₂| never runs out.

inductive URes (B : Type) : Type where
  | success : List (TyVar × Row B) → List (Ty B × Ty B) → URes B
  | clash   : URes B   -- no unifier (projection clash)
  | occurs  : URes B   -- no finite unifier (recursive row)
  | stuck   : URes B   -- solutions may exist, no unique mgu (Wand ambiguity)

def URes.addEq {B : Type} (τ τ' : Ty B) : URes B → URes B
  | .success σ eqs => .success σ ((τ, τ') :: eqs)
  | r => r

-- ## Spine measurements
def sHasVar {B : Type} : List (Atom B) → Bool
  | [] => false
  | .var _ :: _ => true
  | .field _ _ :: s => sHasVar s

def sFieldCount {B : Type} (l : Label) : List (Atom B) → Nat
  | [] => 0
  | .field l' _ :: s => (if l' = l then 1 else 0) + sFieldCount l s
  | .var _ :: s => sFieldCount l s

def sLabels {B : Type} : List (Atom B) → List Label
  | [] => []
  | .field l _ :: s => l :: sLabels s
  | .var _ :: s => sLabels s

-- U-clash, projection-based (checked globally, not per-window — cf. the
-- (β | l: Int | α) ≐ᵣ (l′: Bool) example for why).
def projClash {B : Type} (s₁ s₂ : List (Atom B)) : Bool :=
  (sLabels s₁ ++ sLabels s₂).any fun l =>
    (decide (sFieldCount l s₂ < sFieldCount l s₁) && !sHasVar s₂) ||
    (decide (sFieldCount l s₁ < sFieldCount l s₂) && !sHasVar s₁)

-- ## Move detectors
-- U-ε-var: every var of an exhausted-side remainder is forced to ε; a
-- leftover field has nowhere to come from.
def allVarsEmpty {B : Type} : List (Atom B) → Option (List (TyVar × Row B))
  | [] => some []
  | .var α :: s => (allVarsEmpty s).map ((α, Row.empty) :: ·)
  | .field _ _ :: _ => none

-- U-var-refl at the left end.
def stripL {B : Type} : List (Atom B) → List (Atom B) →
    Option (List (Atom B) × List (Atom B))
  | .var α :: t₁, .var β :: t₂ => if α = β then some (t₁, t₂) else none
  | _, _ => none

-- … and at the right end (trace monoids cancel on both sides).
def stripR {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (List (Atom B) × List (Atom B)) :=
  match stripL s₁.reverse s₂.reverse with
  | some (t₁, t₂) => some (t₁.reverse, t₂.reverse)
  | none => none

-- U-var-solve: a whole-var remainder, occurs-checked.
def solveVar {B : Type} : List (Atom B) → List (Atom B) → Option (URes B)
  | [.var α], s₂ =>
      some (if (sVarSeq s₂).contains α then .occurs
            else .success [(α, ofSpine s₂)] [])
  | _, _ => none

-- First l-field of the WINDOW (leading segment): the search stops at a var.
def windowExtract {B : Type} (l : Label) :
    List (Atom B) → Option (Ty B × List (Atom B))
  | [] => none
  | .var _ :: _ => none
  | .field l' τ :: s =>
      if l' = l then some (τ, s)
      else match windowExtract l s with
        | some (τ', s') => some (τ', .field l' τ :: s')
        | none => none

-- U-field at the left end: leading field of one side matched against the
-- first same-label occurrence in the other side's window.
def matchL {B : Type} : List (Atom B) → List (Atom B) →
    Option (Ty B × Ty B × List (Atom B) × List (Atom B))
  | .field l τ :: t₁, s₂ =>
      match windowExtract l s₂ with
      | some (τ', s₂') => some (τ, τ', t₁, s₂')
      | none => none
  | _, _ => none

-- … and at the right end.
def matchR {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (Ty B × Ty B × List (Atom B) × List (Atom B)) :=
  match matchL s₁.reverse s₂.reverse with
  | some (τ, τ', t₁, t₂) => some (τ, τ', t₁.reverse, t₂.reverse)
  | none => none

-- First l-field ANYWHERE in the spine (vars skipped), removed — the U-ground
-- pairing is positional among concrete fields once counting rules the vars out.
def removeField {B : Type} (l : Label) :
    List (Atom B) → Option (Ty B × List (Atom B))
  | [] => none
  | .var β :: s =>
      match removeField l s with
      | some (τ, s') => some (τ, .var β :: s')
      | none => none
  | .field l' τ :: s =>
      if l' = l then some (τ, s)
      else match removeField l s with
        | some (τ', s') => some (τ', .field l' τ :: s')
        | none => none

def groundMatchAux {B : Type} (s₁ s₂ : List (Atom B)) :
    List Label → Option (Ty B × Ty B × List (Atom B) × List (Atom B))
  | [] => none
  | l :: ls =>
      if sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ then
        match removeField l s₁, removeField l s₂ with
        | some (τ, t₁), some (τ', t₂) => some (τ, τ', t₁, t₂)
        | _, _ => groundMatchAux s₁ s₂ ls
      else groundMatchAux s₁ s₂ ls

-- U-ground: s₂ var-free, some label with equal positive counts.
def groundMatch {B : Type} (s₁ s₂ : List (Atom B)) :
    Option (Ty B × Ty B × List (Atom B) × List (Atom B)) :=
  if sHasVar s₂ then none else groundMatchAux s₁ s₂ (sLabels s₁)

-- ## The algorithm
def unifySpineF {B : Type} : Nat → List (Atom B) → List (Atom B) → URes B
  | _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => .success σ []
      | none   => .clash
  | _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => .success σ []
      | none   => .clash
  | 0, _, _ => .stuck   -- unreachable at fuel ≥ |s₁| + |s₂| (each move eats ≥ 2 atoms)
  | fuel+1, s₁, s₂ =>
      match stripL s₁ s₂ with
      | some (t₁, t₂) => unifySpineF fuel t₁ t₂
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => unifySpineF fuel t₁ t₂
      | none =>
      match solveVar s₁ s₂ with
      | some r => r
      | none =>
      match solveVar s₂ s₁ with
      | some r => r
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) => (unifySpineF fuel t₁ t₂).addEq τ τ'
      | none =>
      if projClash s₁ s₂ then .clash else .stuck

def unifySpine {B : Type} (s₁ s₂ : List (Atom B)) : URes B :=
  unifySpineF (s₁.length + s₂.length) s₁ s₂

def unifyRow {B : Type} (ρ₁ ρ₂ : Row B) : URes B :=
  unifySpine ρ₁.toSpine ρ₂.toSpine

-- ## Executable regressions: the worked examples, kernel-checked
-- (B := Unit; each `rfl` runs the algorithm inside the kernel.)
private def uB : Ty Unit := .base ()

-- U-ε.
theorem unify_empty : unifyRow (B := Unit) .empty .empty = .success [] [] := rfl

-- P&X's shared-tail pitfall (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α): U-var-refl
-- right-cancels α, then U-clash — matches shared_tail_no_unifier.
theorem unify_shared_tail :
    unifyRow (B := Unit) (.cat (.sing "l" uB) (.var "a"))
                         (.cat (.sing "m" uB) (.var "a")) = .clash := rfl

-- The LUtail example (l: 𝓫) ≐ᵣ (α | l: 𝓫): right-match the field, then
-- U-ε-var — finds the mgu α ≔ ε that LUtail misses (lutail_unifier_iff).
theorem unify_lutail :
    unifyRow (B := Unit) (.sing "l" uB) (.cat (.var "a") (.sing "l" uB)) =
      .success [("a", .empty)] [(uB, uB)] := rfl

-- Wand's ambiguity (β | α) ≐ᵣ (l: 𝓫): STUCK — solvable but no mgu
-- (wand_unifiable, wand_no_mgu).
theorem unify_wand :
    unifyRow (B := Unit) (.cat (.var "b") (.var "a")) (.sing "l" uB) =
      .stuck := rfl

-- Worked example 2, (α | l: 𝓫 | β) ≐ᵣ (l: 𝓫): U-ground pairs the l-fields
-- (counting rules the vars out), then U-ε-var forces α ≔ ε, β ≔ ε.
theorem unify_ground_collapse :
    unifyRow (B := Unit) (.cat (.var "a") (.cat (.sing "l" uB) (.var "b")))
                         (.sing "l" uB) =
      .success [("a", .empty), ("b", .empty)] [(uB, uB)] := rfl

-- (β | l: 𝓫 | α) ≐ᵣ (l′: 𝓫), l ≠ l′: U-clash, NOT stuck — the projection
-- check is global, a window-only rule would misfile this.
theorem unify_global_clash :
    unifyRow (B := Unit) (.cat (.var "b") (.cat (.sing "l" uB) (.var "a")))
                         (.sing "m" uB) = .clash := rfl

-- α ≐ᵣ (l: 𝓫 | α): the shared END-var cancels first (solution-preserving!),
-- leaving ε ≐ᵣ (l: 𝓫) — a definite CLASH, strictly stronger than an
-- occurs-failure. Cancellativity subsumes end-aligned occurs cases.
theorem unify_occurs_cancelled :
    unifyRow (B := Unit) (.var "a") (.cat (.sing "l" uB) (.var "a")) =
      .clash := rfl

-- U-var-solve with occurs check: α ≐ᵣ (l: 𝓫 | α | m: 𝓫) — the recursive
-- var is interior, no cancellation applies, genuinely a recursive row.
theorem unify_occurs :
    unifyRow (B := Unit) (.var "a")
      (.cat (.sing "l" uB) (.cat (.var "a") (.sing "m" uB))) = .occurs := rfl

-- Var-var: solved union-find style.
theorem unify_var_var :
    unifyRow (B := Unit) (.var "a") (.var "b") =
      .success [("a", .cat (.var "b") .empty)] [] := rfl

-- The ambiguous mirror (α | l: 𝓫) ≐ᵣ (l: 𝓫 | β): both windows closed by a
-- var, both sides have vars — correctly stuck (Levi splits two ways).
theorem unify_two_sided_stuck :
    unifyRow (B := Unit) (.cat (.var "a") (.sing "l" uB))
                         (.cat (.sing "l" uB) (.var "b")) = .stuck := rfl


------------------------------------ NEXT ------------------------------------
-- Milestones that build on this file (algorithmic.typ, Open questions):
--  * STRICTNESS of the QTyped extension: prove ¬Typed for the two-use
--    program at its precise type (lifts no_plain_principal_scheme through
--    let-inversion) — makes "L2 is strictly more precise" a theorem.
--  * Type safety for QTyped itself (progress/preservation) — the L2 system
--    is the real declarative system of the thesis; minimal.lean's proofs
--    are the template, discharge determinism/monotonicity the new inputs.
--  * ≐ᵣ metatheory: the algorithm is defined and regression-tested above;
--    remaining are the proofs — soundness (success σ eqs ⟹ every θ
--    extending σ and satisfying eqs mod ≈ unifies), mgu-on-success,
--    clash/occurs ⟹ no unifier, stuck ⟹ no unique mgu (the trichotomy;
--    cancel_cat_*, ground_char and wand_no_mgu are the prepared inputs),
--    and fuel-sufficiency (each move consumes ≥ 2 atoms).
--  * The type-level driver ≐: solve the emitted (τ, τ') equations, mutually
--    recursing into rows; occurs/rank discipline across both sorts.
--  * Non-vacuity of qualified schemes: needs lookup_total (RowWF) plus a
--    freshness discipline for the result variables δ.
--  * The covering order ⊴ on qualified schemes (needed to STATE "the
--    principal type improves under reduction").
--  * Solver state S = (θ, Δ, W), stump wake-up, and the confluence argument
--    that the final state is independent of wake-up scheduling
--    (lookup_det + Discharge.mono_of_definite are the two pillars).

end MinimalCalculus
