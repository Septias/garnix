-- Lean 4 formalization of masterthesis/algorithmic.typ:
-- qualified (stump-carrying) schemes and instantiation-with-discharge.
--
-- Builds on minimal.lean: the declarative system, the lookup
-- QScheme is defined afresh instead of retrofitting a
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
-- ⊢  σ.toQ ≥_Γ τ   ↔   σ ≥ τ
theorem QScheme.inst_toQ {B : Type} {Γ : Ctx B} {σ : Scheme B} {τ : Ty B} :
    QScheme.Inst Γ σ.toQ τ ↔ σ.Inst τ := by
  constructor
  · rintro ⟨θ, hfix, -, hbody⟩
    exact ⟨θ, hfix, hbody⟩
  · rintro ⟨θ, hfix, hbody⟩
    exact ⟨θ, hfix, fun s hs => absurd hs List.not_mem_nil, hbody⟩

-- Monotype qualified schemes instantiate only to themselves.
-- ⊢  ⟨[], [], τ₁⟩ ≥_Γ τ   ⟹   τ = τ₁
theorem QScheme.Inst.mono {B : Type} {Γ : Ctx B} {τ₁ τ : Ty B}
    (h : QScheme.Inst Γ ⟨[], [], τ₁⟩ τ) : τ = τ₁ :=
  Scheme.Inst.mono ((QScheme.inst_toQ (σ := ⟨[], τ₁⟩)).mp h)


---------------------------- DISCHARGE METATHEORY -----------------------------

-- Determinism: two discharges of the same stump that substitute the row the
-- same way pin the result variable to the same type (lookup_det lifted).
-- This is what makes ≥_Γ a well-defined relation per θ rather than a choice.
-- ⊢  discharge s @θ₁,  discharge s @θ₂,  θ₁·s.row = θ₂·s.row
--        ⟹   θ₁·s.res = θ₂·s.res
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
-- ⊢  Γ ⊑ᵣ Γ',  discharge s @θ in Γ,  ¬(Γ ⊢ (θ·s.row).l ↓ ?)
--        ⟹   discharge s @θ in Γ'
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
-- ⊢  Γ ⊢ ρ.l ↓ r   ⟹   selQ ≥_Γ ({ρ} → collapse r)
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
-- ⊢  selQ ≥_Γ ({l: τ₀} → τ₀)      (every τ₀)
theorem selQ_inst_found {B : Type} (Γ : Ctx B) (τ₀ : Ty B) :
    QScheme.Inst Γ (selQ B) (.fn (.rcd (.sing "l" τ₀)) τ₀) :=
  selQ_inst_of_lookup (r := .found τ₀) .hit

-- ... and so is the ⊥-typing:
-- ⊢  selQ ≥_Γ ({} → ★)
theorem selQ_inst_absent {B : Type} (Γ : Ctx B) :
    QScheme.Inst Γ (selQ B) (.fn (.rcd .empty) .unk) :=
  selQ_inst_of_lookup (r := .absent) .emp

-- The mixed instance {ε} → {ε} — the one every plain scheme was forced to
-- admit (no_plain_principal_scheme's contradiction) — is NOT an instance:
-- θ must send β to ε, the lookup on ε is definitely ⊥, and discharge then
-- pins δ at ★, never at {ε}. Discharge is exactly the mechanism that plugs
-- the instance-closedness leak.
-- ⊢  ¬ ( selQ ≥_∅ ({} → {}) )
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
-- ⊢  ∀ τ. selQ ≥_Γ τ   ⟹   Γ ⊢ (λx. x.l) : τ
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
-- ⊢  ∃ σ.  (∀ τ. σ ≥_∅ τ ⟹ ∅ ⊢ λx.x.l : τ)
--            ∧  σ ≥_∅ ({l: {}} → {})   ∧  σ ≥_∅ ({} → ★)
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

-- ⊢  vars(s₁ ++ s₂) = vars(s₁) ++ vars(s₂)
theorem sVarSeq_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    sVarSeq (s₁ ++ s₂) = sVarSeq s₁ ++ sVarSeq s₂
  | [], _ => rfl
  | .field _ _ :: s₁, s₂ => sVarSeq_append s₁ s₂
  | .var _ :: s₁, s₂ => congrArg (_ :: ·) (sVarSeq_append s₁ s₂)

-- ⊢  proj_l(s₁ ++ s₂) = proj_l(s₁) ++ map(·+|vars s₁|) proj_l(s₂)
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

-- ⊢  ps ≈ₚ ps
theorem ProjEquiv.refl {B : Type} : (ps : List (Nat × Ty B)) → ProjEquiv ps ps
  | [] => .nil
  | (_, τ) :: ps => .cons rfl (.refl τ) (ProjEquiv.refl ps)

-- ⊢  ps = qs   ⟹   ps ≈ₚ qs
theorem ProjEquiv.of_eq {B : Type} {ps qs : List (Nat × Ty B)} (h : ps = qs) :
    ProjEquiv ps qs := h ▸ ProjEquiv.refl ps

-- ⊢  ps ≈ₚ qs   ⟹   qs ≈ₚ ps
theorem ProjEquiv.symm {B : Type} {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv qs ps
  | .nil => .nil
  | .cons hn hty h => .cons hn.symm hty.symm h.symm

-- ⊢  ps ≈ₚ qs,  qs ≈ₚ rs   ⟹   ps ≈ₚ rs
theorem ProjEquiv.trans {B : Type} {ps qs rs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv qs rs → ProjEquiv ps rs
  | .nil, h => h
  | .cons hn hty h, .cons hn' hty' h' =>
      .cons (hn.trans hn') (hty.trans hty') (h.trans h')

-- ⊢  ps ≈ₚ qs,  ps' ≈ₚ qs'   ⟹   (ps ++ ps') ≈ₚ (qs ++ qs')
theorem ProjEquiv.append {B : Type} {ps qs ps' qs' : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv ps' qs' → ProjEquiv (ps ++ ps') (qs ++ qs')
  | .nil, h => h
  | .cons hn hty h, h' => .cons hn hty (h.append h')

-- ⊢  ps ≈ₚ qs   ⟹   map(·+k) ps ≈ₚ map(·+k) qs
theorem ProjEquiv.mapShift {B : Type} (k : Nat) {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs →
    ProjEquiv (ps.map (fun p => (p.1 + k, p.2))) (qs.map (fun p => (p.1 + k, p.2)))
  | .nil => .nil
  | .cons hn hty h => .cons (by rw [hn]) hty (ProjEquiv.mapShift k h)

-- ⊢  [] ≈ₚ qs   ⟹   qs = []
theorem ProjEquiv.nil_inv {B : Type} {qs : List (Nat × Ty B)}
    (h : ProjEquiv [] qs) : qs = [] := by cases h; rfl

-- ⊢  (n,τ)::ps ≈ₚ qs
--        ⟹  ∃ τ' rest. qs = (n,τ')::rest ∧ τ ≈ τ' ∧ ps ≈ₚ rest
theorem ProjEquiv.cons_inv {B : Type} {n : Nat} {τ : Ty B}
    {ps qs : List (Nat × Ty B)} (h : ProjEquiv ((n, τ) :: ps) qs) :
    ∃ τ' rest, qs = (n, τ') :: rest ∧ TyEquiv τ τ' ∧ ProjEquiv ps rest := by
  cases h with
  | cons hn hty h' => exact ⟨_, _, by rw [← hn], hty, h'⟩

-- Un-shifting: the segment indices are injective in +1 (used to strip a
-- leading var off both sides).
-- ⊢  map(·+1) ps ≈ₚ map(·+1) qs   ⟹   ps ≈ₚ qs
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
-- ⊢  ¬ ( map(·+1) ps ≈ₚ (0,τ)::qs )
theorem ProjEquiv.no_zero_head {B : Type} {qs : List (Nat × Ty B)} {τ : Ty B} :
    (ps : List (Nat × Ty B)) →
    ¬ ProjEquiv (ps.map (fun p => (p.1 + 1, p.2))) ((0, τ) :: qs)
  | [], h => nomatch h
  | _ :: _, h => by
      cases h with
      | cons hn _ _ => exact Nat.succ_ne_zero _ hn

-- ⊢  map(·+1) ps = (0,τ)::qs   ⟹   False
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

-- ⊢  spine ρ₁ = spine ρ₂   ⟹   Char(ρ₁, ρ₂)
theorem Row.Char.of_eq {B : Type} {ρ₁ ρ₂ : Row B}
    (h : ρ₁.toSpine = ρ₂.toSpine) : Row.Char ρ₁ ρ₂ :=
  ⟨by rw [h], fun _ => .of_eq (by rw [h])⟩

-- ## Soundness: ≈ preserves both invariants
-- Every ≈-axiom is invariant-preserving: assoc/units don't move atoms
-- (spines are EQUAL), comm swaps distinct labels (all projections are equal
-- lists), congruence is pointwise, and no rule crosses a var.
-- ⊢  ρ₁ ≈ᵣ ρ₂   ⟹   Char(ρ₁, ρ₂)      (≈ soundness)
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
-- ⊢  ofSpine(s₁ ++ s₂) ≈ᵣ (ofSpine s₁ | ofSpine s₂)
theorem ofSpine_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    RowEquiv (ofSpine (s₁ ++ s₂)) (.cat (ofSpine s₁) (ofSpine s₂))
  | [], _ => RowEquiv.unitL.symm
  | .field _ _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm
  | .var _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm

-- ⊢  ρ ≈ᵣ ofSpine(spine ρ)      (every row ≈ its refolded spine)
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
-- ⊢  proj_l(s) = (0,τ)::rest   ⟹
--       ∃ t.  ofSpine s ≈ᵣ (l:τ | ofSpine t)  ∧  proj_l(t) = rest
--             ∧  (∀ l'≠l. proj_l'(t) = proj_l'(s))  ∧  vars(t) = vars(s)
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
-- ⊢  vars(s) = []  ∧  (∀ l. proj_l(s) = [])   ⟹   s = []
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
-- ⊢  vars(s₁) = vars(s₂)  ∧  (∀ l. proj_l(s₁) ≈ₚ proj_l(s₂))
--        ⟹   ofSpine s₁ ≈ᵣ ofSpine s₂      (≈ completeness)
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
-- ⊢  Char(ρ₁, ρ₂)   ⟹   ρ₁ ≈ᵣ ρ₂
theorem RowEquiv.ofChar {B : Type} {ρ₁ ρ₂ : Row B} (h : Row.Char ρ₁ ρ₂) :
    ρ₁ ≈ᵣ ρ₂ :=
  ρ₁.toSpine_equiv.trans
    ((char_complete _ _ h.1 h.2).trans ρ₂.toSpine_equiv.symm)

--   ρ₁ ≈ ρ₂   iff   same var sequence ∧ pointwise-≈ l-projections
-- ⊢  ρ₁ ≈ᵣ ρ₂   ↔   Char(ρ₁, ρ₂)
theorem rowEquiv_iff_char {B : Type} {ρ₁ ρ₂ : Row B} :
    ρ₁ ≈ᵣ ρ₂ ↔ Row.Char ρ₁ ρ₂ :=
  ⟨RowEquiv.char, RowEquiv.ofChar⟩

-- ## Cancellativity of shared end-vars
-- The trace-monoid fact ≐ᵣ's U-var-refl rests on: stripping a shared var off
-- either end is sound (trivially, by ≈-congruence) AND complete (below) —
-- this is what replaces P&X's shared-tail side condition [Δ₂]ρ₁ = [Δ₁]ρ₁.
-- ⊢  (α | ρ₁) ≈ᵣ (α | ρ₂)   ⟹   ρ₁ ≈ᵣ ρ₂
theorem RowEquiv.cancel_var_left {B : Type} {α : TyVar} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat (.var α) ρ₁) (.cat (.var α) ρ₂)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, List.cons_append, List.nil_append, sVarSeq] at hv
  refine RowEquiv.ofChar ⟨?_, fun l => ?_⟩
  · injection hv
  · have h' := hp l
    simp only [Row.toSpine, List.cons_append, List.nil_append, sProj] at h'
    exact h'.unshift

-- ⊢  (ρ₁ | α) ≈ᵣ (ρ₂ | α)   ⟹   ρ₁ ≈ᵣ ρ₂
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

-- ⊢  ps ≈ₚ qs   ⟹   |ps| = |qs|
theorem ProjEquiv.length {B : Type} :
    {ps qs : List (Nat × Ty B)} → ProjEquiv ps qs → ps.length = qs.length
  | _, _, .nil => rfl
  | _, _, .cons _ _ h => congrArg (· + 1) h.length

-- ⊢  (a ++ c) ≈ₚ (b ++ d),  |a| = |b|   ⟹   a ≈ₚ b  ∧  c ≈ₚ d
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

-- ⊢  map(·+k) ps ≈ₚ map(·+k) qs   ⟹   ps ≈ₚ qs
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

-- ⊢  (ρ | ρ₁) ≈ᵣ (ρ | ρ₂)   ⟹   ρ₁ ≈ᵣ ρ₂      (shared prefix cancels)
theorem RowEquiv.cancel_cat_left {B : Type} {ρ ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ ρ₁) (.cat ρ ρ₂)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, sVarSeq_append] at hv
  refine RowEquiv.ofChar ⟨(List.append_inj hv rfl).2, fun l => ?_⟩
  have h' := hp l
  simp only [Row.toSpine, sProj_append] at h'
  exact ((ProjEquiv.split h' rfl).2).unshiftK _

-- ⊢  (ρ₁ | ρ) ≈ᵣ (ρ₂ | ρ)   ⟹   ρ₁ ≈ᵣ ρ₂      (shared suffix cancels)
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

-- ## Leading-field cancellation (the completeness ingredient for U-field)
-- Unlike cancel_cat_left (identical shared prefix), the two leading fields carry
-- DIFFERENT types; cancelling them EXTRACTS the type equivalence. Both fields sit
-- at segment 0 (nothing precedes them), so ≈'s l-projection pins their types
-- pairwise and leaves the tails ≈. This is the forward direction the matchL move
-- needs: a unifier of the original must equate the matched field types.
-- ⊢  (l:τ₁ | R₁) ≈ᵣ (l:τ₂ | R₂)   ⟹   τ₁ ≈ₜ τ₂  ∧  R₁ ≈ᵣ R₂
theorem RowEquiv.field_cancel_left {B : Type} {l : Label} {τ₁ τ₂ : Ty B}
    {R₁ R₂ : Row B}
    (h : RowEquiv (.cat (.sing l τ₁) R₁) (.cat (.sing l τ₂) R₂)) :
    TyEquiv τ₁ τ₂ ∧ RowEquiv R₁ R₂ := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, sVarSeq_append, sVarSeq, List.nil_append] at hv
  have hred : ∀ (τ : Ty B) (R : Row B),
      sProj l (Row.toSpine (.cat (.sing l τ) R)) = (0, τ) :: sProj l R.toSpine := by
    intro τ R
    simp only [Row.toSpine, List.cons_append, List.nil_append, sProj]
    split
    · rfl
    · rename_i hne; exact absurd trivial hne
  have hpl := hp l
  rw [hred, hred] at hpl
  obtain ⟨τ', rest, heq, hty, hrest⟩ := hpl.cons_inv
  injection heq with hhd htl
  injection hhd with _ hτ'
  subst hτ'; subst htl
  refine ⟨hty, RowEquiv.ofChar ⟨hv, fun l' => ?_⟩⟩
  by_cases hll' : l = l'
  · subst hll'; exact hrest
  · have h' := hp l'
    have hred' : ∀ (τ : Ty B) (R : Row B),
        sProj l' (Row.toSpine (.cat (.sing l τ) R)) = sProj l' R.toSpine := by
      intro τ R
      simp only [Row.toSpine, List.cons_append, List.nil_append, sProj]
      split
      · rename_i heq; exact absurd heq hll'
      · rfl
    rw [hred', hred'] at h'
    exact h'

-- A product ≈ ε forces each factor ≈ ε (a cancellative monoid with no inverses:
-- ε has empty var sequence and empty projections, and both split over ++). The
-- base case of ≐ᵣ completeness — an exhausted side pins every leftover var to ε.
-- ⊢  (ρ₁ | ρ₂) ≈ᵣ ε   ⟹   ρ₁ ≈ᵣ ε  ∧  ρ₂ ≈ᵣ ε
theorem RowEquiv.cat_empty_split {B : Type} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ₁ ρ₂) .empty) :
    RowEquiv ρ₁ .empty ∧ RowEquiv ρ₂ .empty := by
  obtain ⟨hv, hp⟩ := h.char
  simp only [Row.toSpine, sVarSeq_append, sVarSeq] at hv
  obtain ⟨hv₁, hv₂⟩ := List.append_eq_nil_iff.mp hv
  have hpe : ∀ l, sProj l ρ₁.toSpine = [] ∧ sProj l ρ₂.toSpine = [] := by
    intro l
    have h' := hp l
    simp only [Row.toSpine, sProj_append, sProj] at h'
    have heq := ProjEquiv.nil_inv h'.symm
    obtain ⟨hx, hym⟩ := List.append_eq_nil_iff.mp heq
    exact ⟨hx, List.map_eq_nil_iff.mp hym⟩
  exact ⟨RowEquiv.ofChar ⟨by rw [hv₁]; rfl, fun l => ProjEquiv.of_eq (by rw [(hpe l).1]; rfl)⟩,
         RowEquiv.ofChar ⟨by rw [hv₂]; rfl, fun l => ProjEquiv.of_eq (by rw [(hpe l).2]; rfl)⟩⟩

-- ## Ground rows: the characterization degenerates to the projections
-- (the T-eq workhorse for ≐ᵣ's ground completeness).
-- ⊢  ρ.SpineVarFree   ↔   vars(spine ρ) = []
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

-- ⊢  ρ₁, ρ₂ var-free   ⟹
--       ( ρ₁ ≈ᵣ ρ₂   ↔   ∀ l. proj_l(ρ₁) ≈ₚ proj_l(ρ₂) )
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
-- ⊢  l₁ ≠ l₂   ⟹   ¬ θ ⊨ (l₁:𝓫 | α) ≐ᵣ (l₂:𝓫 | α)
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
-- ⊢  θ ⊨ (l:𝓫) ≐ᵣ (α | l:𝓫)   ↔   θα ≈ᵣ ε
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
-- ⊢  (p₁ ++ p₂) ≈ₚ [(n,τ)]
--        ⟹  (p₁ = [] ∧ p₂ ≈ₚ [(n,τ)])  ∨  (p₁ ≈ₚ [(n,τ)] ∧ p₂ = [])
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
-- ⊢  vars(spine ρ) = [],  proj_l(ρ) ≈ₚ [(0,𝓫)],  (∀ l'≠l. proj_l'(ρ) = [])
--        ⟹   ρ ≈ᵣ (l:𝓫)
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
-- ⊢  ∃ θ.  θ ⊨ (β | α) ≐ᵣ (l:𝓫)
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
-- ⊢  ¬ ∃ θ.  θ ⊨ (β | α) ≐ᵣ (l:𝓫)
--             ∧  (∀ θ'. θ' ⊨ (β | α) ≐ᵣ (l:𝓫) ⟹ θ' ⊑ θ)      (no mgu)
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

-- ⊢  (Γ, x:σ).lookup y  =  if x = y then some σ else Γ.lookup y
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

-- ⊢  Γ.toQ.lookup x  =  (Γ.lookup x).map (·.toQ)
theorem Ctx.toQ_lookup {B : Type} (Γ : Ctx B) (x : Var) :
    Γ.toQ.lookup x = (Γ.lookup x).map Scheme.toQ := by
  simp only [Ctx.toQ, QCtx.lookup, Ctx.lookup, List.find?_map, Option.map_map]
  rfl

-- ⊢  Γ ⊢ e : τ   ⟹   Γ.toQ ⊢_Q e : τ      (declarative embeds into L2)
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

-- ⊢  Γ ⊢ b : ρ   ⟹   Γ.toQ ⊢_Q b : ρ      (record-body version)
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
-- ⊢  ∅ ⊢_Q  let f = (λx. x.l) in { a = f {l = c} | b = f {} }
--            :  { a: 𝓫_c | b: ★ }
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
-- ⊢  unifyRow ε ε  =  success [] []
theorem unify_empty : unifyRow (B := Unit) .empty .empty = .success [] [] := rfl

-- P&X's shared-tail pitfall (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α): U-var-refl
-- right-cancels α, then U-clash — matches shared_tail_no_unifier.
-- ⊢  unifyRow (l:𝓫 | a) (m:𝓫 | a)  =  clash
theorem unify_shared_tail :
    unifyRow (B := Unit) (.cat (.sing "l" uB) (.var "a"))
                         (.cat (.sing "m" uB) (.var "a")) = .clash := rfl

-- The LUtail example (l: 𝓫) ≐ᵣ (α | l: 𝓫): right-match the field, then
-- U-ε-var — finds the mgu α ≔ ε that LUtail misses (lutail_unifier_iff).
-- ⊢  unifyRow (l:𝓫) (a | l:𝓫)  =  success [a ≔ ε] [(𝓫, 𝓫)]
theorem unify_lutail :
    unifyRow (B := Unit) (.sing "l" uB) (.cat (.var "a") (.sing "l" uB)) =
      .success [("a", .empty)] [(uB, uB)] := rfl

-- Wand's ambiguity (β | α) ≐ᵣ (l: 𝓫): STUCK — solvable but no mgu
-- (wand_unifiable, wand_no_mgu).
-- ⊢  unifyRow (b | a) (l:𝓫)  =  stuck
theorem unify_wand :
    unifyRow (B := Unit) (.cat (.var "b") (.var "a")) (.sing "l" uB) =
      .stuck := rfl

-- Worked example 2, (α | l: 𝓫 | β) ≐ᵣ (l: 𝓫): U-ground pairs the l-fields
-- (counting rules the vars out), then U-ε-var forces α ≔ ε, β ≔ ε.
-- ⊢  unifyRow (a | l:𝓫 | b) (l:𝓫)  =  success [a ≔ ε, b ≔ ε] [(𝓫, 𝓫)]
theorem unify_ground_collapse :
    unifyRow (B := Unit) (.cat (.var "a") (.cat (.sing "l" uB) (.var "b")))
                         (.sing "l" uB) =
      .success [("a", .empty), ("b", .empty)] [(uB, uB)] := rfl

-- (β | l: 𝓫 | α) ≐ᵣ (l′: 𝓫), l ≠ l′: U-clash, NOT stuck — the projection
-- check is global, a window-only rule would misfile this.
-- ⊢  unifyRow (b | l:𝓫 | a) (m:𝓫)  =  clash
theorem unify_global_clash :
    unifyRow (B := Unit) (.cat (.var "b") (.cat (.sing "l" uB) (.var "a")))
                         (.sing "m" uB) = .clash := rfl

-- α ≐ᵣ (l: 𝓫 | α): the shared END-var cancels first (solution-preserving!),
-- leaving ε ≐ᵣ (l: 𝓫) — a definite CLASH, strictly stronger than an
-- occurs-failure. Cancellativity subsumes end-aligned occurs cases.
-- ⊢  unifyRow a (l:𝓫 | a)  =  clash
theorem unify_occurs_cancelled :
    unifyRow (B := Unit) (.var "a") (.cat (.sing "l" uB) (.var "a")) =
      .clash := rfl

-- U-var-solve with occurs check: α ≐ᵣ (l: 𝓫 | α | m: 𝓫) — the recursive
-- var is interior, no cancellation applies, genuinely a recursive row.
-- ⊢  unifyRow a (l:𝓫 | a | m:𝓫)  =  occurs
theorem unify_occurs :
    unifyRow (B := Unit) (.var "a")
      (.cat (.sing "l" uB) (.cat (.var "a") (.sing "m" uB))) = .occurs := rfl

-- Var-var: solved union-find style.
-- ⊢  unifyRow a b  =  success [a ≔ (b | ε)] []
theorem unify_var_var :
    unifyRow (B := Unit) (.var "a") (.var "b") =
      .success [("a", .cat (.var "b") .empty)] [] := rfl

-- The ambiguous mirror (α | l: 𝓫) ≐ᵣ (l: 𝓫 | β): both windows closed by a
-- var, both sides have vars — correctly stuck (Levi splits two ways).
-- ⊢  unifyRow (a | l:𝓫) (l:𝓫 | b)  =  stuck
theorem unify_two_sided_stuck :
    unifyRow (B := Unit) (.cat (.var "a") (.sing "l" uB))
                         (.cat (.sing "l" uB) (.var "b")) = .stuck := rfl


--------------------- ≐ᵣ METATHEORY: FIELD-COUNT INVARIANT -------------------
-- The l-field count is a ≈-invariant: ≈ preserves projection-list lengths.
-- Substitution can only ADD l-fields (vars expand to ≥ 0 new fields); for
-- var-free rows the count is fixed. These three facts together prove the
-- U-clash direction of the trichotomy (projClash_no_unifier).

-- Spine roundtrip: toSpine . ofSpine = id.
-- ⊢  spine(ofSpine s) = s
private theorem ofSpine_toSpine {B : Type} : (s : List (Atom B)) → (ofSpine s).toSpine = s
  | [] => rfl
  | .field l τ :: s => by
      simp only [ofSpine, Row.toSpine, List.singleton_append]
      exact congrArg (.field l τ :: ·) (ofSpine_toSpine s)
  | .var α :: s => by
      simp only [ofSpine, Row.toSpine, List.singleton_append]
      exact congrArg (.var α :: ·) (ofSpine_toSpine s)

-- l-field count distributes over spine append.
-- ⊢  count_l(s₁ ++ s₂) = count_l(s₁) + count_l(s₂)
private theorem sFieldCount_append {B : Type} (l : Label) : (s₁ s₂ : List (Atom B)) →
    sFieldCount l (s₁ ++ s₂) = sFieldCount l s₁ + sFieldCount l s₂
  | [], _ => by simp [sFieldCount]
  | .field l' _ :: s₁, s₂ => by
      simp only [List.cons_append, sFieldCount]
      rw [sFieldCount_append l s₁ s₂]; omega
  | .var _ :: s₁, s₂ => by
      simp only [List.cons_append, sFieldCount]
      rw [sFieldCount_append l s₁ s₂]

-- (sProj l s).length = sFieldCount l s (projection list length = concrete count).
-- ⊢  |proj_l(s)| = count_l(s)
private theorem sProj_length_eq_sFieldCount {B : Type} (l : Label) : (s : List (Atom B)) →
    (sProj l s).length = sFieldCount l s
  | [] => rfl
  | .field l' _ :: s => by
      simp only [sProj, sFieldCount]
      by_cases h : l' = l <;> simp [h, sProj_length_eq_sFieldCount l s] <;> omega
  | .var _ :: s => by
      simp only [sProj, sFieldCount, List.length_map, sProj_length_eq_sFieldCount l s]

-- ProjEquiv preserves list length.
-- ⊢  ps ≈ₚ qs   ⟹   |ps| = |qs|
private theorem ProjEquiv.length_eq {B : Type} : {ps qs : List (Nat × Ty B)} →
    ProjEquiv ps qs → ps.length = qs.length
  | _, _, .nil => rfl
  | _, _, .cons _ _ h => congrArg Nat.succ h.length_eq

-- ≈ preserves the l-field count (rowEquiv_iff_char + projection-length).
-- ⊢  ρ₁ ≈ᵣ ρ₂   ⟹   count_l(spine ρ₁) = count_l(spine ρ₂)
theorem rowEquiv_fieldCount_eq {B : Type} {ρ₁ ρ₂ : Row B} (l : Label) (h : ρ₁ ≈ᵣ ρ₂) :
    sFieldCount l ρ₁.toSpine = sFieldCount l ρ₂.toSpine := by
  rw [← sProj_length_eq_sFieldCount, ← sProj_length_eq_sFieldCount]
  exact (h.char.2 l).length_eq

-- sHasVar s = false ↔ sVarSeq s = [] (bridge from Bool to Prop).
-- ⊢  hasVar(s) = false   ↔   vars(s) = []
private theorem sHasVar_false_iff {B : Type} : (s : List (Atom B)) →
    (sHasVar s = false ↔ sVarSeq s = [])
  | [] => ⟨fun _ => rfl, fun _ => rfl⟩
  | .var _ :: _ => by simp [sHasVar, sVarSeq]
  | .field _ _ :: s => by simp [sHasVar, sVarSeq, sHasVar_false_iff s]

-- !sHasVar s → SpineVarFree (ofSpine s) — used in projClash_no_unifier.
-- ⊢  hasVar(s) = false   ⟹   (ofSpine s).SpineVarFree
private theorem spineVarFree_ofSpine {B : Type} {s : List (Atom B)}
    (h : sHasVar s = false) : (ofSpine s).SpineVarFree :=
  (spineVarFree_iff_varSeq_nil (ofSpine s)).mpr
    (by rw [ofSpine_toSpine]; exact (sHasVar_false_iff s).mp h)

-- Substitution can only add l-fields: the count weakly increases.
-- ⊢  count_l(spine ρ) ≤ count_l(spine (θρ))
theorem sFieldCount_applySubst_le {B : Type} (θ : TySubst B) (l : Label) :
    (ρ : Row B) → sFieldCount l ρ.toSpine ≤ sFieldCount l (ρ.applySubst θ).toSpine
  | .empty => Nat.le_refl _
  | .var _ => Nat.zero_le _
  | .sing l' _ => by simp [Row.applySubst, Row.toSpine, sFieldCount]
  | .cat ρ₁ ρ₂ => by
      simp only [Row.applySubst, Row.toSpine, sFieldCount_append]
      exact Nat.add_le_add
        (sFieldCount_applySubst_le θ l ρ₁) (sFieldCount_applySubst_le θ l ρ₂)

-- For var-free rows substitution fixes the l-field count exactly.
-- ⊢  ρ var-free   ⟹   count_l(spine (θρ)) = count_l(spine ρ)
theorem sFieldCount_applySubst_varFree {B : Type} (θ : TySubst B) (l : Label) :
    {ρ : Row B} → ρ.SpineVarFree →
    sFieldCount l (ρ.applySubst θ).toSpine = sFieldCount l ρ.toSpine
  | .empty, _ => rfl
  | .var _, h => nomatch h
  | .sing l' _, _ => by simp [Row.applySubst, Row.toSpine, sFieldCount]
  | .cat ρ₁ ρ₂, .cat hv₁ hv₂ => by
      simp only [Row.applySubst, Row.toSpine, sFieldCount_append]
      rw [sFieldCount_applySubst_varFree θ l hv₁, sFieldCount_applySubst_varFree θ l hv₂]

-- ## The stuck ⟹ no-unique-mgu direction, via field-count monotonicity
-- Substitution never DELETES an l-field (sFieldCount_applySubst_le) and ≈
-- preserves counts (rowEquiv_fieldCount_eq). So if θ' factors through θ
-- (θ' ⊑ θ), then θ carries no more l-fields at any single variable than θ'
-- does: an mgu is pointwise-MINIMAL in every label count. A unifier that
-- strictly undercuts a candidate somewhere therefore refutes its maximality —
-- this single fact is the engine behind Wand's ambiguity (U-stuck).

-- ⊢  θ' ⊑ θ   ⟹   count_l(θ x) ≤ count_l(θ' x)
theorem instanceOf_fieldCount_mono {B : Type} {θ' θ : TySubst B}
    (h : InstanceOf θ' θ) (x : TyVar) (l : Label) :
    sFieldCount l (θ.row x).toSpine ≤ sFieldCount l (θ'.row x).toSpine := by
  obtain ⟨σ, hrow, -⟩ := h
  rw [rowEquiv_fieldCount_eq l (hrow x)]
  exact sFieldCount_applySubst_le σ l (θ.row x)

-- The reusable no-mgu principle: if every unifier is strictly undercut at some
-- variable/label by another unifier, then no unifier is most general.
-- ⊢  (∀ unifier θ. ∃ unifier u, x, l.  count_l(u x) < count_l(θ x))
--        ⟹   ¬ ∃ mgu
theorem no_mgu_of_witness_shrinks {B : Type} {ρ₁ ρ₂ : Row B}
    (H : ∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ →
         ∃ (u : TySubst B) (x : TyVar) (l : Label),
           Unifies u ρ₁ ρ₂ ∧
           sFieldCount l (u.row x).toSpine < sFieldCount l (θ.row x).toSpine) :
    ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ ∧
        ∀ θ' : TySubst B, Unifies θ' ρ₁ ρ₂ → InstanceOf θ' θ := by
  rintro ⟨θ, hu, hmgu⟩
  obtain ⟨u, x, l, huni, hlt⟩ := H θ hu
  exact absurd (instanceOf_fieldCount_mono (hmgu u huni) x l) (by omega)

-- Wand's example, re-proved through the counting principle: in (β | α) ≐ᵣ (l:𝓫)
-- the single l-field must be hosted by θβ or θα (their counts sum to 1); the
-- witness that hosts it in the OTHER variable undercuts the chosen one's count
-- from 1 to 0, so no mgu exists. Cleaner and more uniform than the direct
-- projection argument (and the template for every field-vs-vars stuck case).
-- ⊢  ¬ ∃ mgu for (β | α) ≐ᵣ (l:𝓫)
theorem wand_no_mgu_count {B : Type} (b : B) (l : Label) :
    ¬ ∃ θ : TySubst B,
        Unifies θ (.cat (.var "β") (.var "α")) (.sing l (.base b)) ∧
        ∀ θ' : TySubst B,
          Unifies θ' (.cat (.var "β") (.var "α")) (.sing l (.base b)) →
          InstanceOf θ' θ := by
  apply no_mgu_of_witness_shrinks
  intro θ hu
  have hu' : RowEquiv (Row.cat (θ.row "β") (θ.row "α")) (Row.sing l (.base b)) := by
    have h := hu; unfold Unifies at h
    simpa only [Row.applySubst, Ty.applySubst] using h
  have hcount : sFieldCount l (θ.row "β").toSpine
              + sFieldCount l (θ.row "α").toSpine = 1 := by
    have h := rowEquiv_fieldCount_eq l hu'
    rw [show (Row.cat (θ.row "β") (θ.row "α")).toSpine
          = (θ.row "β").toSpine ++ (θ.row "α").toSpine from rfl,
        sFieldCount_append] at h
    simpa [Row.toSpine, sFieldCount] using h
  by_cases hα : sFieldCount l (θ.row "α").toSpine = 0
  · -- the field is in β (count β = 1); empty-β witness undercuts at "β"
    refine ⟨⟨fun x => .var x, fun x => if x = "β" then .empty
              else if x = "α" then .sing l (.base b) else .var x⟩, "β", l, ?_, ?_⟩
    · unfold Unifies; simp [Row.applySubst, Ty.applySubst]; exact RowEquiv.unitL
    · show 0 < sFieldCount l (θ.row "β").toSpine; omega
  · -- the field is in α (count α = 1); empty-α witness undercuts at "α"
    refine ⟨⟨fun x => .var x, fun x => if x = "β" then .sing l (.base b)
              else if x = "α" then .empty else .var x⟩, "α", l, ?_, ?_⟩
    · unfold Unifies; simp [Row.applySubst, Ty.applySubst]; exact RowEquiv.unitR
    · show 0 < sFieldCount l (θ.row "α").toSpine; omega

-- The dual of the count bound: a VAR-FREE component of θ is RIGID — every
-- instance has the exact same l-count there, because with no variables to
-- expand, substitution can neither add nor delete fields
-- (sFieldCount_applySubst_varFree turns the ≤ into an =).
-- ⊢  θ' ⊑ θ,  (θ x) var-free   ⟹   count_l(θ' x) = count_l(θ x)
theorem instanceOf_fieldCount_eq_of_varFree {B : Type} {θ' θ : TySubst B}
    (h : InstanceOf θ' θ) {x : TyVar} (hvf : (θ.row x).SpineVarFree) (l : Label) :
    sFieldCount l (θ'.row x).toSpine = sFieldCount l (θ.row x).toSpine := by
  obtain ⟨σ, hrow, -⟩ := h
  rw [rowEquiv_fieldCount_eq l (hrow x), sFieldCount_applySubst_varFree σ l hvf]

-- The OTHER canonical stuck shape: (α | l:𝓫) ≐ᵣ (l:𝓫 | β), α ≠ β. Counting alone
-- CANNOT fire here — the ε,ε unifier has count 0 at every variable, so nothing
-- can be undercut. The kill is RIGIDITY. The empty witness (α,β ↦ ε) pins
-- count_l(θα) = count_l(θβ) = 0; the unifier equation then forces θα var-free
-- (its lone l-projection would sit at segment |vars θα|, which must match
-- segment 0 on the right — so |vars θα| = 0); and a var-free θα is rigid, so the
-- doubling witness (α,β ↦ l:𝓫), which needs count 1 at α, cannot factor through
-- any such θ. Complements wand_no_mgu_count: together the two canonical stuck
-- shapes (field-vs-vars and two-sided) are both proven ambiguous.
-- ⊢  ¬ ∃ mgu for (α | l:𝓫) ≐ᵣ (l:𝓫 | β)
theorem two_sided_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ ∃ θ : TySubst B,
        Unifies θ (.cat (.var "α") (.sing l (.base b)))
                  (.cat (.sing l (.base b)) (.var "β")) ∧
        ∀ θ' : TySubst B,
          Unifies θ' (.cat (.var "α") (.sing l (.base b)))
                     (.cat (.sing l (.base b)) (.var "β")) →
          InstanceOf θ' θ := by
  rintro ⟨θ, hu, hmgu⟩
  have hu' : RowEquiv (Row.cat (θ.row "α") (Row.sing l (.base b)))
                      (Row.cat (Row.sing l (.base b)) (θ.row "β")) := by
    have h := hu; unfold Unifies at h
    simpa only [Row.applySubst, Ty.applySubst] using h
  -- witness u₁ = (α,β ↦ ε): a unifier
  have hu1 : Unifies
      (⟨fun x => .var x, fun x => if x = "α" then .empty
        else if x = "β" then .empty else .var x⟩ : TySubst B)
      (.cat (.var "α") (.sing l (.base b))) (.cat (.sing l (.base b)) (.var "β")) := by
    unfold Unifies
    show RowEquiv (Row.cat Row.empty (Row.sing l (.base b)))
                  (Row.cat (Row.sing l (.base b)) Row.empty)
    exact RowEquiv.unitL.trans RowEquiv.unitR.symm
  have hI1 := hmgu _ hu1
  have hcα : sFieldCount l (θ.row "α").toSpine = 0 := by
    have h : sFieldCount l (θ.row "α").toSpine ≤ 0 := instanceOf_fieldCount_mono hI1 "α" l
    omega
  have hcβ : sFieldCount l (θ.row "β").toSpine = 0 := by
    have h : sFieldCount l (θ.row "β").toSpine ≤ 0 := instanceOf_fieldCount_mono hI1 "β" l
    omega
  -- the l-projections are empty (length = count = 0)
  have hπα : sProj l (θ.row "α").toSpine = [] := by
    rcases hh : sProj l (θ.row "α").toSpine with _ | ⟨p, ps⟩
    · rfl
    · exfalso
      have hlen : (sProj l (θ.row "α").toSpine).length = 0 := by
        rw [sProj_length_eq_sFieldCount]; exact hcα
      rw [hh] at hlen; simp at hlen
  have hπβ : sProj l (θ.row "β").toSpine = [] := by
    rcases hh : sProj l (θ.row "β").toSpine with _ | ⟨p, ps⟩
    · rfl
    · exfalso
      have hlen : (sProj l (θ.row "β").toSpine).length = 0 := by
        rw [sProj_length_eq_sFieldCount]; exact hcβ
      rw [hh] at hlen; simp at hlen
  -- θα is var-free: its single l-field would sit at segment |vars θα|, forced 0
  obtain ⟨-, hp⟩ := hu'.char
  have hpl := hp l
  have hsing : sProj l [Atom.field l (.base b)] = [(0, (.base b : Ty B))] := by
    simp [sProj]
  rw [show (Row.cat (θ.row "α") (Row.sing l (.base b))).toSpine
        = (θ.row "α").toSpine ++ [Atom.field l (.base b)] from rfl,
      show (Row.cat (Row.sing l (.base b)) (θ.row "β")).toSpine
        = [Atom.field l (.base b)] ++ (θ.row "β").toSpine from rfl,
      sProj_append, sProj_append, hπα, hπβ, hsing] at hpl
  simp only [List.nil_append, List.append_nil, List.map_cons, List.map_nil] at hpl
  have hvnil : sVarSeq (θ.row "α").toSpine = [] := by
    have hv0 : (sVarSeq (θ.row "α").toSpine).length = 0 := by
      obtain ⟨τ', rest, heq, -, -⟩ := hpl.cons_inv
      rw [List.cons.injEq, Prod.mk.injEq] at heq
      omega
    rcases hh : sVarSeq (θ.row "α").toSpine with _ | ⟨x, xs⟩
    · rfl
    · rw [hh] at hv0; simp at hv0
  have hvfα : (θ.row "α").SpineVarFree :=
    (spineVarFree_iff_varSeq_nil (θ.row "α")).mpr hvnil
  -- witness u₂ = (α,β ↦ l:𝓫): needs count 1 at α, which rigidity forbids
  have hu2 : Unifies
      (⟨fun x => .var x, fun x => if x = "α" then .sing l (.base b)
        else if x = "β" then .sing l (.base b) else .var x⟩ : TySubst B)
      (.cat (.var "α") (.sing l (.base b))) (.cat (.sing l (.base b)) (.var "β")) := by
    unfold Unifies
    show RowEquiv (Row.cat (Row.sing l (.base b)) (Row.sing l (.base b)))
                  (Row.cat (Row.sing l (.base b)) (Row.sing l (.base b)))
    exact RowEquiv.refl _
  have hI2 := hmgu _ hu2
  have hrig := instanceOf_fieldCount_eq_of_varFree hI2 hvfα l
  rw [hcα] at hrig
  simp [Row.toSpine, sFieldCount] at hrig

-- ## No-mgu depends only on the unifier SET
-- `HasMgu` packages "a most general unifier exists". Crucially InstanceOf never
-- mentions the rows — only θ's action on variables — so two unification problems
-- with the SAME unifiers (as substitutions) have the SAME mgu-status. This is the
-- vehicle for lifting stuck⟹no-mgu through the unifier-set-PRESERVING moves.
def HasMgu {B : Type} (ρ₁ ρ₂ : Row B) : Prop :=
  ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ ∧
    ∀ θ' : TySubst B, Unifies θ' ρ₁ ρ₂ → InstanceOf θ' θ

-- ⊢  (∀θ. θ ⊨ ρ₁≐ᵣρ₂  ↔  θ ⊨ ρ₁'≐ᵣρ₂')   ⟹   (HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂')
theorem hasMgu_congr {B : Type} {ρ₁ ρ₂ ρ₁' ρ₂' : Row B}
    (h : ∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ ↔ Unifies θ ρ₁' ρ₂') :
    HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂' :=
  ⟨fun ⟨θ, hu, hmax⟩ => ⟨θ, (h θ).mp hu, fun θ' hu' => hmax θ' ((h θ').mpr hu')⟩,
   fun ⟨θ, hu, hmax⟩ => ⟨θ, (h θ).mpr hu, fun θ' hu' => hmax θ' ((h θ').mp hu')⟩⟩

-- No-mgu is a ≈-INVARIANT: replacing either side by a ≈-equal row preserves the
-- unifier set (applySubst is a ≈-congruence), hence mgu-status. Lets the base
-- no-mgu theorems (stated on plain rows) transfer to the ofSpine forms the
-- algorithm produces.
-- ⊢  ρ₁ ≈ᵣ ρ₁',  ρ₂ ≈ᵣ ρ₂'   ⟹   (HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂')
theorem hasMgu_rowEquiv {B : Type} {ρ₁ ρ₂ ρ₁' ρ₂' : Row B}
    (h₁ : RowEquiv ρ₁ ρ₁') (h₂ : RowEquiv ρ₂ ρ₂') :
    HasMgu ρ₁ ρ₂ ↔ HasMgu ρ₁' ρ₂' :=
  hasMgu_congr (fun θ =>
    ⟨fun hu => ((RowEquiv.applySubst θ h₁).symm.trans hu).trans (RowEquiv.applySubst θ h₂),
     fun hu => ((RowEquiv.applySubst θ h₁).trans hu).trans (RowEquiv.applySubst θ h₂).symm⟩)

-- ## projClash soundness: the projection-clash direction of the trichotomy.
-- If projClash s₁ s₂, no θ can unify (ofSpine s₁) with (ofSpine s₂).
-- The ground side's l-count is fixed under substitution; the other side can
-- only grow; ≈ forces equality; omega closes the Nat arithmetic.
-- ⊢  projClash s₁ s₂   ⟹   ¬ ∃ θ. θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
theorem projClash_no_unifier {B : Type} {s₁ s₂ : List (Atom B)}
    (hclash : projClash s₁ s₂ = true) :
    ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s₁) (ofSpine s₂) := by
  unfold projClash at hclash
  rw [List.any_eq_true] at hclash
  obtain ⟨l, -, hl⟩ := hclash
  simp only [Bool.or_eq_true, Bool.and_eq_true, decide_eq_true_eq] at hl
  rintro ⟨θ, hunify⟩
  unfold Unifies at hunify
  have hfc := rowEquiv_fieldCount_eq l hunify
  have hle₁ : sFieldCount l s₁ ≤ sFieldCount l ((ofSpine s₁).applySubst θ).toSpine := by
    have h := sFieldCount_applySubst_le θ l (ofSpine s₁); rwa [ofSpine_toSpine] at h
  have hle₂ : sFieldCount l s₂ ≤ sFieldCount l ((ofSpine s₂).applySubst θ).toSpine := by
    have h := sFieldCount_applySubst_le θ l (ofSpine s₂); rwa [ofSpine_toSpine] at h
  rcases hl with ⟨hlt, hvf⟩ | ⟨hlt, hvf⟩
  · -- s₂ var-free: count fixed at sFieldCount l s₂ < sFieldCount l s₁
    have h₂ : sHasVar s₂ = false := by
      cases h : sHasVar s₂ with | false => rfl | true => simp [h] at hvf
    have heq₂ : sFieldCount l ((ofSpine s₂).applySubst θ).toSpine = sFieldCount l s₂ := by
      rw [sFieldCount_applySubst_varFree θ l (spineVarFree_ofSpine h₂), ofSpine_toSpine]
    omega
  · -- s₁ var-free: count fixed at sFieldCount l s₁ < sFieldCount l s₂
    have h₁ : sHasVar s₁ = false := by
      cases h : sHasVar s₁ with | false => rfl | true => simp [h] at hvf
    have heq₁ : sFieldCount l ((ofSpine s₁).applySubst θ).toSpine = sFieldCount l s₁ := by
      rw [sFieldCount_applySubst_varFree θ l (spineVarFree_ofSpine h₁), ofSpine_toSpine]
    omega

-- ## occurs: what the recursive-row check really guarantees
-- The occurs verdict (solveVar hitting `(sVarSeq s₂).contains α`) is CONSERVATIVE.
-- It is genuinely no-unifier only when the recursive variable is pinned by a
-- FIELD; an all-variable interior occurrence is unifiable by collapsing the
-- surrounding variables to ε.

-- A recursive variable α of ρ contributes its whole l-field count to (θρ) on top
-- of ρ's own explicit l-fields — because α occurs inside ρ, so θα is spliced in.
-- ⊢  α ∈ vars(spine ρ)   ⟹   count_l(spine ρ) + count_l(spine (θα)) ≤ count_l(spine (θρ))
theorem fieldCount_var_lower {B : Type} (θ : TySubst B) (l : Label) (α : TyVar) :
    (ρ : Row B) → α ∈ sVarSeq ρ.toSpine →
    sFieldCount l ρ.toSpine + sFieldCount l (θ.row α).toSpine
      ≤ sFieldCount l (ρ.applySubst θ).toSpine
  | .empty, h => by simp [Row.toSpine, sVarSeq] at h
  | .sing _ _, h => by simp [Row.toSpine, sVarSeq] at h
  | .var β, h => by
      simp only [Row.toSpine, sVarSeq, List.mem_singleton] at h
      subst h; simp [Row.applySubst, Row.toSpine, sFieldCount]
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.applySubst, Row.toSpine, sFieldCount_append, sVarSeq_append,
        List.mem_append] at h ⊢
      rcases h with h | h
      · have ih := fieldCount_var_lower θ l α ρ₁ h
        have hle := sFieldCount_applySubst_le θ l ρ₂
        omega
      · have ih := fieldCount_var_lower θ l α ρ₂ h
        have hle := sFieldCount_applySubst_le θ l ρ₁
        omega

-- The GENUINE occurs case: α occurs recursively in s₂ AND some label l has a
-- field there. ≈ pins l's count, but θα would have to hold that field's l-count
-- both on its own (as the lhs) and again inside the rhs — an impossible strict
-- growth. Mirrors projClash_no_unifier (the clash direction).
-- ⊢  α ∈ vars s₂,  0 < count_l s₂   ⟹   ¬ ∃ θ. θ ⊨ α ≐ᵣ ofSpine s₂
theorem occurs_field_no_unifier {B : Type} {α : TyVar} {s₂ : List (Atom B)}
    {l : Label} (hmem : α ∈ sVarSeq s₂) (hfield : 0 < sFieldCount l s₂) :
    ¬ ∃ θ : TySubst B, Unifies θ (.var α) (ofSpine s₂) := by
  rintro ⟨θ, hu⟩
  unfold Unifies at hu
  have hfc := rowEquiv_fieldCount_eq l hu
  simp only [Row.applySubst] at hfc
  have hlb := fieldCount_var_lower θ l α (ofSpine s₂) (by rw [ofSpine_toSpine]; exact hmem)
  rw [ofSpine_toSpine] at hlb
  omega

-- occurs is CONSERVATIVE, not sound-for-no-unifier: the all-variable interior
-- occurrence α ≐ᵣ (β | α | γ) is REPORTED occurs yet IS unifiable — take β,γ ↦ ε.
-- Only the field-pinned case (occurs_field_no_unifier) is a real non-unifier.
-- ⊢  unifyRow α (β|α|γ) = occurs   ∧   ∃ θ. θ ⊨ α ≐ᵣ (β|α|γ)
theorem occurs_allVar_unifiable {B : Type} :
    unifyRow (B := B) (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c")))
        = .occurs
    ∧ ∃ θ : TySubst B,
        Unifies θ (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c"))) :=
  ⟨rfl,
   ⟨⟨(.var ·), fun x => if x = "b" then .empty else if x = "c" then .empty else .var x⟩,
    by unfold Unifies
       simp only [Row.applySubst]
       exact (RowEquiv.unitL.trans RowEquiv.unitR).symm⟩⟩

-- ## Base-case clash: the OTHER place the algorithm answers clash
-- unifySpineF returns clash when one side is exhausted but the other still
-- carries a field (`allVarsEmpty = none`). This is the second half of clash
-- soundness (projClash_no_unifier is the interior/projection half): a field on
-- the leftover side has nowhere to go against the empty row.

-- allVarsEmpty fails exactly when a field remains: it walks vars, stops at a field.
-- ⊢  allVarsEmpty s = none   ⟹   ∃ l. 0 < count_l s
theorem allVarsEmpty_none_field {B : Type} :
    (s : List (Atom B)) → allVarsEmpty s = none → ∃ l, 0 < sFieldCount l s
  | [], h => by simp [allVarsEmpty] at h
  | .field l _ :: s, _ => ⟨l, by
      show 0 < (if l = l then 1 else 0) + sFieldCount l s
      rw [if_pos rfl]; omega⟩
  | .var _ :: s, h => by
      cases hs : allVarsEmpty s with
      | some σ => simp [allVarsEmpty, hs] at h
      | none =>
        obtain ⟨l, hl⟩ := allVarsEmpty_none_field s hs
        exact ⟨l, by simpa [sFieldCount] using hl⟩

-- ε cannot unify a row that still holds a field: ≈ pins the l-count at 0 on the
-- left, but substitution can only keep the leftover field's count positive.
-- ⊢  0 < count_l s   ⟹   ¬ ∃ θ. θ ⊨ ε ≐ᵣ ofSpine s
theorem empty_no_unifier {B : Type} {s : List (Atom B)} {l : Label}
    (hfield : 0 < sFieldCount l s) :
    ¬ ∃ θ : TySubst B, Unifies θ Row.empty (ofSpine s) := by
  rintro ⟨θ, hu⟩
  unfold Unifies at hu
  have hfc := rowEquiv_fieldCount_eq l hu
  simp only [Row.applySubst, Row.toSpine, sFieldCount] at hfc
  have hle := sFieldCount_applySubst_le θ l (ofSpine s)
  rw [ofSpine_toSpine] at hle
  omega

-- The base-case clash is sound (both orientations, by ≈-symmetry).
-- ⊢  allVarsEmpty s = none   ⟹   ¬ ∃ θ. θ ⊨ ε ≐ᵣ ofSpine s
theorem allVarsEmpty_none_no_unifier {B : Type} {s : List (Atom B)}
    (h : allVarsEmpty s = none) :
    ¬ ∃ θ : TySubst B, Unifies θ Row.empty (ofSpine s) :=
  let ⟨_, hl⟩ := allVarsEmpty_none_field s h
  empty_no_unifier hl

-- ⊢  allVarsEmpty s = none   ⟹   ¬ ∃ θ. θ ⊨ ofSpine s ≐ᵣ ε
theorem allVarsEmpty_none_no_unifier' {B : Type} {s : List (Atom B)}
    (h : allVarsEmpty s = none) :
    ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s) Row.empty := by
  rintro ⟨θ, hu⟩
  refine allVarsEmpty_none_no_unifier h ⟨θ, ?_⟩
  unfold Unifies at hu ⊢
  exact hu.symm

------------------------- ≐ᵣ SUCCESS SOUNDNESS (SETUP) -----------------------
-- The success case emits a row-var solution list σ and residual type
-- equations eqs. A substitution θ "extends σ" when it agrees with every
-- binding (α ≔ ρ) up to ≈ under θ, and "satisfies eqs" when it makes every
-- emitted pair ≈-equal. Soundness (below/next): under both, θ unifies the
-- original rows. The individual MOVE-REFLECTION lemmas here are the reusable
-- content — each says "if θ unifies the residual, it unified the original".

def SolSat {B : Type} (θ : TySubst B) (σ : List (TyVar × Row B)) : Prop :=
  ∀ p ∈ σ, RowEquiv (θ.row p.1) (p.2.applySubst θ)

def EqsSat {B : Type} (θ : TySubst B) (eqs : List (Ty B × Ty B)) : Prop :=
  ∀ p ∈ eqs, TyEquiv (p.1.applySubst θ) (p.2.applySubst θ)

theorem EqsSat.cons {B : Type} {θ : TySubst B} {τ τ' : Ty B} {eqs : List (Ty B × Ty B)}
    (hty : TyEquiv (τ.applySubst θ) (τ'.applySubst θ)) (h : EqsSat θ eqs) :
    EqsSat θ ((τ, τ') :: eqs) := by
  intro p hp
  rcases List.mem_cons.mp hp with rfl | hp'
  · exact hty
  · exact h p hp'

-- addEq only prepends to the eqs of a success; it inverts cleanly.
-- ⊢  r.addEq τ τ' = success σ eqs
--        ⟹  ∃ eqs'. r = success σ eqs' ∧ eqs = (τ,τ')::eqs'
theorem URes.addEq_success {B : Type} {τ τ' : Ty B} {r : URes B}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)} :
    r.addEq τ τ' = .success σ eqs →
    ∃ eqs', r = .success σ eqs' ∧ eqs = (τ, τ') :: eqs' := by
  cases r with
  | success σ₀ eqs₀ =>
      intro h; simp only [URes.addEq] at h
      obtain ⟨hσ, heq⟩ := URes.success.inj h
      exact ⟨eqs₀, by rw [hσ], heq.symm⟩
  | clash  => intro h; cases (h : URes.clash = .success σ eqs)
  | occurs => intro h; cases (h : URes.occurs = .success σ eqs)
  | stuck  => intro h; cases (h : URes.stuck = .success σ eqs)

-- ## ofSpine cons is definitionally a cat (kept as named rewrites).
-- ⊢  ofSpine (α :: s) = (α | ofSpine s)
theorem ofSpine_var_cons {B : Type} (α : TyVar) (s : List (Atom B)) :
    ofSpine (.var α :: s) = .cat (.var α) (ofSpine s) := rfl

-- ⊢  ofSpine ((l:τ) :: s) = (l:τ | ofSpine s)
theorem ofSpine_field_cons {B : Type} (l : Label) (τ : Ty B) (s : List (Atom B)) :
    ofSpine (.field l τ :: s) = .cat (.sing l τ) (ofSpine s) := rfl

-- ## Move-reflection lemmas
-- Each: "if θ unifies the residual (and satisfies the emitted binding/eq),
-- then θ unified the original pair." These are the per-rule soundness steps.

-- U-var-refl (left): a shared leading var contributes θα to both sides, so it
-- drops out of the equivalence — no constraint on θ.
-- Inversion: stripL succeeds only on a shared leading var.
-- ⊢  stripL s₁ s₂ = some (t₁,t₂)   ⟹   ∃ α. s₁ = α::t₁ ∧ s₂ = α::t₂
theorem stripL_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} :
    stripL s₁ s₂ = some (t₁, t₂) → ∃ α, s₁ = .var α :: t₁ ∧ s₂ = .var α :: t₂ := by
  cases s₁ with
  | nil => simp [stripL]
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [stripL]
    | var α =>
      cases s₂ with
      | nil => simp [stripL]
      | cons a₂ r₂ =>
        cases a₂ with
        | field _ _ => simp [stripL]
        | var β =>
          intro h
          simp only [stripL] at h
          split at h
          · rename_i hαβ
            simp only [Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            exact ⟨α, rfl, by rw [hαβ]⟩
          · simp at h

-- Inversion: stripR succeeds only on a shared trailing var.
-- ⊢  stripR s₁ s₂ = some (t₁,t₂)   ⟹   ∃ α. s₁ = t₁++[α] ∧ s₂ = t₂++[α]
theorem stripR_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} :
    stripR s₁ s₂ = some (t₁, t₂) → ∃ α, s₁ = t₁ ++ [.var α] ∧ s₂ = t₂ ++ [.var α] := by
  intro h
  unfold stripR at h
  cases hsl : stripL s₁.reverse s₂.reverse with
  | none => rw [hsl] at h; simp at h
  | some p =>
    rw [hsl] at h
    obtain ⟨u₁, u₂⟩ := p
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    obtain ⟨α, hr₁, hr₂⟩ := stripL_inv hsl
    refine ⟨α, ?_, ?_⟩
    · rw [← List.reverse_reverse s₁, hr₁]; simp
    · rw [← List.reverse_reverse s₂, hr₂]; simp

-- U-var-refl (left): a shared leading var contributes θα to both sides, so it
-- drops out of the equivalence — no constraint on θ.
-- ⊢  stripL s₁ s₂ = some (t₁,t₂),  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem stripL_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv hstrip
  simp only [ofSpine_var_cons, Row.applySubst]
  exact RowEquiv.cat (.refl _) hrec

-- U-var-refl (right): the same at the trailing end, via ofSpine over append.
-- ⊢  stripR s₁ s₂ = some (t₁,t₂),  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem stripR_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv hstrip
  have e₁ := RowEquiv.applySubst θ (ofSpine_append t₁ [Atom.var α])
  have e₂ := RowEquiv.applySubst θ (ofSpine_append t₂ [Atom.var α])
  simp only [ofSpine, Row.applySubst] at e₁ e₂
  exact e₁.trans ((RowEquiv.cat hrec (.refl _)).trans e₂.symm)

-- U-var-solve: s₁ = [var α], θ satisfies α ≔ ofSpine s₂ ⟹ θ unifies.
-- ⊢  solveVar s₁ s₂ = some (success σ eqs),  θ ⊨ σ
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem solveVar_reflect {B : Type} {θ : TySubst B} {s₁ s₂ : List (Atom B)}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (hsolve : solveVar s₁ s₂ = some (.success σ eqs))
    (hsol : SolSat θ σ) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  cases s₁ with
  | nil => simp [solveVar] at hsolve
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVar] at hsolve
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVar] at hsolve
      | nil =>
        simp only [solveVar] at hsolve
        split at hsolve
        · simp at hsolve
        · simp only [Option.some.injEq, URes.success.injEq] at hsolve
          obtain ⟨rfl, -⟩ := hsolve
          have hbind := hsol (α, ofSpine s₂) (by simp)
          simp only [ofSpine, Row.applySubst]
          exact RowEquiv.unitR.trans hbind

-- U-field (left): a leading field matched against the other side's window.
-- windowExtract bubbles the matched field to the front (distinct-label comm).
-- ⊢  windowExtract l s = some (τ,s')   ⟹   ofSpine s ≈ᵣ (l:τ | ofSpine s')
theorem windowExtract_equiv {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') →
    RowEquiv (ofSpine s) (.cat (.sing l τ) (ofSpine s'))
  | [], _, _, h => by simp [windowExtract] at h
  | .var β :: s, _, _, h => by simp [windowExtract] at h
  | .field l' τ₀ :: s, τ, s', h => by
      simp only [windowExtract] at h
      split at h
      · rename_i hl
        subst hl
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact RowEquiv.refl _
      · rename_i hl
        split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := windowExtract_equiv l s hwe
          simp only [ofSpine_field_cons]
          exact (RowEquiv.cat (.refl _) ih).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (.comm hl) (.refl _)).trans RowEquiv.assoc))
        · simp at h

-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem matchL_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  cases s₁ with
  | nil => simp [matchL] at hmatch
  | cons a₁ r₁ =>
    cases a₁ with
    | var α => simp [matchL] at hmatch
    | field l τ₀ =>
      simp only [matchL] at hmatch
      split at hmatch
      · rename_i τ'' s₂' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
        obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
        have hw := RowEquiv.applySubst θ (windowExtract_equiv l s₂ hwe)
        simp only [ofSpine_field_cons, Row.applySubst]
        refine RowEquiv.trans ?_ hw.symm
        simp only [Row.applySubst]
        exact RowEquiv.cat (.sing heq) hrec
      · simp at hmatch

-- Base case: an all-vars remainder, each solved to ε, unifies with the empty
-- side (allVarsEmpty forces every var to ε).
-- ⊢  allVarsEmpty s = some σ,  θ ⊨ σ   ⟹   θ(ofSpine s) ≈ᵣ ε
theorem allVarsEmpty_sound {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → SolSat θ σ →
    RowEquiv ((ofSpine s).applySubst θ) .empty
  | [], _, hσ, _ => by
      simp only [allVarsEmpty, Option.some.injEq] at hσ; subst hσ; exact .refl _
  | .field _ _ :: _, _, hσ, _ => by simp [allVarsEmpty] at hσ
  | .var α :: s, σ, hσ, hsol => by
      simp only [allVarsEmpty] at hσ
      cases hs : allVarsEmpty s with
      | none => rw [hs] at hσ; simp at hσ
      | some σ' =>
        rw [hs] at hσ
        have hσ' : σ = (α, Row.empty) :: σ' := (Option.some.inj hσ).symm
        subst hσ'
        have hhead := hsol (α, Row.empty) (by simp)
        have htail : SolSat θ σ' := fun p hp => hsol p (by simp [hp])
        have ih := allVarsEmpty_sound s hs htail
        simp only [ofSpine_var_cons, Row.applySubst]
        exact (RowEquiv.cat hhead ih).trans RowEquiv.unitL

-- Completeness counterpart: if θ makes ofSpine s collapse to ε (the exhausted-
-- side unifier), then θ satisfies every ε-binding allVarsEmpty emitted.
-- ⊢  allVarsEmpty s = some σ,  θ(ofSpine s) ≈ᵣ ε   ⟹   SolSat θ σ
theorem allVarsEmpty_complete {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → RowEquiv ((ofSpine s).applySubst θ) .empty →
    SolSat θ σ
  | [], _, hσ, _ => by
      simp only [allVarsEmpty, Option.some.injEq] at hσ; subst hσ
      intro p hp; simp at hp
  | .field _ _ :: _, _, hσ, _ => by simp [allVarsEmpty] at hσ
  | .var α :: s, σ, hσ, hu => by
      simp only [allVarsEmpty] at hσ
      cases hs : allVarsEmpty s with
      | none => rw [hs] at hσ; simp at hσ
      | some σ' =>
        rw [hs] at hσ
        have hσ' : σ = (α, Row.empty) :: σ' := (Option.some.inj hσ).symm
        subst hσ'
        simp only [ofSpine_var_cons, Row.applySubst] at hu
        obtain ⟨hα, hrest⟩ := hu.cat_empty_split
        have ih := allVarsEmpty_complete s hs hrest
        intro p hp
        rcases List.mem_cons.mp hp with rfl | hp'
        · simp only [Row.applySubst]; exact hα
        · exact ih p hp'

-- Row reversal (reverses cat order): the algebraic image of List.reverse on
-- spines. Used only to transport windowExtract_equiv from s.reverse to s — no
-- List.reverseRecOn is available (no Batteries dep), so we go through revRow.
def revRow {B : Type} : Row B → Row B
  | .empty     => .empty
  | .var α     => .var α
  | .sing l τ  => .sing l τ
  | .cat ρ₁ ρ₂ => .cat (revRow ρ₂) (revRow ρ₁)

-- ⊢  revRow (revRow ρ) = ρ
theorem revRow_involutive {B : Type} : (ρ : Row B) → revRow (revRow ρ) = ρ
  | .empty     => rfl
  | .var _     => rfl
  | .sing _ _  => rfl
  | .cat a b   => by simp only [revRow, revRow_involutive a, revRow_involutive b]

-- ⊢  a ≈ᵣ b   ⟹   revRow a ≈ᵣ revRow b
theorem RowEquiv.revRow {B : Type} :
    {a b : Row B} → RowEquiv a b → RowEquiv (revRow a) (revRow b)
  | _, _, .refl _      => .refl _
  | _, _, .symm h      => (RowEquiv.revRow h).symm
  | _, _, .trans h₁ h₂ => (RowEquiv.revRow h₁).trans (RowEquiv.revRow h₂)
  | _, _, .sing hty    => .sing hty
  | _, _, .cat h₁ h₂   => .cat (RowEquiv.revRow h₂) (RowEquiv.revRow h₁)
  | _, _, .assoc       => RowEquiv.assoc.symm
  | _, _, .unitL       => RowEquiv.unitR
  | _, _, .unitR       => RowEquiv.unitL
  | _, _, .comm hne    => RowEquiv.comm (fun h => hne h.symm)

-- ofSpine of a reversed spine is the row-reversal of ofSpine (mod ≈).
-- ⊢  ofSpine (reverse as) ≈ᵣ revRow (ofSpine as)
theorem ofSpine_reverse_equiv {B : Type} : (as : List (Atom B)) →
    RowEquiv (ofSpine as.reverse) (revRow (ofSpine as))
  | [] => .refl _
  | .field l τ :: t => by
      rw [List.reverse_cons]
      refine (ofSpine_append t.reverse [Atom.field l τ]).trans ?_
      simp only [ofSpine, revRow]
      exact RowEquiv.cat (ofSpine_reverse_equiv t) RowEquiv.unitR
  | .var α :: t => by
      rw [List.reverse_cons]
      refine (ofSpine_append t.reverse [Atom.var α]).trans ?_
      simp only [ofSpine, revRow]
      exact RowEquiv.cat (ofSpine_reverse_equiv t) RowEquiv.unitR

-- U-field (right): the trailing-end mirror of matchL. matchR runs windowExtract
-- on the REVERSED spines, so the matched field sits in the trailing var-free
-- window and bubbles to the RIGHT end past distinct-label fields only. We get
-- this by transporting windowExtract_equiv through revRow.
-- ⊢  windowExtract l (reverse s) = some (τ,q)
--        ⟹   ofSpine s ≈ᵣ (ofSpine (reverse q) | l:τ)
theorem windowExtract_reverse_equiv {B : Type} (l : Label) (s : List (Atom B))
    {τ : Ty B} {q : List (Atom B)}
    (h : windowExtract l s.reverse = some (τ, q)) :
    RowEquiv (ofSpine s) (.cat (ofSpine q.reverse) (.sing l τ)) := by
  have hwe := windowExtract_equiv l s.reverse h
  have h1 : RowEquiv (revRow (ofSpine s)) (.cat (.sing l τ) (ofSpine q)) :=
    (ofSpine_reverse_equiv s).symm.trans hwe
  have h2 := RowEquiv.revRow h1
  rw [revRow_involutive (ofSpine s)] at h2
  simp only [revRow] at h2
  exact h2.trans (RowEquiv.cat (ofSpine_reverse_equiv q).symm (.refl _))

-- Inversion of a leading field-match.
-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂)
--        ⟹  ∃ l. s₁ = (l:τ)::t₁ ∧ windowExtract l s₂ = some (τ',t₂)
theorem matchL_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B} :
    matchL s₁ s₂ = some (τ, τ', t₁, t₂) →
    ∃ l, s₁ = .field l τ :: t₁ ∧ windowExtract l s₂ = some (τ', t₂) := by
  cases s₁ with
  | nil => simp [matchL]
  | cons a₁ r₁ =>
    cases a₁ with
    | var α => simp [matchL]
    | field l τ₀ =>
      intro h
      simp only [matchL] at h
      split at h
      · rename_i τ'' s₂' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl, rfl⟩ := h
        exact ⟨l, rfl, hwe⟩
      · simp at h

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)      (trailing-field mirror)
theorem matchR_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  -- matchR = matchL on the reverses; recover the leading-field inversion there.
  unfold matchR at hmatch
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at hmatch; simp at hmatch
  | some p =>
    rw [hml] at hmatch
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    -- matchR returns (τa, τb, u₁.reverse, u₂.reverse); pin the theorem vars.
    simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
    obtain ⟨l, hrev, hwe⟩ := matchL_inv hml
    -- hrev : s₁.reverse = field l τa :: u₁ ; so s₁ = u₁.reverse ++ [field l τa]
    have hs₁ : s₁ = u₁.reverse ++ [Atom.field l τa] := by
      rw [← List.reverse_reverse s₁, hrev]; simp
    -- windowExtract l s₂.reverse = some (τb, u₂); right-bubble on s₂.
    have hs₂equiv := windowExtract_reverse_equiv l s₂ hwe
    have hs₁equiv : RowEquiv (ofSpine s₁) (.cat (ofSpine u₁.reverse) (.sing l τa)) := by
      rw [hs₁]
      exact (ofSpine_append u₁.reverse [Atom.field l τa]).trans
        (RowEquiv.cat (.refl _) RowEquiv.unitR)
    have e₁ := RowEquiv.applySubst θ hs₁equiv
    have e₂ := RowEquiv.applySubst θ hs₂equiv
    simp only [Row.applySubst] at e₁ e₂
    refine e₁.trans (RowEquiv.trans ?_ e₂.symm)
    exact RowEquiv.cat hrec (.sing heq)

-- ## FORWARD reflection: a unifier of the ORIGINAL unifies the RESIDUAL (+ eqs)
-- The converse of the *_reflect lemmas — the COMPLETENESS direction each move
-- needs. For strip this is just cancellativity (shared θα prefix/suffix). For
-- match it is leading/trailing-field cancellation (field_cancel_left/right),
-- which additionally EXTRACTS the emitted type equation θτ ≈ θτ'. Together these
-- say every move preserves the unifier set, so no-unifier propagates backwards.

theorem RowEquiv.field_cancel_right {B : Type} {l : Label} {τ₁ τ₂ : Ty B}
    {R₁ R₂ : Row B}
    (h : RowEquiv (.cat R₁ (.sing l τ₁)) (.cat R₂ (.sing l τ₂))) :
    TyEquiv τ₁ τ₂ ∧ RowEquiv R₁ R₂ := by
  obtain ⟨hty, hR⟩ := (h.revRow).field_cancel_left
  refine ⟨hty, ?_⟩
  have hRR := hR.revRow
  rwa [revRow_involutive, revRow_involutive] at hRR

-- ⊢  stripL s₁ s₂ = some (t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem stripL_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv hstrip
  simp only [ofSpine, Row.applySubst] at hu
  exact hu.cancel_cat_left

-- ⊢  stripR s₁ s₂ = some (t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem stripR_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv hstrip
  have e₁ := RowEquiv.applySubst θ (ofSpine_append t₁ [Atom.var α])
  have e₂ := RowEquiv.applySubst θ (ofSpine_append t₂ [Atom.var α])
  simp only [ofSpine, Row.applySubst] at e₁ e₂
  exact (e₁.symm.trans (hu.trans e₂)).cancel_cat_right

-- ## Strip moves reflect no-mgu (the unifier-set-preserving case of the lift)
-- stripL/stripR cancel a shared end-var, and reflect + reflect_fwd together show
-- they preserve the unifier set EXACTLY. So mgu-status transfers both ways: a
-- config that gets stuck purely by end-stripping inherits the no-mgu verdict of
-- its stripped core. (matchL/matchR/groundMatch emit a type equation, so they do
-- NOT preserve the unifier set — those need the harder augmented-witness lift.)
-- ⊢  stripL s₁ s₂ = some (t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--                                       HasMgu (ofSpine t₁)(ofSpine t₂))
theorem stripL_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔ HasMgu (ofSpine t₁) (ofSpine t₂) :=
  hasMgu_congr (fun _ => ⟨fun hu => stripL_reflect_fwd hstrip hu,
                          fun hu => stripL_reflect hstrip hu⟩)

-- ⊢  stripR s₁ s₂ = some (t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--                                       HasMgu (ofSpine t₁)(ofSpine t₂))
theorem stripR_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔ HasMgu (ofSpine t₁) (ofSpine t₂) :=
  hasMgu_congr (fun _ => ⟨fun hu => stripR_reflect_fwd hstrip hu,
                          fun hu => stripR_reflect hstrip hu⟩)

-- End-to-end demonstration that the pieces compose: the Wand core wrapped in a
-- shared leading var. stripL peels γ, the stripped core is Wand's no-mgu config,
-- so the whole thing has no mgu — obtained mechanically from stripL_hasMgu_iff +
-- hasMgu_rowEquiv + wand_no_mgu_count. (This is exactly how the eventual lift
-- discharges its strip arms.)
-- ⊢  ¬ HasMgu (γ | β | α) (γ | l:𝓫)
theorem wand_under_strip_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ HasMgu (ofSpine [Atom.var "γ", .var "β", .var "α"])
             (ofSpine [Atom.var "γ", .field l (.base b)]) := by
  have hs : stripL ([Atom.var "γ", .var "β", .var "α"] : List (Atom B))
                   [Atom.var "γ", .field l (.base b)]
          = some ([.var "β", .var "α"], [.field l (.base b)]) := rfl
  rw [stripL_hasMgu_iff hs]
  simp only [ofSpine]
  rw [hasMgu_rowEquiv (ρ₁' := .cat (.var "β") (.var "α")) (ρ₂' := .sing l (.base b))
        (RowEquiv.cat (RowEquiv.refl _) RowEquiv.unitR) RowEquiv.unitR]
  exact wand_no_mgu_count b l

-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem matchL_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨l, rfl, hwe⟩ := matchL_inv hmatch
  have hs₂ := RowEquiv.applySubst θ (windowExtract_equiv l s₂ hwe)
  simp only [ofSpine, Row.applySubst] at hu hs₂
  exact (hu.trans hs₂).field_cancel_left

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem matchR_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  unfold matchR at hmatch
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at hmatch; simp at hmatch
  | some p =>
    rw [hml] at hmatch
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
    obtain ⟨l, hrev, hwe⟩ := matchL_inv hml
    have hs₁ : s₁ = u₁.reverse ++ [Atom.field l τa] := by
      rw [← List.reverse_reverse s₁, hrev]; simp
    have hs₂equiv := windowExtract_reverse_equiv l s₂ hwe
    have hs₁equiv : RowEquiv (ofSpine s₁) (.cat (ofSpine u₁.reverse) (.sing l τa)) := by
      rw [hs₁]
      exact (ofSpine_append u₁.reverse [Atom.field l τa]).trans
        (RowEquiv.cat (.refl _) RowEquiv.unitR)
    have e₁ := RowEquiv.applySubst θ hs₁equiv
    have e₂ := RowEquiv.applySubst θ hs₂equiv
    simp only [Row.applySubst] at e₁ e₂
    exact (e₁.symm.trans (hu.trans e₂)).field_cancel_right

-- ## U-ground: the reusable algebraic core
-- A field ≈-commutes past a row that is BOTH var-free and l-free. (Past a var
-- it would NOT commute — shadowing — so both hypotheses are essential; this is
-- exactly why groundMatch's soundness is conditional on the skipped vars being
-- l-free under θ, which the counting forces.)
-- ⊢  R var-free,  count_l(spine R) = 0   ⟹   (l:τ | R) ≈ᵣ (R | l:τ)
theorem field_comm_lfree {B : Type} (l : Label) (τ : Ty B) :
    (R : Row B) → R.SpineVarFree → sFieldCount l R.toSpine = 0 →
    RowEquiv (.cat (.sing l τ) R) (.cat R (.sing l τ))
  | .empty, _, _ => RowEquiv.unitR.trans RowEquiv.unitL.symm
  | .var _, hv, _ => nomatch hv
  | .sing l' τ', _, hc => by
      have hne : l' ≠ l := by
        intro h; subst h; simp [Row.toSpine, sFieldCount] at hc
      exact RowEquiv.comm (fun h => hne h.symm)
  | .cat R₁ R₂, .cat hv₁ hv₂, hc => by
      rw [Row.toSpine, sFieldCount_append] at hc
      have hc₁ : sFieldCount l R₁.toSpine = 0 := by omega
      have hc₂ : sFieldCount l R₂.toSpine = 0 := by omega
      have IH₁ := field_comm_lfree l τ R₁ hv₁ hc₁
      have IH₂ := field_comm_lfree l τ R₂ hv₂ hc₂
      exact RowEquiv.assoc.symm.trans
        ((RowEquiv.cat IH₁ (.refl _)).trans
          (RowEquiv.assoc.trans
            ((RowEquiv.cat (.refl _) IH₂).trans RowEquiv.assoc.symm)))

-- removeField pulls the matched l-field to the front — UNDER θ, provided every
-- variable of the spine is var-free and l-free under θ (vacuous when the spine
-- is var-free, e.g. the ground side). The var case is the only one that needs
-- field_comm_lfree; the field case only crosses distinct labels (comm).
-- ⊢  removeField l s = some (τ,t),
--       (∀ β ∈ vars(s). θβ var-free ∧ count_l(spine θβ) = 0)
--        ⟹   θ(ofSpine s) ≈ᵣ (l:θτ | θ(ofSpine t))
theorem removeField_equiv_of {B : Type} {θ : TySubst B} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) →
    (∀ β ∈ sVarSeq s, (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0) →
    RowEquiv ((ofSpine s).applySubst θ)
             (.cat (.sing l (τ.applySubst θ)) ((ofSpine t).applySubst θ))
  | [], _, _, h, _ => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h, hvars => by
      simp only [removeField] at h
      split at h
      · rename_i hl'
        subst hl'
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [ofSpine_field_cons, Row.applySubst]
        exact .refl _
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have hvars' : ∀ β ∈ sVarSeq s,
              (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0 :=
            fun β hβ => hvars β (by simp only [sVarSeq]; exact hβ)
          have IH := removeField_equiv_of l s hrem hvars'
          simp only [ofSpine_field_cons, Row.applySubst]
          exact (RowEquiv.cat (.refl _) IH).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (RowEquiv.comm hl') (.refl _)).trans RowEquiv.assoc))
        · simp at h
  | .var β :: s, τ, t, h, hvars => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have hβ : (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0 :=
          hvars β (by simp [sVarSeq])
        have hvars' : ∀ γ ∈ sVarSeq s,
            (θ.row γ).SpineVarFree ∧ sFieldCount l (θ.row γ).toSpine = 0 :=
          fun γ hγ => hvars γ (by simp only [sVarSeq]; exact List.mem_cons_of_mem β hγ)
        have IH := removeField_equiv_of l s hrem hvars'
        simp only [ofSpine_var_cons, Row.applySubst]
        exact (RowEquiv.cat (.refl _) IH).trans
          (RowEquiv.assoc.symm.trans
            ((RowEquiv.cat
              (field_comm_lfree l (τ''.applySubst θ) (θ.row β) hβ.1 hβ.2).symm (.refl _)).trans
              RowEquiv.assoc))
      · simp at h

-- ## U-ground: the counting (measurement lemmas)
-- removeField keeps the variable sequence (it only deletes a concrete field).
-- ⊢  removeField l s = some (τ,t)   ⟹   vars(t) = vars(s)
theorem removeField_sVarSeq {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) → sVarSeq t = sVarSeq s
  | [], _, _, h => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          simp only [sVarSeq]; exact removeField_sVarSeq l s hrem
        · simp at h
  | .var β :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sVarSeq]; rw [removeField_sVarSeq l s hrem]
      · simp at h

-- removeField deletes exactly one l-field.
-- ⊢  removeField l s = some (τ,t)   ⟹   count_l(s) = count_l(t) + 1
theorem removeField_sFieldCount {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) → sFieldCount l s = sFieldCount l t + 1
  | [], _, _, h => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i hl'
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sFieldCount, if_pos hl']; omega
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have hcnt := removeField_sFieldCount l s hrem
          simp only [sFieldCount, if_neg hl']
          omega
        · simp at h
  | .var β :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sFieldCount]
        rw [removeField_sFieldCount l s hrem]
      · simp at h

-- A var-free spine stays var-free under θ (θ introduces vars only via row-vars).
-- ⊢  vars(s) = []   ⟹   vars(spine (θ(ofSpine s))) = []
theorem varSeq_applySubst_nil {B : Type} (θ : TySubst B) :
    (s : List (Atom B)) → sVarSeq s = [] →
    sVarSeq ((ofSpine s).applySubst θ).toSpine = []
  | [], _ => rfl
  | .field l τ :: s, hs => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sVarSeq_append,
        sVarSeq, List.nil_append]
      exact varSeq_applySubst_nil θ s (by simpa [sVarSeq] using hs)
  | .var β :: s, hs => by simp [sVarSeq] at hs

-- If the θ-image of ofSpine s carries NO spine variable, every variable of s is
-- var-free under θ.
-- ⊢  vars(spine (θ(ofSpine s))) = []   ⟹   ∀ β ∈ vars(s). θβ var-free
theorem allVars_varFree_of {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) →
    sVarSeq ((ofSpine s).applySubst θ).toSpine = [] →
    ∀ β ∈ sVarSeq s, (θ.row β).SpineVarFree
  | [], _, β, hβ => by simp [sVarSeq] at hβ
  | .field l τ :: s, h, β, hβ => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sVarSeq_append,
        sVarSeq, List.nil_append] at h
      exact allVars_varFree_of s h β (by simpa [sVarSeq] using hβ)
  | .var γ :: s, h, β, hβ => by
      simp only [ofSpine_var_cons, Row.applySubst, Row.toSpine, sVarSeq_append] at h
      obtain ⟨hγ, hs⟩ := List.append_eq_nil_iff.mp h
      simp only [sVarSeq] at hβ
      rcases List.mem_cons.mp hβ with rfl | hβ'
      · exact (spineVarFree_iff_varSeq_nil _).mpr hγ
      · exact allVars_varFree_of s hs β hβ'

-- If θ does not INCREASE the l-count of ofSpine s (it never decreases it), then
-- every variable of s is l-free under θ — the U-ground counting fact.
-- ⊢  count_l(spine (θ(ofSpine s))) = count_l(s)
--        ⟹   ∀ β ∈ vars(s). count_l(spine θβ) = 0
theorem allVars_lfree_of {B : Type} {θ : TySubst B} (l : Label) :
    (s : List (Atom B)) →
    sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s →
    ∀ β ∈ sVarSeq s, sFieldCount l (θ.row β).toSpine = 0
  | [], _, β, hβ => by simp [sVarSeq] at hβ
  | .field l' τ :: s, h, β, hβ => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sFieldCount_append,
        sFieldCount] at h
      have h' : sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s := by omega
      exact allVars_lfree_of l s h' β (by simpa [sVarSeq] using hβ)
  | .var γ :: s, h, β, hβ => by
      simp only [ofSpine_var_cons, Row.applySubst, Row.toSpine, sFieldCount_append,
        sFieldCount] at h
      have hmono : sFieldCount l s ≤ sFieldCount l ((ofSpine s).applySubst θ).toSpine := by
        have hle := sFieldCount_applySubst_le θ l (ofSpine s); rwa [ofSpine_toSpine] at hle
      have hs : sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s := by omega
      simp only [sVarSeq] at hβ
      rcases List.mem_cons.mp hβ with rfl | hβ'
      · omega
      · exact allVars_lfree_of l s hs β hβ'

-- ## U-ground: inversion + the reflection lemma
-- ⊢  groundMatchAux s₁ s₂ ls = some (τ,τ',t₁,t₂)   ⟹   ∃ l.
--       count_l(s₁) = count_l(s₂) ∧ 0 < count_l(s₁)
--       ∧ removeField l s₁ = some (τ,t₁) ∧ removeField l s₂ = some (τ',t₂)
theorem groundMatchAux_inv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} :
    (ls : List Label) → groundMatchAux s₁ s₂ ls = some (τ, τ', t₁, t₂) →
    ∃ l, sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ ∧
         removeField l s₁ = some (τ, t₁) ∧ removeField l s₂ = some (τ', t₂)
  | [], h => by simp [groundMatchAux] at h
  | l :: ls, h => by
      simp only [groundMatchAux] at h
      split at h
      · rename_i hcond
        cases hr₁ : removeField l s₁ with
        | none => simp only [hr₁] at h; exact groundMatchAux_inv ls h
        | some p₁ =>
          cases hr₂ : removeField l s₂ with
          | none => simp only [hr₁, hr₂] at h; exact groundMatchAux_inv ls h
          | some p₂ =>
            obtain ⟨τa, ta⟩ := p₁
            obtain ⟨τb, tb⟩ := p₂
            simp only [hr₁, hr₂, Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl, rfl, rfl⟩ := h
            exact ⟨l, hcond.1, hcond.2, hr₁, hr₂⟩
      · exact groundMatchAux_inv ls h

-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   vars(s₂) = [] ∧ ∃ l.
--       count_l(s₁) = count_l(s₂) ∧ 0 < count_l(s₁)
--       ∧ removeField l s₁ = some (τ,t₁) ∧ removeField l s₂ = some (τ',t₂)
theorem groundMatch_inv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} :
    groundMatch s₁ s₂ = some (τ, τ', t₁, t₂) →
    sVarSeq s₂ = [] ∧ ∃ l, sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ ∧
      removeField l s₁ = some (τ, t₁) ∧ removeField l s₂ = some (τ', t₂) := by
  intro h
  unfold groundMatch at h
  split at h
  · simp at h
  · rename_i hnv
    exact ⟨(sHasVar_false_iff s₂).mp (by simpa using hnv), groundMatchAux_inv (sLabels s₁) h⟩

-- U-ground reflection: the paired l-field bubbles out of BOTH sides. The ground
-- side (s₂ var-free) is a direct removeField_equiv_of. The other side needs its
-- variables to be l-free under θ — which the counting forces: hrec pins the
-- l-count of (ofSpine t₁)θ to that of the var-free (ofSpine t₂)θ, and
-- sFieldCount l s₁ = sFieldCount l s₂ then makes θ introduce zero l-fields
-- across s₁'s variables (allVars_lfree_of); likewise they stay var-free
-- (allVars_varFree_of). This is the one move whose soundness is genuinely
-- non-local — it reads the residual solution back through the counting.
-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem groundMatch_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨hs₂vars, l, hcount, _, hr₁, hr₂⟩ := groundMatch_inv hg
  have hs₂equiv : RowEquiv ((ofSpine s₂).applySubst θ)
      (.cat (.sing l (τ'.applySubst θ)) ((ofSpine t₂).applySubst θ)) :=
    removeField_equiv_of l s₂ hr₂ (fun β hβ => by simp [hs₂vars] at hβ)
  have ht₂vars : sVarSeq t₂ = [] := by rw [removeField_sVarSeq l s₂ hr₂]; exact hs₂vars
  have ht₂vf : (ofSpine t₂).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact ht₂vars)
  have hv₁nil : sVarSeq ((ofSpine t₁).applySubst θ).toSpine = [] := by
    rw [hrec.char.1]; exact varSeq_applySubst_nil θ t₂ ht₂vars
  have hfc : sFieldCount l ((ofSpine t₁).applySubst θ).toSpine = sFieldCount l t₁ := by
    have e1 := rowEquiv_fieldCount_eq l hrec
    have e2 : sFieldCount l ((ofSpine t₂).applySubst θ).toSpine = sFieldCount l t₂ := by
      rw [sFieldCount_applySubst_varFree θ l ht₂vf, ofSpine_toSpine]
    have c1 := removeField_sFieldCount l s₁ hr₁
    have c2 := removeField_sFieldCount l s₂ hr₂
    omega
  have hvarfree := allVars_varFree_of t₁ hv₁nil
  have hlfree := allVars_lfree_of l t₁ hfc
  have hvars₁ : sVarSeq t₁ = sVarSeq s₁ := removeField_sVarSeq l s₁ hr₁
  have hs₁equiv : RowEquiv ((ofSpine s₁).applySubst θ)
      (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) :=
    removeField_equiv_of l s₁ hr₁
      (fun β hβ => ⟨hvarfree β (by rw [hvars₁]; exact hβ),
                    hlfree β (by rw [hvars₁]; exact hβ)⟩)
  exact hs₁equiv.trans ((RowEquiv.cat (.sing heq) hrec).trans hs₂equiv.symm)

-- U-ground FORWARD: a unifier of the original ground-match pins the field types
-- and unifies the residual. The var-free + l-free side conditions on s₁ (which
-- the backward lemma reads off the residual) are here DERIVED from hu itself:
-- the ground side s₂ fixes count_l and the var sequence under θ, hu transports
-- both to s₁, and hcount forces θ to add no l-fields across s₁'s vars.
-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem groundMatch_reflect_fwd {B : Type} {θ : TySubst B}
    {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨hs₂vars, l, hcount, _, hr₁, hr₂⟩ := groundMatch_inv hg
  have hs₂equiv : RowEquiv ((ofSpine s₂).applySubst θ)
      (.cat (.sing l (τ'.applySubst θ)) ((ofSpine t₂).applySubst θ)) :=
    removeField_equiv_of l s₂ hr₂ (fun β hβ => by simp [hs₂vars] at hβ)
  have hs₂vf : (ofSpine s₂).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact hs₂vars)
  have hv₁nil : sVarSeq ((ofSpine s₁).applySubst θ).toSpine = [] := by
    rw [hu.char.1]; exact varSeq_applySubst_nil θ s₂ hs₂vars
  have hfc : sFieldCount l ((ofSpine s₁).applySubst θ).toSpine = sFieldCount l s₁ := by
    have e1 := rowEquiv_fieldCount_eq l hu
    have e2 : sFieldCount l ((ofSpine s₂).applySubst θ).toSpine = sFieldCount l s₂ := by
      rw [sFieldCount_applySubst_varFree θ l hs₂vf, ofSpine_toSpine]
    omega
  have hvarfree := allVars_varFree_of s₁ hv₁nil
  have hlfree := allVars_lfree_of l s₁ hfc
  have hs₁equiv : RowEquiv ((ofSpine s₁).applySubst θ)
      (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) :=
    removeField_equiv_of l s₁ hr₁ (fun β hβ => ⟨hvarfree β hβ, hlfree β hβ⟩)
  exact (hs₁equiv.symm.trans (hu.trans hs₂equiv)).field_cancel_left

-- ## Assembly: success ⟹ unifies (induction on unifySpineF's fuel)
-- Base cases: one side empty ⟹ allVarsEmpty forces the other's vars to ε.
-- ⊢  unifySpineF fuel [] s₂ = success σ eqs,  θ ⊨ σ
--        ⟹   θ(ofSpine []) ≈ᵣ θ(ofSpine s₂)
theorem unifySpineF_nil_left {B : Type} {θ : TySubst B} (fuel : Nat) (s₂ : List (Atom B))
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel [] s₂ = .success σ eqs) (hσ : SolSat θ σ) :
    RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst]
      exact (allVarsEmpty_sound s₂ hae hσ).symm

-- ⊢  unifySpineF fuel (a::s₁) [] = success σ eqs,  θ ⊨ σ
--        ⟹   θ(ofSpine (a::s₁)) ≈ᵣ θ(ofSpine [])
theorem unifySpineF_cons_nil {B : Type} {θ : TySubst B} (fuel : Nat)
    (a : Atom B) (s₁ : List (Atom B))
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel (a :: s₁) [] = .success σ eqs) (hσ : SolSat θ σ) :
    RowEquiv ((ofSpine (a :: s₁)).applySubst θ) ((ofSpine ([] : List (Atom B))).applySubst θ) := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, -⟩ := h
      simp only [ofSpine, Row.applySubst]
      exact allVarsEmpty_sound (a :: s₁) hae hσ

-- ⊢  unifySpineF fuel s₁ s₂ = success σ eqs,  θ ⊨ σ,  θ ⊨ eqs
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem unifySpineF_success_sound {B : Type} {θ : TySubst B} (fuel : Nat) :
    ∀ (s₁ s₂ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)},
      unifySpineF fuel s₁ s₂ = .success σ eqs → SolSat θ σ → EqsSat θ eqs →
      RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  induction fuel with
  | zero =>
      intro s₁ s₂ σ eqs h hσ _
      cases s₁ with
      | nil => exact unifySpineF_nil_left 0 s₂ h hσ
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil 0 a s₁ h hσ
        | cons b s₂ => simp [unifySpineF] at h
  | succ fuel ih =>
      intro s₁ s₂ σ eqs h hσ heqs
      cases s₁ with
      | nil => exact unifySpineF_nil_left (fuel + 1) s₂ h hσ
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil (fuel + 1) a s₁ h hσ
        | cons b s₂ =>
          unfold unifySpineF at h
          cases hsl : stripL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
            exact stripL_reflect hsl (ih t₁ t₂ h hσ heqs)
          | none =>
          cases hsr : stripR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
            exact stripR_reflect hsr (ih t₁ t₂ h hσ heqs)
          | none =>
          cases hv1 : solveVar (a :: s₁) (b :: s₂) with
          | some r =>
            simp only [hsl, hsr, hv1] at h
            exact solveVar_reflect (hv1.trans (congrArg some h)) hσ
          | none =>
          cases hv2 : solveVar (b :: s₂) (a :: s₁) with
          | some r =>
            simp only [hsl, hsr, hv1, hv2] at h
            exact (solveVar_reflect (hv2.trans (congrArg some h)) hσ).symm
          | none =>
          cases hml : matchL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact matchL_reflect hml (heqs (τ0, τ0') (by simp))
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp])))
          | none =>
          cases hml2 : matchL (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact (matchL_reflect hml2 (heqs (τ0, τ0') (by simp)).symm
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp]))).symm).symm
          | none =>
          cases hmr : matchR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact matchR_reflect hmr (heqs (τ0, τ0') (by simp))
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp])))
          | none =>
          cases hmr2 : matchR (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact (matchR_reflect hmr2 (heqs (τ0, τ0') (by simp)).symm
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp]))).symm).symm
          | none =>
          cases hg : groundMatch (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact groundMatch_reflect hg (heqs (τ0, τ0') (by simp))
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp])))
          | none =>
          cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            exact (groundMatch_reflect hg2 (heqs (τ0, τ0') (by simp)).symm
              (ih t₁ t₂ hre hσ (fun p hp => heqs p (by simp [hp]))).symm).symm
          | none =>
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            split at h <;> simp at h

-- The ≐ᵣ success case is SOUND: any θ that meets the emitted row-var solution σ
-- and residual type equations eqs unifies the two rows.
-- ⊢  unifyRow ρ₁ ρ₂ = success σ eqs,  θ ⊨ σ,  θ ⊨ eqs   ⟹   θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRow_success_sound {B : Type} {θ : TySubst B} {ρ₁ ρ₂ : Row B}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifyRow ρ₁ ρ₂ = .success σ eqs) (hσ : SolSat θ σ) (heqs : EqsSat θ eqs) :
    Unifies θ ρ₁ ρ₂ := by
  unfold unifyRow unifySpine at h
  have key := unifySpineF_success_sound _ ρ₁.toSpine ρ₂.toSpine h hσ heqs
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.trans (key.trans e₂.symm)

------------------------- ≐ᵣ CLASH SOUNDNESS (algorithm level) --------------
-- Lifting the two local clash cores through the whole control flow, using the
-- FORWARD reflection: at every move a unifier of the original also unifies the
-- residual, so no-unifier propagates backwards. solveVar never yields clash, and
-- addEq preserves clash, so a clash comes only from a base case (allVarsEmpty) or
-- the final projClash — both already refuted locally. This is the ≐ᵣ CLASH leg of
-- the trichotomy, now at the algorithm level (not just the local conditions).

-- solveVar answers success or occurs, never clash.
theorem solveVar_ne_clash {B : Type} {s₁ s₂ : List (Atom B)} :
    solveVar s₁ s₂ ≠ some .clash := by
  intro h
  cases s₁ with
  | nil => simp [solveVar] at h
  | cons a r =>
    cases a with
    | field _ _ => simp [solveVar] at h
    | var α =>
      cases r with
      | cons _ _ => simp [solveVar] at h
      | nil => simp only [solveVar] at h; split at h <;> simp at h

-- addEq only rewrites a success; a clash result must come from a clash residual.
theorem addEq_clash_inv {B : Type} {τ τ' : Ty B} {u : URes B} :
    u.addEq τ τ' = .clash → u = .clash := by
  cases u <;> simp [URes.addEq]

-- ⊢  unifySpineF fuel s₁ s₂ = clash   ⟹   ¬ ∃ θ. θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
theorem unifySpineF_clash_no_unifier {B : Type} :
    ∀ (fuel : Nat) (s₁ s₂ : List (Atom B)),
      unifySpineF fuel s₁ s₂ = .clash →
      ¬ ∃ θ : TySubst B, Unifies θ (ofSpine s₁) (ofSpine s₂) := by
  intro fuel
  induction fuel with
  | zero =>
      intro s₁ s₂ h
      cases s₁ with
      | nil =>
          simp only [unifySpineF] at h
          cases hae : allVarsEmpty s₂ with
          | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
          | some => simp [hae] at h
      | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
              | some => simp [hae] at h
          | cons b s₂ => simp [unifySpineF] at h
  | succ fuel ih =>
      intro s₁ s₂ h
      cases s₁ with
      | nil =>
          simp only [unifySpineF] at h
          cases hae : allVarsEmpty s₂ with
          | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier hae ⟨θ, hu⟩
          | some => simp [hae] at h
      | cons a s₁ =>
          cases s₂ with
          | nil =>
              simp only [unifySpineF] at h
              cases hae : allVarsEmpty (a :: s₁) with
              | none => rintro ⟨θ, hu⟩; exact allVarsEmpty_none_no_unifier' hae ⟨θ, hu⟩
              | some => simp [hae] at h
          | cons b s₂ =>
              rintro ⟨θ, hu⟩
              unfold unifySpineF at h
              cases hsl : stripL (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
                exact ih t₁ t₂ h ⟨θ, stripL_reflect_fwd hsl hu⟩
              | none =>
              cases hsr : stripR (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
                exact ih t₁ t₂ h ⟨θ, stripR_reflect_fwd hsr hu⟩
              | none =>
              cases hv1 : solveVar (a :: s₁) (b :: s₂) with
              | some r =>
                simp only [hsl, hsr, hv1] at h; exact solveVar_ne_clash (hv1.trans (congrArg some h))
              | none =>
              cases hv2 : solveVar (b :: s₂) (a :: s₁) with
              | some r =>
                simp only [hsl, hsr, hv1, hv2] at h
                exact solveVar_ne_clash (hv2.trans (congrArg some h))
              | none =>
              cases hml : matchL (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchL_reflect_fwd hml hu).2⟩
              | none =>
              cases hml2 : matchL (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchL_reflect_fwd hml2 hu.symm).2.symm⟩
              | none =>
              cases hmr : matchR (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchR_reflect_fwd hmr hu).2⟩
              | none =>
              cases hmr2 : matchR (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (matchR_reflect_fwd hmr2 hu.symm).2.symm⟩
              | none =>
              cases hg : groundMatch (a :: s₁) (b :: s₂) with
              | some p =>
                obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (groundMatch_reflect_fwd hg hu).2⟩
              | none =>
              cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
              | some p =>
                obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
                exact ih t₁ t₂ (addEq_clash_inv h) ⟨θ, (groundMatch_reflect_fwd hg2 hu.symm).2.symm⟩
              | none =>
                simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
                split at h
                · rename_i hpc; exact projClash_no_unifier hpc ⟨θ, hu⟩
                · simp at h

-- ≐ᵣ CLASH is SOUND: a clash verdict means the two rows have no unifier.
-- ⊢  unifyRow ρ₁ ρ₂ = clash   ⟹   ¬ ∃ θ. θ ⊨ ρ₁ ≐ᵣ ρ₂
theorem unifyRow_clash_no_unifier {B : Type} {ρ₁ ρ₂ : Row B}
    (h : unifyRow ρ₁ ρ₂ = .clash) : ¬ ∃ θ : TySubst B, Unifies θ ρ₁ ρ₂ := by
  rintro ⟨θ, hu⟩
  unfold unifyRow unifySpine at h
  refine unifySpineF_clash_no_unifier _ ρ₁.toSpine ρ₂.toSpine h ⟨θ, ?_⟩
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact e₁.symm.trans (hu.trans e₂)

------------------------- ≐ᵣ SUCCESS COMPLETENESS (mgu) ---------------------
-- The emitted σ (row-var bindings) and eqs (deferred type equations) are
-- NECESSARY: EVERY unifier satisfies them. With unifyRow_success_sound (they are
-- SUFFICIENT) this makes unifyRow's output characterize the unifier set exactly —
-- i.e. ≐ᵣ computes a most general unifier, presented as row bindings + residual
-- type equations. The FORWARD reflection layer is the engine: it pushes any
-- unifier through each move to the residual and reads off the emitted type eq.

-- solveVar's success binds α ≔ ofSpine s₂; a unifier of the two rows is exactly
-- a θ meeting that binding (θα ≈ (ofSpine s₂)θ).
theorem solveVar_complete {B : Type} {θ : TySubst B} {s₁ s₂ : List (Atom B)}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (hsolve : solveVar s₁ s₂ = some (.success σ eqs))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  cases s₁ with
  | nil => simp [solveVar] at hsolve
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [solveVar] at hsolve
    | var α =>
      cases r₁ with
      | cons _ _ => simp [solveVar] at hsolve
      | nil =>
        simp only [solveVar] at hsolve
        split at hsolve
        · simp at hsolve
        · simp only [Option.some.injEq, URes.success.injEq] at hsolve
          obtain ⟨rfl, rfl⟩ := hsolve
          refine ⟨fun p hp => ?_, fun p hp => by simp at hp⟩
          simp only [List.mem_singleton] at hp
          subst hp
          simp only [ofSpine, Row.applySubst] at hu
          exact RowEquiv.unitR.symm.trans hu

theorem unifySpineF_nil_left_complete {B : Type} {θ : TySubst B} (fuel : Nat)
    (s₂ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel [] s₂ = .success σ eqs)
    (hu : RowEquiv ((ofSpine ([] : List (Atom B))).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty s₂ with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact ⟨allVarsEmpty_complete s₂ hae hu.symm, fun p hp => by simp at hp⟩

theorem unifySpineF_cons_nil_complete {B : Type} {θ : TySubst B} (fuel : Nat)
    (a : Atom B) (s₁ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifySpineF fuel (a :: s₁) [] = .success σ eqs)
    (hu : RowEquiv ((ofSpine (a :: s₁)).applySubst θ)
                   ((ofSpine ([] : List (Atom B))).applySubst θ)) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  simp only [unifySpineF] at h
  cases hae : allVarsEmpty (a :: s₁) with
  | none => simp [hae] at h
  | some σ' =>
      simp only [hae, URes.success.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      simp only [ofSpine, Row.applySubst] at hu
      exact ⟨allVarsEmpty_complete (a :: s₁) hae hu, fun p hp => by simp at hp⟩

-- ⊢  unifySpineF fuel s₁ s₂ = success σ eqs,  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   SolSat θ σ  ∧  EqsSat θ eqs
theorem unifySpineF_success_complete {B : Type} {θ : TySubst B} (fuel : Nat) :
    ∀ (s₁ s₂ : List (Atom B)) {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)},
      unifySpineF fuel s₁ s₂ = .success σ eqs →
      RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) →
      SolSat θ σ ∧ EqsSat θ eqs := by
  induction fuel with
  | zero =>
      intro s₁ s₂ σ eqs h hu
      cases s₁ with
      | nil => exact unifySpineF_nil_left_complete 0 s₂ h hu
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil_complete 0 a s₁ h hu
        | cons b s₂ => simp [unifySpineF] at h
  | succ fuel ih =>
      intro s₁ s₂ σ eqs h hu
      cases s₁ with
      | nil => exact unifySpineF_nil_left_complete (fuel + 1) s₂ h hu
      | cons a s₁ =>
        cases s₂ with
        | nil => exact unifySpineF_cons_nil_complete (fuel + 1) a s₁ h hu
        | cons b s₂ =>
          unfold unifySpineF at h
          cases hsl : stripL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
            exact ih t₁ t₂ h (stripL_reflect_fwd hsl hu)
          | none =>
          cases hsr : stripR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
            exact ih t₁ t₂ h (stripR_reflect_fwd hsr hu)
          | none =>
          cases hv1 : solveVar (a :: s₁) (b :: s₂) with
          | some r =>
            simp only [hsl, hsr, hv1] at h
            exact solveVar_complete (hv1.trans (congrArg some h)) hu
          | none =>
          cases hv2 : solveVar (b :: s₂) (a :: s₁) with
          | some r =>
            simp only [hsl, hsr, hv1, hv2] at h
            exact solveVar_complete (hv2.trans (congrArg some h)) hu.symm
          | none =>
          cases hml : matchL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml hu
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru
            exact ⟨hsol, EqsSat.cons hty heqs⟩
          | none =>
          cases hml2 : matchL (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchL_reflect_fwd hml2 hu.symm
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru.symm
            exact ⟨hsol, EqsSat.cons hty.symm heqs⟩
          | none =>
          cases hmr : matchR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr hu
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru
            exact ⟨hsol, EqsSat.cons hty heqs⟩
          | none =>
          cases hmr2 : matchR (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := matchR_reflect_fwd hmr2 hu.symm
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru.symm
            exact ⟨hsol, EqsSat.cons hty.symm heqs⟩
          | none =>
          cases hg : groundMatch (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg hu
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru
            exact ⟨hsol, EqsSat.cons hty heqs⟩
          | none =>
          cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            obtain ⟨eqs', hre, rfl⟩ := URes.addEq_success h
            obtain ⟨hty, hru⟩ := groundMatch_reflect_fwd hg2 hu.symm
            obtain ⟨hsol, heqs⟩ := ih t₁ t₂ hre hru.symm
            exact ⟨hsol, EqsSat.cons hty.symm heqs⟩
          | none =>
            simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
            split at h <;> simp at h

-- ≐ᵣ SUCCESS COMPLETENESS: any unifier of ρ₁,ρ₂ satisfies the emitted σ and eqs.
-- Together with unifyRow_success_sound: {unifiers of ρ₁ ≐ᵣ ρ₂} = {θ : SolSat θ σ ∧
-- EqsSat θ eqs} — the algorithm's output is a most general unifier.
-- ⊢  unifyRow ρ₁ ρ₂ = success σ eqs,  θ ⊨ ρ₁ ≐ᵣ ρ₂   ⟹   SolSat θ σ ∧ EqsSat θ eqs
theorem unifyRow_success_complete {B : Type} {θ : TySubst B} {ρ₁ ρ₂ : Row B}
    {σ : List (TyVar × Row B)} {eqs : List (Ty B × Ty B)}
    (h : unifyRow ρ₁ ρ₂ = .success σ eqs) (hu : Unifies θ ρ₁ ρ₂) :
    SolSat θ σ ∧ EqsSat θ eqs := by
  unfold unifyRow unifySpine at h
  unfold Unifies at hu
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact unifySpineF_success_complete _ ρ₁.toSpine ρ₂.toSpine h (e₁.symm.trans (hu.trans e₂))

------------------------- ≐ᵣ FUEL SUFFICIENCY -------------------------------
-- Every recursive move of unifySpineF removes exactly ONE atom from each side
-- (a shared end-var for strip; a matched field + its window/counterpart for
-- match/ground), so the total spine length drops by 2 per step. Hence the
-- starting fuel |s₁| + |s₂| never runs out: unifySpineF is INVARIANT to fuel
-- above that threshold, and the fuel-0 `.stuck` branch is unreachable for
-- unifySpine. A `.stuck` result is therefore a genuine ambiguity (Wand class),
-- not an out-of-fuel artifact — the precondition that makes the trichotomy's
-- stuck/occurs legs well-posed.

-- Each move's length bookkeeping: |t| + 1 = |s| for the field extractors, and
-- |t₁| + |t₂| + 2 = |s₁| + |s₂| for every two-sided move.
theorem windowExtract_len {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') → s'.length + 1 = s.length
  | [], _, _, h => by simp [windowExtract] at h
  | .var _ :: _, _, _, h => by simp [windowExtract] at h
  | .field l' _ :: s, _, _, h => by
      simp only [windowExtract] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := windowExtract_len l s hwe
          simp only [List.length_cons]; omega
        · simp at h

theorem removeField_len {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    removeField l s = some (τ, s') → s'.length + 1 = s.length
  | [], _, _, h => by simp [removeField] at h
  | .var _ :: s, _, _, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have ih := removeField_len l s hwe
        simp only [List.length_cons]; omega
      · simp at h
  | .field l' _ :: s, _, _, h => by
      simp only [removeField] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := removeField_len l s hwe
          simp only [List.length_cons]; omega
        · simp at h

theorem stripL_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripL s₁ s₂ = some (t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv h; simp only [List.length_cons]; omega

theorem stripR_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (h : stripR s₁ s₂ = some (t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv h
  simp only [List.length_append, List.length_cons, List.length_nil]; omega

theorem matchL_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (h : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨l, rfl, hwe⟩ := matchL_inv h
  have := windowExtract_len l s₂ hwe
  simp only [List.length_cons]; omega

theorem matchR_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (h : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  unfold matchR at h
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at h; simp at h
  | some p =>
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    rw [hml] at h
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl, rfl, rfl⟩ := h
    have := matchL_len hml
    simp only [List.length_reverse] at this ⊢
    omega

theorem groundMatch_len {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (h : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    t₁.length + t₂.length + 2 = s₁.length + s₂.length := by
  obtain ⟨_, l, _, _, hr₁, hr₂⟩ := groundMatch_inv h
  have := removeField_len l s₁ hr₁
  have := removeField_len l s₂ hr₂
  omega

-- Fuel invariance: any two fuels ≥ |s₁|+|s₂| give the same result. Induction on
-- a length bound N; each recursive arm drops the bound by 2 (the *_len lemmas)
-- and applies the IH. The control-flow cascade mirrors unifySpineF_success_sound.
theorem unifySpineF_fuel_irrel {B : Type} (N : Nat) :
    ∀ (s₁ s₂ : List (Atom B)) (fuel fuel' : Nat),
      s₁.length + s₂.length ≤ N →
      s₁.length + s₂.length ≤ fuel → s₁.length + s₂.length ≤ fuel' →
      unifySpineF fuel s₁ s₂ = unifySpineF fuel' s₁ s₂ := by
  induction N with
  | zero =>
      intro s₁ s₂ fuel fuel' hN _ _
      cases s₁ with
      | nil => simp only [unifySpineF]
      | cons a s₁ =>
        cases s₂ with
        | nil => simp only [unifySpineF]
        | cons b s₂ => simp only [List.length_cons] at hN; omega
  | succ N IH =>
      intro s₁ s₂ fuel fuel' hN hf hf'
      cases s₁ with
      | nil => simp only [unifySpineF]
      | cons a s₁ =>
        cases s₂ with
        | nil => simp only [unifySpineF]
        | cons b s₂ =>
          have hpos : 2 ≤ (a :: s₁).length + (b :: s₂).length := by
            simp only [List.length_cons]; omega
          obtain ⟨f, rfl⟩ := Nat.exists_eq_succ_of_ne_zero (show fuel ≠ 0 by omega)
          obtain ⟨f', rfl⟩ := Nat.exists_eq_succ_of_ne_zero (show fuel' ≠ 0 by omega)
          simp only [unifySpineF]
          cases hsl : stripL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p
            have hlen := stripL_len hsl
            exact IH t₁ t₂ f f' (by omega) (by omega) (by omega)
          | none =>
          cases hsr : stripR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨t₁, t₂⟩ := p
            have hlen := stripR_len hsr
            exact IH t₁ t₂ f f' (by omega) (by omega) (by omega)
          | none =>
          cases hv1 : solveVar (a :: s₁) (b :: s₂) with
          | some r => rfl
          | none =>
          cases hv2 : solveVar (b :: s₂) (a :: s₁) with
          | some r => rfl
          | none =>
          cases hml : matchL (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; dsimp only
            have hlen := matchL_len hml
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hml2 : matchL (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; dsimp only
            have hlen := matchL_len hml2
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hmr : matchR (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; dsimp only
            have hlen := matchR_len hmr
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hmr2 : matchR (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; dsimp only
            have hlen := matchR_len hmr2
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hg : groundMatch (a :: s₁) (b :: s₂) with
          | some p =>
            obtain ⟨τ0, τ0', t₁, t₂⟩ := p; dsimp only
            have hlen := groundMatch_len hg
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none =>
          cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
          | some p =>
            obtain ⟨τ0', τ0, t₂, t₁⟩ := p; dsimp only
            have hlen := groundMatch_len hg2
            rw [IH t₁ t₂ f f' (by omega) (by omega) (by omega)]
          | none => rfl

-- unifySpine's own fuel (|s₁|+|s₂|) is enough: any larger fuel agrees with it.
theorem unifySpineF_fuel_stable {B : Type} (s₁ s₂ : List (Atom B)) {fuel : Nat}
    (h : s₁.length + s₂.length ≤ fuel) :
    unifySpineF fuel s₁ s₂ = unifySpine s₁ s₂ :=
  unifySpineF_fuel_irrel (s₁.length + s₂.length) s₁ s₂ fuel _ (Nat.le_refl _) h (Nat.le_refl _)

------------------------------------ NEXT ------------------------------------
-- Milestones that build on this file (algorithmic.typ, Open questions):
--  * STRICTNESS of the QTyped extension: prove ¬Typed for the two-use
--    program at its precise type (lifts no_plain_principal_scheme through
--    let-inversion) — makes "L2 is strictly more precise" a theorem.
--  * Type safety for QTyped itself (progress/preservation) — the L2 system
--    is the real declarative system of the thesis; minimal.lean's proofs
--    are the template, discharge determinism/monotonicity the new inputs.
--  * ≐ᵣ SUCCESS SOUNDNESS — DONE. unifyRow_success_sound: if unifyRow ρ₁ ρ₂ =
--    success σ eqs and θ meets σ (SolSat) and eqs (EqsSat), then Unifies θ ρ₁ ρ₂.
--    Axiom-clean (propext / Quot.sound, no sorry). Assembled from:
--      - move-reflection lemmas ("θ unifies the residual ⟹ θ unified the
--        original"): stripL/stripR_reflect, solveVar_reflect, matchL_reflect
--        (via windowExtract_equiv), matchR_reflect (via revRow /
--        windowExtract_reverse_equiv), groundMatch_reflect, allVarsEmpty_sound;
--      - the U-ground core: field_comm_lfree (a field ≈-commutes past a
--        var-free, l-free row) + removeField_equiv_of, with the COUNTING
--        (allVars_varFree_of / allVars_lfree_of) discharging the "skipped vars
--        are l-free under θ" side condition from hrec + the ground side being
--        var-free — the one genuinely non-local step, now closed;
--      - fuel induction (unifySpineF_success_sound) discharging each match arm.
--    FUEL SUFFICIENCY — DONE (section "≐ᵣ FUEL SUFFICIENCY"):
--    unifySpineF_fuel_irrel / unifySpineF_fuel_stable — each move eats exactly 2
--    atoms (the *_len lemmas), so |s₁|+|s₂| fuel never runs out and a .stuck
--    result is genuine, not out-of-fuel.
--    OCCURS — CHARACTERIZED: occurs is CONSERVATIVE. occurs_allVar_unifiable
--    shows unifyRow α (β|α|γ) = occurs is UNIFIABLE (β,γ ↦ ε), so the check is
--    incomplete; the genuine no-unifier case needs a field —
--    occurs_field_no_unifier (α ∈ vars s₂ ∧ 0 < count_l s₂ ⟹ no unifier), the
--    occurs analogue of projClash_no_unifier. Both after projClash_no_unifier.
--    CLASH — ALGORITHM-LEVEL, DONE: unifyRow_clash_no_unifier (clash ⟹ no
--    unifier), via unifySpineF_clash_no_unifier (fuel induction) using the two
--    local halves (projClash_no_unifier interior + allVarsEmpty_none_no_unifier
--    base) plus the FORWARD-REFLECTION layer + solveVar_ne_clash/addEq_clash_inv.
--    FORWARD REFLECTION — DONE (completeness direction of every move):
--    strip/match/groundMatch_reflect_fwd via field_cancel_left/right — a unifier
--    of the original unifies the residual (+ emitted type eq).
--    MGU-ON-SUCCESS — DONE: unifyRow_success_complete — every unifier satisfies
--    the emitted σ and eqs, so with unifyRow_success_sound the unifier set is
--    EXACTLY {θ : SolSat θ σ ∧ EqsSat θ eqs}: ≐ᵣ computes a most general unifier.
--    Via unifySpineF_success_complete (fuel induction, forward) + solveVar_complete
--    + allVarsEmpty_complete (RowEquiv.cat_empty_split) + EqsSat.cons.
--    STUCK ⟹ NO-MGU — both canonical base shapes DONE: (1) instanceOf_fieldCount_mono
--    (an mgu is pointwise count-minimal, as subst never deletes a field) + the general
--    no_mgu_of_witness_shrinks; wand_no_mgu_count re-proves Wand (field-vs-vars) via
--    counting. (2) instanceOf_fieldCount_eq_of_varFree (a var-free component of an mgu is
--    RIGID) + two_sided_no_mgu for (α|l:𝓫)≐ᵣ(l:𝓫|β) — counting can't shrink the ε,ε
--    unifier there, so rigidity does the kill. LIFT INFRA: HasMgu + hasMgu_congr/
--    hasMgu_rowEquiv (no-mgu depends only on the unifier SET, is a ≈-invariant) +
--    stripL/stripR_hasMgu_iff (strip moves preserve the unifier set) discharge the STRIP
--    arms of the fuel induction (demo: wand_under_strip_no_mgu). Remaining for the full
--    unifySpineF=stuck⟹no-mgu: (a) the general BASE arm (any immediately-stuck config ⟹
--    no-mgu — only wand/two_sided shapes so far; may be conservative, unchecked); (b) the
--    MATCH/GROUND arms (emit a type eq ⟹ unifier sets differ ⟹ hasMgu_congr doesn't apply;
--    need the augmented-witness argument: satisfy the emitted eq via the witnesses' type
--    parts, then re-run count-shrink/rigidity at the original level).
--    NOTE occurs does NOT lift to a no-unifier theorem — it is conservative
--    (occurs_allVar_unifiable); only occurs_field_no_unifier genuine.
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





