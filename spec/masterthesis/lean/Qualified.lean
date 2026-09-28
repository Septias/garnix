-- Qualified schemes: stumps, discharge, discharge metatheory, the
-- principal qualified scheme of λx.x.l, and the QTyped relation over QCtx.
-- Independent of the row-unification algorithm (imports only `minimal`).

import minimal
import LabelLookup

namespace MinimalCalculus

----------------------------------- STUMPS -----------------------------------
-- A stump ⟨ρ.l ↓ δ⟩ is a parked selection: the lookup of l in ρ blocked on a
-- row-variable, and δ is the *result* standing for whatever the lookup will
-- turn out to be. Algorithmically δ keeps the selection's result position
-- writable; declaratively a stump is a constraint on the scheme. The result is
-- a TYPE, not a variable: a promise unification has already spent (δ ≔ α → β,
-- `spentEx_*`) is still a stump ⟨ρ.l ↓ α → β⟩. A-sel-? parks `.var δ`.

-- The label is a QUERY TYPE (LabelLookup.lean): ⌊l⌋ for a static selection
-- `e.l`, a label variable for a dynamic `e₁.(e₂)` whose label is not known yet.
-- A label-variable query is a second source of `?`, and the stump then waits
-- for that variable exactly as it waits for a row variable.
-- a stump's result is a type, and parked stumps are told apart by it
deriving instance DecidableEq for Ty, Row

structure Stump (B : Type) where
  row   : Row B
  label : Ty B
  res   : Ty B

deriving instance DecidableEq for Stump

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


--------------------------- PUSHING σ UNDER A SCHEME ---------------------------
-- Substituting under `σ.vars` WITHOUT renaming them. This is capture-avoiding
-- exactly when θ cannot be captured by those binders, which is what
-- `QScheme.Avoiding` below says and `QCovers.forward_of_avoiding` (QSubst.lean)
-- cashes in. `A-var`'s `FreshRenaming` is the discipline that arranges it.

def QScheme.applySubst {B : Type} (σ : QScheme B) (θ : TySubst B) : QScheme B :=
  ⟨σ.vars,
   σ.constraints.map (fun st => (⟨st.row.applySubst θ, st.label.applySubst θ, st.res⟩ : Stump B)),
   σ.body.applySubst θ⟩

theorem QScheme.applySubst_vars {B : Type} (σ : QScheme B) (θ : TySubst B) :
    (σ.applySubst θ).vars = σ.vars := rfl

/-- σ's result variables are among its binders — the well-formedness the header
states ("δ are drawn from vars like every other quantified variable"), and what
makes a stump a constraint on the scheme's OWN binder rather than on a free
variable. -/
def QScheme.WF {B : Type} (σ : QScheme B) : Prop :=
  ∀ st ∈ σ.constraints, ∃ δ ∈ σ.vars, st.res = .var δ

/-- what σ mentions at a non-binding position. -/
def QScheme.freeFtv {B : Type} (σ : QScheme B) : List TyVar :=
  σ.constraints.flatMap (fun st => st.row.ftv ++ st.label.ftv) ++ σ.body.ftv

/-- θ cannot be captured by σ's binders: it fixes them, and its image on σ's
other variables never mentions one. Exactly the side condition that makes
`QScheme.applySubst` — which does NOT rename — legitimate. -/
def QScheme.Avoiding {B : Type} (σ : QScheme B) (θ : TySubst B) : Prop :=
  (∀ α ∈ σ.vars, θ.ty α = .var α ∧ θ.row α = .var α) ∧
  (∀ α ∈ σ.freeFtv, α ∉ σ.vars →
     (∀ β ∈ (θ.ty α).ftv, β ∉ σ.vars) ∧ (∀ β ∈ (θ.row α).ftv, β ∉ σ.vars))

---------------------------------- DISCHARGE ----------------------------------
-- Γ ⊢ (θρ).l ↓ r  replayed per instantiation θ:
--
--   D-hit    r = τ_r  ⟹  θδ = τ_r    (the T-sel moment)
--   D-⊥      r = ⊥    ⟹  θδ = ★      (T-sel-⊥; W-flag on the algo side)
--   D-?      r = ?    ⟹  θδ = ★      (T-sel-★: still-unknown stays blurred;
--                                     algorithmically this case re-parks
--                                     instead — only finalization commits ★)

inductive Stump.Discharge {B : Type} (θ : TySubst B) (s : Stump B) :
    Prop where
  | hit {τ : Ty B} :
      LookupQ (s.row.applySubst θ) (s.label.applySubst θ) (.found τ) →
      s.res.applySubst θ = τ → Discharge θ s
  | abs :
      LookupQ (s.row.applySubst θ) (s.label.applySubst θ) .absent →
      s.res.applySubst θ = .unk → Discharge θ s
  | unk :
      LookupQ (s.row.applySubst θ) (s.label.applySubst θ) .unknown →
      s.res.applySubst θ = .unk → Discharge θ s

-- Instantiation-with-discharge  σ ≥ τ
--
-- NO LONGER Γ-RELATIVE. `algorithmic.typ` wrote this `σ ≥_Γ τ` and called the
-- Γ-dependence "the price of cross-instantiation refinement", because
-- discharge had to read Γ's row-solutions. It does not: discharge substitutes
-- the row with θ and then looks up, and ↓ reads nothing else. So ≥ is again
-- the Γ-independent relation of the minimal calculus, and the refinement that
-- Γ was carrying is carried by θ — which is where the algorithm keeps it
-- anyway.

def QScheme.Inst {B : Type} (σ : QScheme B) (τ : Ty B) : Prop :=
  ∃ θ : TySubst B, θ.FixedOutside σ.vars ∧
    (∀ s ∈ σ.constraints, s.Discharge θ) ∧
    σ.body.applySubst θ = τ

-- Plain schemes embed: with Q = ∅ the discharge condition is vacuous and
-- ≥ degenerates to the plain Scheme.Inst. This is the seam between
-- the two sysstems (decl. & alg.) — everything minimal.lean knows about plain
-- schemes lifts across this equivalence.
-- ⊢  σ.toQ ≥ τ   ↔   σ ≥ τ
theorem QScheme.inst_toQ {B : Type} {σ : Scheme B} {τ : Ty B} :
    QScheme.Inst σ.toQ τ ↔ σ.Inst τ := by
  constructor
  · rintro ⟨θ, hfix, -, hbody⟩
    exact ⟨θ, hfix, hbody⟩
  · rintro ⟨θ, hfix, hbody⟩
    exact ⟨θ, hfix, fun s hs => absurd hs List.not_mem_nil, hbody⟩

-- Monotype qualified schemes instantiate only to themselves.
-- ⊢  ⟨[], [], τ₁⟩ ≥ τ   ⟹   τ = τ₁
theorem QScheme.Inst.mono {B : Type} {τ₁ τ : Ty B}
    (h : QScheme.Inst ⟨[], [], τ₁⟩ τ) : τ = τ₁ :=
  Scheme.Inst.mono ((QScheme.inst_toQ (σ := ⟨[], τ₁⟩)).mp h)


---------------------------- DISCHARGE METATHEORY -----------------------------

-- Determinism: two discharges of the same stump that substitute the row the
-- same way pin the result variable to the same type (lookup_det lifted).
-- This is what makes ≥ a well-defined relation per θ rather than a choice.
-- ⊢  discharge s @θ₁,  discharge s @θ₂,  θ₁·s.row = θ₂·s.row
--        ⟹   θ₁·s.res = θ₂·s.res
theorem Stump.Discharge.det {B : Type} {θ₁ θ₂ : TySubst B}
    {s : Stump B} (h₁ : s.Discharge θ₁) (h₂ : s.Discharge θ₂)
    (hrow : s.row.applySubst θ₁ = s.row.applySubst θ₂)
    (hlab : s.label.applySubst θ₁ = s.label.applySubst θ₂) :
    s.res.applySubst θ₁ = s.res.applySubst θ₂ := by
  cases h₁ with
  | hit hl₁ hδ₁ =>
      rw [hrow, hlab] at hl₁
      cases h₂ with
      | hit hl₂ hδ₂ => cases hl₁.det hl₂; rw [hδ₁, hδ₂]
      | abs hl₂ _   => cases hl₁.det hl₂
      | unk hl₂ _   => cases hl₁.det hl₂
  | abs hl₁ hδ₁ =>
      rw [hrow, hlab] at hl₁
      cases h₂ with
      | hit hl₂ _   => cases hl₁.det hl₂
      | abs _ hδ₂   => rw [hδ₁, hδ₂]
      | unk _ hδ₂   => rw [hδ₁, hδ₂]
  | unk hl₁ hδ₁ =>
      rw [hrow, hlab] at hl₁
      cases h₂ with
      | hit hl₂ _   => cases hl₁.det hl₂
      | abs _ hδ₂   => rw [hδ₁, hδ₂]
      | unk _ hδ₂   => rw [hδ₁, hδ₂]

-- Definite-stability: a discharge whose lookup came out definite (τ/⊥)
-- survives any FURTHER SUBSTITUTION χ, with the pinned result carried along.
-- This is the algorithmic "a resolved stump NEVER needs re-checking" — wake-up
-- lists never contain resolved stumps, no fixpoint iteration. The ?-case is
-- deliberately NOT stable: wake-up exists precisely to improve it.
--
-- Was stated over row-extensions of Γ (`Ctx.RowExt` + `LookupQ.mono`); refining
-- the solution IS composing with χ, so this is the same theorem with the
-- refinement where the algorithm actually keeps it. `LookupQ.applySubst` is the
-- content, exactly as `LookupQ.mono` was before.
-- ⊢  discharge s @θ definite   ⟹   discharge s @(χ∘θ)
theorem Stump.Discharge.applySubst_of_definite {B : Type}
    {θ χ : TySubst B} {s : Stump B}
    (h : s.Discharge θ)
    (hdef : ¬ LookupQ (s.row.applySubst θ) (s.label.applySubst θ) .unknown) :
    s.Discharge (χ.comp θ) := by
  cases h with
  | @hit τ hl hδ =>
      refine .hit (τ := τ.applySubst χ) ?_ ?_
      · rw [← Row.applySubst_applySubst, ← Ty.applySubst_applySubst]
        exact LookupQ.applySubst χ hl (by intro hc; cases hc)
      · rw [← Ty.applySubst_applySubst, hδ]
  | abs hl hδ =>
      refine .abs ?_ ?_
      · rw [← Row.applySubst_applySubst, ← Ty.applySubst_applySubst]
        exact LookupQ.applySubst (r := .absent) χ hl (by intro hc; cases hc)
      · rw [← Ty.applySubst_applySubst, hδ]; rfl
  | unk hl _ => exact absurd hl hdef

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
   [⟨.var "β", .lab "l", .var "δ"⟩],
   .fn (.rcd (.var "β")) (.var "δ")⟩

-- The worked-example table of algorithmic.typ in one statement: ANY lookup
-- verdict on the argument row yields the corresponding instance —
--   Γ ⊢ ρ.l ↓ found τ_r  ⟹  {ρ} → τ_r      (what L1 loses)
--   Γ ⊢ ρ.l ↓ ⊥          ⟹  {ρ} → ★
--   Γ ⊢ ρ.l ↓ ?          ⟹  {ρ} → ★
-- ⊢ Γ ⊢ ρ.l ↓ r          ⟹  selQ ≥ ({ρ} → collapse r)
theorem selQ_inst_of_lookup {B : Type} {ρ : Row B}
    {r : LookupRes B} (h : Lookup ρ "l" r) :
    QScheme.Inst (selQ B) (.fn (.rcd ρ) r.collapse) := by
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
    | found τ => exact .hit (by simpa [Row.applySubst, Ty.applySubst] using h)
                            (by simp [LookupRes.collapse, Ty.applySubst])
    | absent  => exact .abs (by simpa [Row.applySubst, Ty.applySubst] using h)
                            (by simp [LookupRes.collapse, Ty.applySubst])
    | unknown => exact .unk (by simpa [Row.applySubst, Ty.applySubst] using h)
                            (by simp [LookupRes.collapse, Ty.applySubst])
  · simp [selQ, Ty.applySubst, Row.applySubst]

-- The found-typing family is covered (τ₀ arbitrary — the typings L1's frozen
-- ★ could never reach, cf. finalized_no_blur):
-- ⊢  selQ ≥ ({l: τ₀} → τ₀)      (every τ₀)
theorem selQ_inst_found {B : Type} (τ₀ : Ty B) :
    QScheme.Inst (selQ B) (.fn (.rcd (.sing "l" τ₀)) τ₀) :=
  selQ_inst_of_lookup (r := .found τ₀) .hit

-- ... and so is the ⊥-typing:
-- ⊢  selQ ≥ ({} → ★)
theorem selQ_inst_absent {B : Type} :
    QScheme.Inst (selQ B) (.fn (.rcd .empty) .unk) :=
  selQ_inst_of_lookup (r := .absent) .emp

-- The mixed instance {ε} → {ε} — the one every plain scheme was forced to
-- admit (no_plain_principal_scheme's contradiction) — is NOT an instance:
-- θ must send β to ε, the lookup on ε is definitely ⊥, and discharge then
-- pins δ at ★, never at {ε}. *Discharge is exactly the mechanism that plugs the instance-closedness leak.*
-- ⊢  ¬ ( selQ ≥_∅ ({} → {}) )
theorem selQ_no_mixed {B : Type} :
    ¬ QScheme.Inst (selQ B)
        (.fn (.rcd .empty) (.rcd (.empty : Row B))) := by
  rintro ⟨θ, -, hQ, hbody⟩
  simp only [selQ, Ty.applySubst, Row.applySubst] at hbody
  injection hbody with hdom hres
  injection hdom with hβ
  have hs := hQ ⟨.var "β", .lab "l", .var "δ"⟩ (by simp [selQ])
  cases hs with
  | hit hl _ =>
      simp only [Row.applySubst, Ty.applySubst, LookupQ.lab_iff] at hl
      rw [hβ] at hl
      cases hl
  | abs _ hδ => rw [show θ.ty "δ" = _ from hδ] at hres; cases hres
  | unk _ hδ => rw [show θ.ty "δ" = _ from hδ] at hres; cases hres

-- Instance-closedness: EVERY ≥-instance of selQ is a declarative typing of
-- λx. x.l — in any context Γ. The three discharge cases replay exactly
-- T-sel / T-sel-⊥ / T-sel-★; the Lean regression proof of minimal.lean was
-- already this case split, per instance.
-- ⊢  ∀ τ. selQ ≥ τ   ⟹   Γ ⊢ (λx. x.l) : τ
theorem selQ_instance_closed {B C : Type} (constTy : C → B) (Γ : Ctx B) :
    ∀ τ, QScheme.Inst (selQ B) τ → Typed constTy Γ (selEx C) τ := by
  rintro τ ⟨θ, -, hQ, hbody⟩
  simp only [selQ, Ty.applySubst, Row.applySubst] at hbody
  subst hbody
  have hs := hQ ⟨.var "β", .lab "l", .var "δ"⟩ (by simp [selQ])
  -- the λ-bound variable types at its annotation …
  have hvar : Typed constTy (Γ.bindTy "x" (.rcd (θ.row "β")))
      (.var "x" : Expr C) (.rcd (θ.row "β")) :=
    .tVar (by simp [Ctx.lookup_bindTy]) (Scheme.Inst.refl _)
  -- … and the discharge's lookup is already the lookup T-sel wants: it reads
  -- the row alone, so there is nothing to transport under the binder.
  cases hs with
  | hit hl hδ =>
      simp only [Row.applySubst, Ty.applySubst, LookupQ.lab_iff] at hl
      rw [show θ.ty "δ" = _ from hδ]
      exact .tLam (.tSel hvar hl)
  | abs hl hδ =>
      simp only [Row.applySubst, Ty.applySubst, LookupQ.lab_iff] at hl
      rw [show θ.ty "δ" = _ from hδ]
      exact .tLam (.tSelAbs hvar hl)
  | unk hl hδ =>
      simp only [Row.applySubst, Ty.applySubst, LookupQ.lab_iff] at hl
      rw [show θ.ty "δ" = _ from hδ]
      exact .tLam (.tSelUnk hvar hl)

-- The bookend to no_plain_principal_scheme: a QUALIFIED scheme CAN be
-- simultaneously instance-closed and cover both typings, so qualified schemes
-- are the necessary form of let-generalization for a calculus with
-- lookup-stumps.
-- ⊢  ∃ σ.  (∀ τ. σ ≥_∅ τ ⟹ ∅ ⊢ λx.x.l : τ)
--              ∧ σ ≥_∅ ({l: {}} → {}) ∧ σ ≥_∅ ({} → ★)
theorem qualified_principal_scheme {B C : Type} (constTy : C → B) :
    ∃ σ : QScheme B,
      (∀ τ, QScheme.Inst σ τ →
        Typed constTy Ctx.empty (selEx C) τ) ∧
      QScheme.Inst σ
        (.fn (.rcd (.sing "l" (.rcd .empty))) (.rcd .empty)) ∧
      QScheme.Inst σ (.fn (.rcd .empty) .unk) :=
  ⟨selQ B, selQ_instance_closed constTy Ctx.empty,
   selQ_inst_found (.rcd .empty), selQ_inst_absent⟩


--============================ THE COVERING ORDER ==============================--
-- σ ⊴ σ' — "σ' is at least as general as σ" [glossar: ⊴]. A scheme says exactly
-- what its instances say, so generality is instance containment. Two wrinkles
-- are specific to THIS calculus and both are forced, not chosen:
--
--  (1) MOBILITY UNDER SUBSTITUTION, not Γ-relativity. This used to read "≥_Γ
--      reads Γ's row-solutions through discharge", and the order came in two
--      flavors, ⊴[Γ] (containment at one context) and ⊴ (containment in every
--      context). With ≥ Γ-free there is ONE order. The phenomenon that forced
--      two survives and is stated where it belongs: the instance set of a
--      stump-carrying scheme MOVES when the solution is refined, and refining
--      the solution is applying a substitution — see
--      covered_not_applySubst_stable. What a let-bound scheme sees between its
--      generalization and its uses is a θ, not a bigger Γ.
--
--  (2) BLUR. The typing relation is closed under T-★-intro (qUnk); an instance
--      set is not, because ★ is rigid (finalized_no_blur). λx.x.l types at
--      {(l: 𝓫)} → ★, and that type is NOT an instance of selQ: discharge pins
--      the result at 𝓫 (selQ_no_blurred_inst). So plain containment can never
--      state principality here — no scheme's instances exhaust a blur-closed
--      typing set — and the order that can is ⊴⊑, containment UP TO PRECISION:
--      σ' must offer, for each instance of σ, an instance at least as precise.
--
-- NB: `Covers θ σ σ'` in minimal.lean is a different relation — transport of
-- instances along one substitution, used by the substitution lemma. This is the
-- generality preorder on schemes and quantifies over instances, not over θ.

namespace QScheme

-- Inst(σ) ⊆ Inst(σ')
def Covered {B : Type} (σ σ' : QScheme B) : Prop :=
  ∀ τ, QScheme.Inst σ τ → QScheme.Inst σ' τ

-- ... up to precision: σ' answers each instance of σ with one at least as
-- precise. This is the order principality is stated in.
def PrecCovered {B : Type} (σ σ' : QScheme B) : Prop :=
  ∀ τ, QScheme.Inst σ τ → ∃ τ', QScheme.Inst σ' τ' ∧ TyPrec τ' τ

end QScheme

infix:50 " ⊴ "  => QScheme.Covered
infix:50 " ⊴⊑ " => QScheme.PrecCovered

-- Both are preorders, and neither is antisymmetric: α-renamed binders give
-- distinct schemes with equal instance sets, so ⊴ ∩ ⊵ is the equivalence the
-- order is really about.
theorem QScheme.Covered.refl {B : Type} (σ : QScheme B) : σ ⊴ σ :=
  fun _ h => h

theorem QScheme.Covered.trans {B : Type} {σ₁ σ₂ σ₃ : QScheme B}
    (h₁ : σ₁ ⊴ σ₂) (h₂ : σ₂ ⊴ σ₃) : σ₁ ⊴ σ₃ :=
  fun τ hτ => h₂ τ (h₁ τ hτ)

theorem QScheme.PrecCovered.refl {B : Type} (σ : QScheme B) :
    σ ⊴⊑ σ := fun τ hτ => ⟨τ, hτ, .refl τ⟩

-- the only place ⊑-transitivity is consumed
theorem QScheme.PrecCovered.trans {B : Type} {σ₁ σ₂ σ₃ : QScheme B}
    (h₁ : σ₁ ⊴⊑ σ₂) (h₂ : σ₂ ⊴⊑ σ₃) : σ₁ ⊴⊑ σ₃ := by
  intro τ hτ
  obtain ⟨τ', hτ', hp'⟩ := h₁ τ hτ
  obtain ⟨τ'', hτ'', hp''⟩ := h₂ τ' hτ'
  exact ⟨τ'', hτ'', hp''.trans hp'⟩

-- ⊴ refines ⊴⊑ (precision is reflexive), so every result about ⊴ transports.
theorem QScheme.Covered.toPrec {B : Type} {σ σ' : QScheme B}
    (h : σ ⊴ σ') : σ ⊴⊑ σ' := fun τ hτ => ⟨τ, h τ hτ, .refl τ⟩

-- PLAIN SCHEMES: ⊴ is exactly L1 instance containment. The seam of
-- QScheme.inst_toQ, one level up.
-- ⊢  σ.toQ ⊴ σ'.toQ  ↔  ∀ τ. σ ≥ τ → σ' ≥ τ
theorem QScheme.covered_toQ {B : Type} {σ σ' : Scheme B} :
    σ.toQ ⊴ σ'.toQ ↔ ∀ τ, σ.Inst τ → σ'.Inst τ := by
  constructor
  · intro h τ hτ
    exact QScheme.inst_toQ.mp (h τ (QScheme.inst_toQ.mpr hτ))
  · intro h τ hτ
    exact QScheme.inst_toQ.mpr (h τ (QScheme.inst_toQ.mp hτ))

-- A monotype sits below σ' exactly when σ' instantiates to it — the base case
-- every generalization argument bottoms out in.
-- ⊢  ⟨[], [], τ₁⟩ ⊴ σ'  ↔  σ' ≥ τ₁
theorem QScheme.covered_mono {B : Type} {τ₁ : Ty B}
    {σ' : QScheme B} :
    (⟨[], [], τ₁⟩ : QScheme B) ⊴ σ' ↔ QScheme.Inst σ' τ₁ := by
  constructor
  · intro h
    exact h τ₁ (QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], τ₁⟩))
  · intro h τ hτ
    rw [QScheme.Inst.mono hτ]
    exact h

-- ⊴ IS BLIND TO VACUITY: a scheme with no Γ-instance sits below everything.
-- This is qLet's inhabitation premise seen from the order side — generality
-- alone cannot rule out a scheme that promises nothing, which is why
-- Principal (below) carries inhabitation as a separate conjunct.
-- ⊢  (¬ ∃ τ. σ ≥ τ)  ⟹  σ ⊴ σ'     (for EVERY σ')
theorem QScheme.coveredAt_of_uninhabited {B : Type}
    {σ σ' : QScheme B} (h : ¬ ∃ τ, QScheme.Inst σ τ) : σ ⊴ σ' :=
  fun τ hτ => absurd ⟨τ, hτ⟩ h

-- WHY ⊴ IS NOT ENOUGH (1): the blurred type {(l: 𝓫)} → ★ is NOT an instance of
-- selQ. θ must send β to (l: 𝓫), the lookup on that row is definitely found 𝓫,
-- and discharge then pins δ at 𝓫 — never at ★. (It IS a typing of λx.x.l:
-- selEx_blurred_typing, at the end of this file.)
-- ⊢  ¬ ( selQ ≥_∅ ({(l: 𝓫)} → ★) )
theorem selQ_no_blurred_inst {B : Type} (b : B) :
    ¬ QScheme.Inst (selQ B)
        (.fn (.rcd (.sing "l" (.base b))) .unk) := by
  rintro ⟨θ, -, hdis, hbody⟩
  simp only [selQ, Ty.applySubst, Row.applySubst] at hbody
  injection hbody with h1 h2
  injection h1 with hβ
  have hrow : (Row.var "β").applySubst θ = Row.sing "l" (.base b) := hβ
  have hs := hdis ⟨.var "β", .lab "l", .var "δ"⟩ (by simp [selQ])
  cases hs with
  | hit hl hres =>
      rw [hrow] at hl
      cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit)
      change θ.ty "δ" = _ at hres; rw [h2] at hres
      cases hres
  | abs hl _ =>
      rw [hrow] at hl
      cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit)
  | unk hl _ =>
      rw [hrow] at hl
      cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit)

-- ... and (2): under ⊴⊑ the same scheme DOES answer it, with the sharper
-- found-instance {(l: 𝓫)} → 𝓫 ⊑ {(l: 𝓫)} → ★. Blur is absorbed by the order
-- instead of by the scheme.
-- ⊢  ∃ τ'. selQ ≥_∅ τ' ∧ τ' ⊑ₜ ({(l: 𝓫)} → ★)
theorem selQ_prec_answers_blur {B : Type} (b : B) :
    ∃ τ', QScheme.Inst (selQ B) τ' ∧
      TyPrec τ' (.fn (.rcd (.sing "l" (.base b))) .unk) :=
  ⟨_, selQ_inst_found (.base b), .fn (.refl _) (.unk _)⟩


--------------------------- THE L2 TYPING RELATION ----------------------------
-- The qualified declarative system [algorithmic.typ, L2]: contexts bind
-- QSchemes, T-var instantiates via ≥ (instantiation-with-discharge), and
-- T-let's instance-closed premise quantifies over DISCHARGED instances only.
-- Everything else mirrors minimal.lean's Typed verbatim.
--
-- QTyped EXTENDS Typed (Typed.toQ below), strictly: the two-use program at the
-- bottom types one let-binding at BOTH the found- and the ⊥-instance — a
-- combination no plain scheme admits (l1_rejects_two_use; the
-- strictness of the inclusion is l1_strictly_weaker).

structure QCtx (B : Type) where
  tyEnv  : List (Var × QScheme B)

namespace QCtx

def lookup (Γ : QCtx B) (x : Var) : Option (QScheme B) :=
  (Γ.tyEnv.find? (·.1 == x)).map (·.2)

def bindScheme (Γ : QCtx B) (x : Var) (σ : QScheme B) : QCtx B :=
  { Γ with tyEnv := (x, σ) :: Γ.tyEnv }

def bindTy (Γ : QCtx B) (x : Var) (τ : Ty B) : QCtx B :=
  Γ.bindScheme x ⟨[], [], τ⟩

def empty : QCtx B := ⟨[]⟩

-- ⊢  (Γ, x:σ).lookup y  =  if x = y then some σ else Γ.lookup y
theorem lookup_bindScheme (Γ : QCtx B) (x y : Var) (σ : QScheme B) :
    (Γ.bindScheme x σ).lookup y = if x == y then some σ else Γ.lookup y := by
  simp only [QCtx.lookup, QCtx.bindScheme, List.find?_cons]
  cases hxy : (x == y) <;> simp_all

end QCtx

--------------------------- WHAT A QCtx MENTIONS -------------------------------
-- Γ-freshness ("the instantiation lands on names Γ has not committed to") was
-- not expressible because there was no ftv of a `QCtx`. These supply it. Each is
-- an OVER-approximation — binders are counted alongside free variables — which
-- is the safe direction for an avoid-set: freshness against the larger list
-- implies freshness against the free variables. L1's `Ctx.schemeFtv`
-- (minimal.lean) over-approximates the same way, for the same reason.

/-- every variable a stump mentions: its row, and its result variable δ. -/
def Stump.ftv {B : Type} (s : Stump B) : List TyVar := s.res.ftv ++ (s.row.ftv ++ s.label.ftv)

/-- every variable a qualified scheme mentions, binders included. -/
def QScheme.ftv {B : Type} (σ : QScheme B) : List TyVar :=
  σ.vars ++ σ.constraints.flatMap Stump.ftv ++ σ.body.ftv

/-- every variable Γ mentions: its schemes' variables, binders included. -/
def QCtx.ftv {B : Type} (Γ : QCtx B) : List TyVar :=
  Γ.tyEnv.flatMap (fun p => p.2.ftv)

-- ⊢ a scheme Γ knows about is covered by Γ.ftv
theorem QCtx.lookup_ftv_subset {B : Type} {Γ : QCtx B} {x : Var} {σ : QScheme B}
    (h : Γ.lookup x = some σ) : ∀ α ∈ σ.ftv, α ∈ Γ.ftv := by
  intro α hα
  unfold QCtx.lookup at h
  cases hf : Γ.tyEnv.find? (·.1 == x) with
  | none => rw [hf] at h; cases h
  | some p =>
      rw [hf] at h
      simp only [Option.map_some, Option.some.injEq] at h
      subst h
      exact List.mem_flatMap.mpr ⟨p, List.mem_of_find?_eq_some hf, hα⟩

-- ⊢ …and so is every variable of its BODY, which is the form A-var consumes
theorem QCtx.lookup_body_ftv_subset {B : Type} {Γ : QCtx B} {x : Var}
    {σ : QScheme B} (h : Γ.lookup x = some σ) : ∀ α ∈ σ.body.ftv, α ∈ Γ.ftv :=
  fun α hα => QCtx.lookup_ftv_subset h α
    (List.mem_append_right _ hα)

-- ⊢ binding only ever ADDS variables
theorem QCtx.ftv_bindScheme {B : Type} (Γ : QCtx B) (x : Var) (σ : QScheme B) :
    ∀ α ∈ Γ.ftv, α ∈ (Γ.bindScheme x σ).ftv := by
  intro α hα
  simp only [QCtx.ftv, QCtx.bindScheme, List.flatMap_cons, List.mem_append] at hα ⊢
  exact .inr hα

mutual
  inductive QTyped {B C : Type} (constTy : C → B) :
      QCtx B → Expr C → Ty B → Prop where
    | qCon : QTyped constTy Γ (.con c) (.base (constTy c))
    -- x : σ ∈ Γ   σ ≥ τ           (instantiation-with-discharge)
    | qVar : Γ.lookup x = some σ → QScheme.Inst σ τ →
             QTyped constTy Γ (.var x) τ
    | qEq  : QTyped constTy Γ e τ₁ → TyEquiv τ₁ τ₂ → QTyped constTy Γ e τ₂
    | qLam : QTyped constTy (Γ.bindTy x τ₁) e τ₂ →
             QTyped constTy Γ (.lam x e) (.fn τ₁ τ₂)
    | qApp : QTyped constTy Γ e₁ (.fn τ₁ τ₂) → QTyped constTy Γ e₂ τ₁ →
             QTyped constTy Γ (.app e₁ e₂) τ₂
    -- ∀ τ₁ ≥ σ.  Γ ⊢ e₁ : τ₁     (instance-closed over DISCHARGED instances)
    -- The INHABITATION premise is not bureaucracy: with Q ≠ ∅ a scheme can have
    -- NO Γ-instance, and then the instance-closed premise says nothing at all
    -- about e₁ — `let x = (3 4) in 5` would type while being stuck, and progress
    -- would be false. Plain schemes satisfy it by Scheme.Inst.self; the solver
    -- satisfies it by construction (a parked stump discharges at ★ if nothing
    -- better), so this is the declarative shadow of "stumps always finalize".
    | qLet : σ.WF →
             (∀ τ₁, QScheme.Inst σ τ₁ → QTyped constTy Γ e₁ τ₁) →
             (∃ τ₁, QScheme.Inst σ τ₁) →
             QTyped constTy (Γ.bindScheme x σ) e₂ τ₂ →
             QTyped constTy Γ (.letE x e₁ e₂) τ₂
    | qCat : QTyped constTy Γ e₁ (.rcd ρ₁) → QTyped constTy Γ e₂ (.rcd ρ₂) →
             QTyped constTy Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))
    | qSel : QTyped constTy Γ e (.rcd ρ) → Lookup ρ l (.found τ) →
             QTyped constTy Γ (.sel e l) τ
    | qSelUnk : QTyped constTy Γ e (.rcd ρ) → Lookup ρ l .unknown →
                QTyped constTy Γ (.sel e l) .unk
    | qSelAbs : QTyped constTy Γ e (.rcd ρ) → Lookup ρ l .absent →
                QTyped constTy Γ (.sel e l) .unk
    | qUnk : QTyped constTy Γ e τ → QTyped constTy Γ e .unk
    | qRcd : QTypedBody constTy Γ b ρ → QTyped constTy Γ (.rcd b) (.rcd ρ)
    -- FC-labels. A label literal types at its singleton; a dynamic selection
    -- looks up whatever its key's TYPE says (`LookupQ`: ⌊l⌋, a label variable,
    -- or no label at all), with the three verdicts of T-sel / T-sel-★ / T-sel-⊥
    | qLab : QTyped constTy Γ (.lab l) (.lab l)
    | qSelDyn : QTyped constTy Γ e₁ (.rcd ρ) → QTyped constTy Γ e₂ q →
                LookupQ ρ q (.found τ) → QTyped constTy Γ (.selDyn e₁ e₂) τ
    | qSelDynUnk : QTyped constTy Γ e₁ (.rcd ρ) → QTyped constTy Γ e₂ q →
                   LookupQ ρ q .unknown → QTyped constTy Γ (.selDyn e₁ e₂) .unk
    | qSelDynAbs : QTyped constTy Γ e₁ (.rcd ρ) → QTyped constTy Γ e₂ q →
                   LookupQ ρ q .absent → QTyped constTy Γ (.selDyn e₁ e₂) .unk

  inductive QTypedBody {B C : Type} (constTy : C → B) :
      QCtx B → RecBody (Expr C) → Row B → Prop where
    | empty : QTypedBody constTy Γ .empty .empty
    | field : QTyped constTy Γ e τ →
              QTypedBody constTy Γ (.field l e) (.sing l τ)
    | cat : QTypedBody constTy Γ b₁ ρ₁ → QTypedBody constTy Γ b₂ ρ₂ →
            QTypedBody constTy Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)
end

--------------------------- L2 TYPING INVERSION (mod ≈) ------------------------
-- Verbatim the minimal.lean story one level up: qEq peels ≈ₜ layers (collected
-- by transitivity), qUnk adds a `∨ τ = ★` escape hatch kept inside the
-- existentials. The recursion runs over QTyped indices only — the qRcd case
-- returns its QTypedBody witness without recursing into it — so this is a plain
-- structural recursion over one half of the mutual pair, exactly as
-- typed_inv_aux is over Typed.
private theorem qtyped_inv_aux {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    (∀ {c : C}, e = .con c →
      TyEquiv (.base (constTy c)) τ ∨ τ = .unk) ∧
    (∀ {x : Var} {e' : Expr C}, e = .lam x e' →
      ∃ τ₁ τ₂, (TyEquiv (.fn τ₁ τ₂) τ ∨ τ = .unk) ∧
        QTyped constTy (Γ.bindTy x τ₁) e' τ₂) ∧
    (∀ {b : RecBody (Expr C)}, e = .rcd b →
      ∃ ρ, (TyEquiv (.rcd ρ) τ ∨ τ = .unk) ∧ QTypedBody constTy Γ b ρ)
  | _, _, _, .qCon =>
      ⟨(fun h => by cases h; exact .inl (.refl _)),
       (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qVar _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qEq h heq =>
      have ih := qtyped_inv_aux h
      ⟨(fun hc => match ih.1 hc with
         | .inl he => .inl (he.trans heq)
         | .inr hu => .inr (hu ▸ heq).unk_inv),
       (fun hl => match ih.2.1 hl with
         | ⟨_, _, .inl he, hb⟩ => ⟨_, _, .inl (he.trans heq), hb⟩
         | ⟨_, _, .inr hu, hb⟩ => ⟨_, _, .inr (hu ▸ heq).unk_inv, hb⟩),
       (fun hr => match ih.2.2 hr with
         | ⟨_, .inl he, hb⟩ => ⟨_, .inl (he.trans heq), hb⟩
         | ⟨_, .inr hu, hb⟩ => ⟨_, .inr (hu ▸ heq).unk_inv, hb⟩)⟩
  | _, _, _, .qUnk h =>
      have ih := qtyped_inv_aux h
      ⟨(fun _ => .inr rfl),
       (fun hl => match ih.2.1 hl with
         | ⟨_, _, _, hb⟩ => ⟨_, _, .inr rfl, hb⟩),
       (fun hr => match ih.2.2 hr with
         | ⟨_, _, hb⟩ => ⟨_, .inr rfl, hb⟩)⟩
  | _, _, _, .qLam h =>
      ⟨(fun hc => nomatch hc),
       (fun hl => by cases hl; exact ⟨_, _, .inl (.refl _), h⟩),
       (fun hr => nomatch hr)⟩
  | _, _, _, .qApp _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qCat _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qSel _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qSelUnk _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qSelAbs _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qLet _ _ _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qLab =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qSelDyn _ _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qSelDynUnk _ _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qSelDynAbs _ _ _ =>
      ⟨(fun h => nomatch h), (fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, _, .qRcd h =>
      ⟨(fun hc => nomatch hc), (fun hl => nomatch hl),
       (fun hr => by cases hr; exact ⟨_, .inl (.refl _), h⟩)⟩

theorem qtyped_con_inv {B C : Type} {constTy : C → B} {Γ : QCtx B} {c : C}
    {τ : Ty B}
    (h : QTyped constTy Γ (.con c) τ) :
    TyEquiv (.base (constTy c)) τ ∨ τ = .unk :=
  (qtyped_inv_aux h).1 rfl

theorem qtyped_lam_inv {B C : Type} {constTy : C → B} {Γ : QCtx B} {x : Var}
    {e : Expr C} {τ : Ty B}
    (h : QTyped constTy Γ (.lam x e) τ) :
    ∃ τ₁ τ₂, (TyEquiv (.fn τ₁ τ₂) τ ∨ τ = .unk) ∧
      QTyped constTy (Γ.bindTy x τ₁) e τ₂ :=
  (qtyped_inv_aux h).2.1 rfl

theorem qtyped_rcd_inv {B C : Type} {constTy : C → B} {Γ : QCtx B}
    {b : RecBody (Expr C)} {τ : Ty B}
    (h : QTyped constTy Γ (.rcd b) τ) :
    ∃ ρ, (TyEquiv (.rcd ρ) τ ∨ τ = .unk) ∧ QTypedBody constTy Γ b ρ :=
  (qtyped_inv_aux h).2.2 rfl

-- a label literal types at its singleton, up to ≈ and blur
private theorem qtyped_lab_inv_aux {B C : Type} {constTy : C → B} {l : Label} :
    ∀ {Γ : QCtx B} {e : Expr C} {τ : Ty B}, QTyped constTy Γ e τ → e = .lab l →
      TyEquiv (.lab l) τ ∨ τ = .unk
  | _, _, _, .qLab, he => by cases he; exact .inl (.refl _)
  | _, _, _, .qEq h heq, he =>
      match qtyped_lab_inv_aux h he with
      | .inl h' => .inl (h'.trans heq)
      | .inr hu => by subst hu; exact .inr (TyEquiv.unk_inv heq)
  | _, _, _, .qUnk _, _ => .inr rfl
  | _, _, _, .qCon, he => nomatch he
  | _, _, _, .qVar _ _, he => nomatch he
  | _, _, _, .qLam _, he => nomatch he
  | _, _, _, .qApp _ _, he => nomatch he
  | _, _, _, .qLet _ _ _ _, he => nomatch he
  | _, _, _, .qCat _ _, he => nomatch he
  | _, _, _, .qSel _ _, he => nomatch he
  | _, _, _, .qSelUnk _ _, he => nomatch he
  | _, _, _, .qSelAbs _ _, he => nomatch he
  | _, _, _, .qRcd _, he => nomatch he
  | _, _, _, .qSelDyn _ _ _, he => nomatch he
  | _, _, _, .qSelDynUnk _ _ _, he => nomatch he
  | _, _, _, .qSelDynAbs _ _ _, he => nomatch he

theorem qtyped_lab_inv {B C : Type} {constTy : C → B} {Γ : QCtx B} {l : Label}
    {τ : Ty B} (h : QTyped constTy Γ (.lab l : Expr C) τ) :
    TyEquiv (.lab l) τ ∨ τ = .unk :=
  qtyped_lab_inv_aux h rfl

------------------------------ L2 CANONICAL FORMS ------------------------------
-- A value's shape is fixed by the head of its (L2) type, tEq/tUnk notwithstanding.
-- Value/RowEquiv are reused verbatim from minimal — only the typing relation
-- changed, so these track canonical_fn/canonical_rcd rule-for-rule.

theorem qcanonical_fn {B C : Type} {constTy : C → B} {Γ : QCtx B} {v : Expr C}
    {τ₁ τ₂ : Ty B}
    (hv : Value v) (ht : QTyped constTy Γ v (.fn τ₁ τ₂)) :
    ∃ x e, v = .lam x e := by
  cases hv with
  | con =>
      rcases qtyped_con_inv ht with he | hu
      · cases he.base_inv
      · cases hu
  | lam => exact ⟨_, _, rfl⟩
  | rcd =>
      obtain ⟨ρ, he | hu, -⟩ := qtyped_rcd_inv ht
      · obtain ⟨ρ', hσ, -⟩ := he.rcd_inv
        cases hσ
      · cases hu
  | lab =>
      rcases qtyped_lab_inv ht with he | hu
      · cases he.lab_inv
      · cases hu

theorem qcanonical_rcd {B C : Type} {constTy : C → B} {Γ : QCtx B} {v : Expr C}
    {ρ : Row B}
    (hv : Value v) (ht : QTyped constTy Γ v (.rcd ρ)) :
    ∃ b ρ', v = .rcd b ∧ RowEquiv ρ' ρ ∧ QTypedBody constTy Γ b ρ' := by
  cases hv with
  | con =>
      rcases qtyped_con_inv ht with he | hu
      · cases he.base_inv
      · cases hu
  | lam =>
      obtain ⟨τ₁, τ₂, he | hu, -⟩ := qtyped_lam_inv ht
      · obtain ⟨σ₁, σ₂, hσ, -⟩ := he.fn_inv
        cases hσ
      · cases hu
  | rcd =>
      obtain ⟨ρ', he | hu, hb⟩ := qtyped_rcd_inv ht
      · obtain ⟨ρ'', hσ, heq⟩ := he.rcd_inv
        cases hσ
        exact ⟨_, _, rfl, heq, hb⟩
      · cases hu
  | lab =>
      rcases qtyped_lab_inv ht with he | hu
      · cases he.lab_inv
      · cases hu

-- Embedding: every declarative typing is an L2 typing
-- Plain contexts embed by Q = ∅ everywhere; discharge is vacuous, and lookups
-- transport with no side condition at all (↓ does not read the context).

def Ctx.toQ {B : Type} (Γ : Ctx B) : QCtx B :=
  ⟨Γ.tyEnv.map (fun p => (p.1, p.2.toQ))⟩

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
      .qLet (by simp [QScheme.WF, Scheme.toQ])
            (fun τ₁ hq => Typed.toQ (hprem τ₁ (QScheme.inst_toQ.mp hq)))
            ⟨_, QScheme.inst_toQ.mpr (Scheme.Inst.self _)⟩
            (Typed.toQ hbody)
  | _, _, _, .tCat h₁ h₂ => .qCat (Typed.toQ h₁) (Typed.toQ h₂)
  | _, _, _, .tSel h hl =>
      .qSel (Typed.toQ h)
        hl
  | _, _, _, .tSelUnk h hl =>
      .qSelUnk (Typed.toQ h)
        hl
  | _, _, _, .tSelAbs h hl =>
      .qSelAbs (Typed.toQ h)
        hl
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
-- own copy of the stump. No single plain scheme can serve both uses at these
-- types — proved below (l1_rejects_two_use), which is what
-- makes the L1 ⊆ L2 inclusion STRICT.
-- ⊢  ∅ ⊢_Q  let f = (λx. x.l) in { a = f {l = c} | b = f {} }
--            :  { a: 𝓫_c | b: ★ }
theorem qtyped_two_use {B C : Type} (constTy : C → B) (c : C) :
    QTyped constTy QCtx.empty
      (.letE "f" (selEx C)
        (.rcd (.cat
          (.field "a" (.app (.var "f") (.rcd (.field "l" (.con c)))))
          (.field "b" (.app (.var "f") (.rcd .empty))))))
      (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" .unk))) := by
  refine .qLet (σ := selQ B) (by simp [QScheme.WF, selQ]) (fun τ₁ hq => ?_) ⟨_, selQ_inst_absent⟩ ?_
  · exact (selQ_instance_closed constTy Ctx.empty τ₁ hq).toQ
  · refine .qRcd (.cat (.field ?_) (.field ?_))
    · exact .qApp
        (.qVar (by simp [QCtx.lookup_bindScheme])
               (selQ_inst_found (.base (constTy c))))
        (.qRcd (.field .qCon))
    · exact .qApp
        (.qVar (by simp [QCtx.lookup_bindScheme]) selQ_inst_absent)
        (.qRcd .empty)


--======================= L2 METATHEORY: SUBSTITUTION ========================--
-- Progress and preservation for the qualified system. Step/Value/Err/Progress
-- are reused verbatim from minimal — only the typing relation changed — so every
-- lemma here tracks its minimal.lean twin rule-for-rule. The one new ingredient
-- was that qVar/qLet instantiated Γ-RELATIVELY, so weakening had to transport
-- those premises across binders. It does not any more: `QScheme.Inst` and
-- `Stump.Discharge` mention no context, so `Stump.Discharge.congr_rowEnv` and
-- `QScheme.Inst.congr_rowEnv` — the two lemmas that carried a discharge from
-- one row environment to an agreeing one — have nothing left to say and are
-- deleted. Weakening is now pure term-environment bookkeeping.

-- ## The QCtx weakening preorder  Γ₁ ⊑ Γ₂
-- Tracks Ctx.Sub: term-lookups only grow. Subsumes weakening, exchange and
-- shadowing.
def QCtx.Sub {B : Type} (Γ₁ Γ₂ : QCtx B) : Prop :=
  ∀ x σ, Γ₁.lookup x = some σ → Γ₂.lookup x = some σ

theorem QCtx.Sub.refl {B : Type} (Γ : QCtx B) : QCtx.Sub Γ Γ :=
  fun _ _ h => h

theorem QCtx.Sub.trans {B : Type} {Γ₁ Γ₂ Γ₃ : QCtx B}
    (h₁ : QCtx.Sub Γ₁ Γ₂) (h₂ : QCtx.Sub Γ₂ Γ₃) : QCtx.Sub Γ₁ Γ₃ :=
  fun x τ h => h₂ x τ (h₁ x τ h)

-- Binding respects the preorder.
theorem QCtx.Sub.bindScheme {B : Type} {Γ₁ Γ₂ : QCtx B} (h : QCtx.Sub Γ₁ Γ₂)
    (x : Var) (σ : QScheme B) :
    QCtx.Sub (Γ₁.bindScheme x σ) (Γ₂.bindScheme x σ) := by
  intro y σ' hy
  rw [QCtx.lookup_bindScheme] at hy ⊢
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact h y σ' hy
  · simpa [hxy] using hy

theorem QCtx.Sub.bindTy {B : Type} {Γ₁ Γ₂ : QCtx B} (h : QCtx.Sub Γ₁ Γ₂)
    (x : Var) (τ : Ty B) : QCtx.Sub (Γ₁.bindTy x τ) (Γ₂.bindTy x τ) :=
  h.bindScheme x ⟨[], [], τ⟩

theorem QCtx.Sub.exchange {B : Type} (Γ : QCtx B) {x y : Var} (hne : x ≠ y)
    (σ₁ σ₂ : QScheme B) :
    QCtx.Sub ((Γ.bindScheme x σ₁).bindScheme y σ₂)
             ((Γ.bindScheme y σ₂).bindScheme x σ₁) := by
  intro z μ hz
  simp only [QCtx.lookup_bindScheme] at hz ⊢
  cases hyz : (y == z) <;> cases hxz : (x == z) <;>
    simp only [hyz, hxz, Bool.false_eq_true, if_false, if_true] at hz ⊢ <;>
    try exact hz
  exact absurd ((eq_of_beq hxz).trans (eq_of_beq hyz).symm) hne

theorem QCtx.Sub.shadowed {B : Type} {Δ Γ : QCtx B} {x : Var} {σ₁ : QScheme B}
    (h : QCtx.Sub Δ (Γ.bindScheme x σ₁)) (σ : QScheme B) :
    QCtx.Sub (Δ.bindScheme x σ) (Γ.bindScheme x σ) := by
  intro z μ hz
  rw [QCtx.lookup_bindScheme] at hz ⊢
  cases hxz : (x == z)
  · simp only [hxz, Bool.false_eq_true, if_false] at hz ⊢
    have := h z μ hz
    rwa [QCtx.lookup_bindScheme, hxz, if_neg (by simp)] at this
  · simpa [hxz] using hz

-- A closed term (empty tyEnv) types in any context.
theorem QCtx.Sub.ofEmptyTyEnv {B : Type} (Γ : QCtx B) :
    QCtx.Sub QCtx.empty Γ :=
  fun x τ h => by simp [QCtx.lookup, QCtx.empty] at h

-- ## Typing transports along ⊑ (mutual over QTyped/QTypedBody)
-- Every case is now a plain constructor rebuild: the qVar/qLet Inst premises
-- and the qSel lookups mention no context, so they ride across untouched.
mutual
theorem qtyped_sub {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : QCtx B} → {e : Expr C} → {τ : Ty B} → QCtx.Sub Γ₁ Γ₂ →
    QTyped constTy Γ₁ e τ → QTyped constTy Γ₂ e τ
  | _, _, _, _, _,  .qCon         => .qCon
  | _, _, _, _, hs, .qVar h hi    => .qVar (hs _ _ h) hi
  | _, _, _, _, hs, .qEq h heq    => .qEq (qtyped_sub hs h) heq
  | _, _, _, _, hs, .qLam h       => .qLam (qtyped_sub (hs.bindTy _ _) h)
  | _, _, _, _, hs, .qApp h₁ h₂   => .qApp (qtyped_sub hs h₁) (qtyped_sub hs h₂)
  | _, _, _, _, hs, .qCat h₁ h₂   => .qCat (qtyped_sub hs h₁) (qtyped_sub hs h₂)
  | _, _, _, _, hs, .qSel h hl    =>
      .qSel (qtyped_sub hs h) hl
  | _, _, _, _, hs, .qSelUnk h hl =>
      .qSelUnk (qtyped_sub hs h) hl
  | _, _, _, _, hs, .qSelAbs h hl =>
      .qSelAbs (qtyped_sub hs h) hl
  | _, _, _, _, hs, .qUnk h       => .qUnk (qtyped_sub hs h)
  | _, _, _, _, hs, .qLet hwf h₁ hne h₂   =>
      .qLet hwf (fun τ' hi =>
              qtyped_sub hs (h₁ τ' hi))
            hne
            (qtyped_sub (hs.bindScheme _ _) h₂)
  | _, _, _, _, hs, .qRcd h       => .qRcd (qtypedBody_sub hs h)
  | _, _, _, _, _,  .qLab         => .qLab
  | _, _, _, _, hs, .qSelDyn h₁ h₂ hl =>
      .qSelDyn (qtyped_sub hs h₁) (qtyped_sub hs h₂) hl
  | _, _, _, _, hs, .qSelDynUnk h₁ h₂ hl =>
      .qSelDynUnk (qtyped_sub hs h₁) (qtyped_sub hs h₂) hl
  | _, _, _, _, hs, .qSelDynAbs h₁ h₂ hl =>
      .qSelDynAbs (qtyped_sub hs h₁) (qtyped_sub hs h₂) hl

theorem qtypedBody_sub {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} → QCtx.Sub Γ₁ Γ₂ →
    QTypedBody constTy Γ₁ b ρ → QTypedBody constTy Γ₂ b ρ
  | _, _, _, _, _,  .empty     => .empty
  | _, _, _, _, hs, .field h   => .field (qtyped_sub hs h)
  | _, _, _, _, hs, .cat h₁ h₂ =>
      .cat (qtypedBody_sub hs h₁) (qtypedBody_sub hs h₂)
end

-- ## Substitution  e[x := v]  (mutual over QTyped/QTypedBody)
-- The scheme-bound value v must be typeable at every DISCHARGED instance — the
-- premise qLet supplies at let-β. Verbatim subst_aux one level up — the qVar
-- and qLet-premise cases used to add a row-view congruence to move Inst between
-- Δ's and Γ's row environments; with Inst context-free they pass it through.
mutual
private theorem qsubst_aux {B C : Type} {constTy : C → B} :
    {Δ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Δ e τ →
    ∀ {Γ : QCtx B} {x : Var} {v : Expr C} {σ : QScheme B},
      QCtx.Sub Δ (Γ.bindScheme x σ) →
      (∀ τ', QScheme.Inst σ τ' → QTyped constTy QCtx.empty v τ') →
      QTyped constTy Γ (subst x v e) τ
  | _, _, _, .qCon, _, _, _, _, _, _ => .qCon
  | _, .var y, _, .qVar h hi, _, x, _, _, hsub, hv => by
      have hy := hsub _ _ h
      rw [QCtx.lookup_bindScheme] at hy
      simp only [subst]
      cases hxy : (x == y)
      · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
        exact .qVar hy hi
      · simp only [hxy, if_true] at hy ⊢
        cases Option.some.inj hy
        exact qtyped_sub (QCtx.Sub.ofEmptyTyEnv _) (hv _ hi)
  | _, _, _, .qEq h heq, _, _, _, _, hsub, hv =>
      .qEq (qsubst_aux h hsub hv) heq
  | _, .lam y e₀, _, .qLam h, _, x, _, _, hsub, hv => by
      simp only [subst]
      cases hxy : (x == y)
      · simp only [Bool.false_eq_true, if_false]
        exact .qLam (qsubst_aux h
          ((hsub.bindTy _ _).trans
            (QCtx.Sub.exchange _ (by simpa using hxy) _ _)) hv)
      · simp only [if_true]
        exact .qLam (qtyped_sub ((eq_of_beq hxy) ▸ hsub.shadowed _) h)
  | _, _, _, .qApp h₁ h₂, _, _, _, _, hsub, hv =>
      .qApp (qsubst_aux h₁ hsub hv) (qsubst_aux h₂ hsub hv)
  | _, _, _, .qCat h₁ h₂, _, _, _, _, hsub, hv =>
      .qCat (qsubst_aux h₁ hsub hv) (qsubst_aux h₂ hsub hv)
  | _, _, _, .qSel h hl, _, _, _, _, hsub, hv =>
      .qSel (qsubst_aux h hsub hv) hl
  | _, _, _, .qSelUnk h hl, _, _, _, _, hsub, hv =>
      .qSelUnk (qsubst_aux h hsub hv) hl
  | _, _, _, .qSelAbs h hl, _, _, _, _, hsub, hv =>
      .qSelAbs (qsubst_aux h hsub hv) hl
  | _, _, _, .qUnk h, _, _, _, _, hsub, hv =>
      .qUnk (qsubst_aux h hsub hv)
  | _, _, _, .qLab, _, _, _, _, _, _ => .qLab
  | _, _, _, .qSelDyn h₁ h₂ hl, _, _, _, _, hsub, hv =>
      .qSelDyn (qsubst_aux h₁ hsub hv) (qsubst_aux h₂ hsub hv)
        hl
  | _, _, _, .qSelDynUnk h₁ h₂ hl, _, _, _, _, hsub, hv =>
      .qSelDynUnk (qsubst_aux h₁ hsub hv) (qsubst_aux h₂ hsub hv)
        hl
  | _, _, _, .qSelDynAbs h₁ h₂ hl, _, _, _, _, hsub, hv =>
      .qSelDynAbs (qsubst_aux h₁ hsub hv) (qsubst_aux h₂ hsub hv)
        hl
  | _, .letE y e₁ e₂, _, .qLet hwf h₁ hne h₂, _, x, _, _, hsub, hv => by
      simp only [subst]
      cases hxy : (x == y)
      · simp only [Bool.false_eq_true, if_false]
        exact .qLet hwf
          (fun τ' hi =>
            qsubst_aux (h₁ τ' hi) hsub hv)
          hne
          (qsubst_aux h₂
            ((hsub.bindScheme _ _).trans
              (QCtx.Sub.exchange _ (by simpa using hxy) _ _)) hv)
      · simp only [if_true]
        exact .qLet hwf
          (fun τ' hi =>
            qsubst_aux (h₁ τ' hi) hsub hv)
          hne
          (qtyped_sub ((eq_of_beq hxy) ▸ hsub.shadowed _) h₂)
  | _, _, _, .qRcd h, _, _, _, _, hsub, hv =>
      .qRcd (qsubstBody_aux h hsub hv)

private theorem qsubstBody_aux {B C : Type} {constTy : C → B} :
    {Δ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    QTypedBody constTy Δ b ρ →
    ∀ {Γ : QCtx B} {x : Var} {v : Expr C} {σ : QScheme B},
      QCtx.Sub Δ (Γ.bindScheme x σ) →
      (∀ τ', QScheme.Inst σ τ' → QTyped constTy QCtx.empty v τ') →
      QTypedBody constTy Γ (substBody x v b) ρ
  | _, _, _, .empty, _, _, _, _, _, _ => .empty
  | _, _, _, .field h, _, _, _, _, hsub, hv => .field (qsubst_aux h hsub hv)
  | _, _, _, .cat h₁ h₂, _, _, _, _, hsub, hv =>
      .cat (qsubstBody_aux h₁ hsub hv) (qsubstBody_aux h₂ hsub hv)
end

-- Scheme-bound variables: v typeable at every discharged instance (let-β).
theorem qsubst_scheme_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : QCtx B) (x : Var) (v : Expr C) (σ : QScheme B) (τ₂ : Ty B) (e : Expr C)
    (hv : ∀ τ', QScheme.Inst σ τ' → QTyped constTy QCtx.empty v τ')
    (he : QTyped constTy (Γ.bindScheme x σ) e τ₂) :
    QTyped constTy Γ (subst x v e) τ₂ :=
  qsubst_aux he (QCtx.Sub.refl _) hv

-- Monotype-bound variables (λ): the singleton case.
theorem qsubst_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : QCtx B) (x : Var) (v : Expr C) (τ₁ τ₂ : Ty B) (e : Expr C)
    (hv : QTyped constTy QCtx.empty v τ₁)
    (he : QTyped constTy (Γ.bindTy x τ₁) e τ₂) :
    QTyped constTy Γ (subst x v e) τ₂ :=
  qsubst_scheme_preserves_typing constTy Γ x v ⟨[], [], τ₁⟩ τ₂ e
    (fun _ hi => hi.mono.symm ▸ hv) he


--======================= L2 METATHEORY: PRESERVATION ========================--
-- Term/type lookup agreement on a typed record body, one level up. Bodies have
-- spine-var-free rows, so their lookups are never ? (mirrors the minimal twins).

theorem QTypedBody.spineVarFree {B C : Type} {constTy : C → B} {Γ : QCtx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → QTypedBody constTy Γ b ρ →
    ρ.SpineVarFree
  | _, _, .empty     => .empty
  | _, _, .field _   => .sing
  | _, _, .cat h₁ h₂ => .cat (QTypedBody.spineVarFree h₁)
                             (QTypedBody.spineVarFree h₂)

theorem QTypedBody.lookup_absent {B C : Type} {constTy : C → B} {Γ : QCtx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → QTypedBody constTy Γ b ρ →
    ∀ {l : Label}, Lookup ρ l .absent → RecBody.lookup l b = none
  | _, _, .empty => fun _ => rfl
  | _, _, .field _ => fun hl => by
      cases hl with
      | miss hne => simp [RecBody.lookup, Ne.symm hne]
  | _, _, .cat h₁ h₂ => fun hl => by
      cases hl with
      | catSkip ha hr =>
          simp [RecBody.lookup, QTypedBody.lookup_absent h₁ ha,
                QTypedBody.lookup_absent h₂ hr]

theorem QTypedBody.lookup_found {B C : Type} {constTy : C → B} {Γ : QCtx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → QTypedBody constTy Γ b ρ →
    ∀ {l : Label} {τ : Ty B}, Lookup ρ l (.found τ) →
    ∃ e, RecBody.lookup l b = some e ∧ QTyped constTy Γ e τ
  | _, _, .empty => fun hl => nomatch hl
  | _, _, .field ht => fun hl => by
      cases hl
      exact ⟨_, by simp [RecBody.lookup], ht⟩
  | _, _, .cat h₁ h₂ => fun hl => by
      cases hl with
      | catHit hf =>
          obtain ⟨e, hb, hte⟩ := QTypedBody.lookup_found h₁ hf
          exact ⟨e, by simp [RecBody.lookup, hb], hte⟩
      | catSkip ha hr =>
          obtain ⟨e, hb, hte⟩ := QTypedBody.lookup_found h₂ hr
          exact ⟨e, by simp [RecBody.lookup,
                             QTypedBody.lookup_absent h₁ ha, hb], hte⟩

-- a field the body holds is typeable at some type (dynamic selection at ★ needs
-- no more than this)
theorem QTypedBody.lookup_some {B C : Type} {constTy : C → B} {Γ : QCtx B} :
    {b : RecBody (Expr C)} → {ρ : Row B} → QTypedBody constTy Γ b ρ →
    ∀ {l : Label} {e : Expr C}, RecBody.lookup l b = some e →
      ∃ τ, True ∧ QTyped constTy Γ e τ
  | _, _, .empty, _, _, h => nomatch h
  | _, _, .field (l := l') ht, l, _, h => by
      simp only [RecBody.lookup] at h
      split at h
      · cases h; exact ⟨_, trivial, ht⟩
      · cases h
  | _, _, .cat h₁ h₂, l, _, h => by
      simp only [RecBody.lookup] at h
      split at h
      · rename_i e' he; cases h; exact QTypedBody.lookup_some h₁ he
      · exact QTypedBody.lookup_some h₂ h

-- ## Preservation  (closed programs)
--   If ⊢_Q e : τ  and  e → e'  then  ⊢_Q e' : τ.
-- Stated at the empty QCtx so the substitution lemmas' closed-value premise
-- lines up (β needs a closed argument). qApp/qLet-β route through Phase 2;
-- the qSel* cases reuse minimal's lookup-across-≈ᵣ helpers unchanged.
private theorem qpreservation_aux {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    Γ = QCtx.empty → ∀ {e' : Expr C}, Step e e' → QTyped constTy Γ e' τ
  | _, _, _, .qCon,   _, _ => (nomatch ·)
  | _, _, _, .qVar _ _, _, _ => (nomatch ·)
  | _, _, _, .qLam _, _, _ => (nomatch ·)
  | _, _, _, .qRcd _, _, _ => (nomatch ·)
  | _, _, _, .qEq h heq, hΓ, _ => fun hs =>
      .qEq (qpreservation_aux h hΓ hs) heq
  | _, _, _, .qApp h₁ h₂, hΓ, _ => fun hs => by
      cases hs with
      | appFun s     => exact .qApp (qpreservation_aux h₁ hΓ s) h₂
      | appArg v s   => exact .qApp h₁ (qpreservation_aux h₂ hΓ s)
      | beta hval =>
          subst hΓ
          obtain ⟨σ₁, σ₂, heq | hu, hbody⟩ := qtyped_lam_inv h₁
          · obtain ⟨τ₁', τ₂', hfn, he₁, he₂⟩ := heq.fn_inv
            cases hfn
            exact .qEq
              (qsubst_preserves_typing _ _ _ _ _ _ _
                (.qEq h₂ he₁.symm) hbody)
              he₂
          · cases hu
  | _, _, _, .qCat h₁ h₂, hΓ, _ => fun hs => by
      cases hs with
      | catLeft s    => exact .qCat (qpreservation_aux h₁ hΓ s) h₂
      | catRight v s => exact .qCat h₁ (qpreservation_aux h₂ hΓ s)
      | catVal =>
          obtain ⟨ρ₁', he₁ | hu₁, hb₁⟩ := qtyped_rcd_inv h₁
          · obtain ⟨ρ₂', he₂ | hu₂, hb₂⟩ := qtyped_rcd_inv h₂
            · obtain ⟨_, hσ₁, hr₁⟩ := he₁.rcd_inv
              obtain ⟨_, hσ₂, hr₂⟩ := he₂.rcd_inv
              cases hσ₁; cases hσ₂
              exact .qEq (.qRcd (.cat hb₂ hb₁)) (.rcd (.cat hr₂ hr₁))
            · cases hu₂
          · cases hu₁
  | _, _, _, .qSel h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .qSel (qpreservation_aux h hΓ s) hl
      | selVal hbl =>
          obtain ⟨ρ', he | hu, hb⟩ := qtyped_rcd_inv h
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre with
            | found hty =>
                obtain ⟨e'', hbl', hte⟩ := QTypedBody.lookup_found hb hl'
                rw [hbl] at hbl'
                exact Option.some.inj hbl' ▸ .qEq hte hty.symm
          · cases hu
  | _, _, _, .qSelUnk h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .qSelUnk (qpreservation_aux h hΓ s) hl
      | selVal hbl =>
          obtain ⟨ρ', he | hu, hb⟩ := qtyped_rcd_inv h
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre
            exact (Lookup.not_unknown_of_spineVarFree hb.spineVarFree hl').elim
          · cases hu
  | _, _, _, .qSelAbs h hl, hΓ, _ => fun hs => by
      cases hs with
      | selStep s => exact .qSelAbs (qpreservation_aux h hΓ s) hl
      | selVal hbl =>
          obtain ⟨ρ', he | hu, hb⟩ := qtyped_rcd_inv h
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre
            rw [QTypedBody.lookup_absent hb hl'] at hbl
            cases hbl
          · cases hu
  | _, _, _, .qUnk h, hΓ, _ => fun hs =>
      .qUnk (qpreservation_aux h hΓ hs)
  | _, _, _, .qLab, _, _ => (nomatch ·)
  -- a FOUND dynamic lookup had a literal key: a label value types only at its
  -- own singleton (or ★, which finds nothing), so this is the static case
  | _, _, _, .qSelDyn h₁ h₂ hl, hΓ, _ => fun hs => by
      cases hs with
      | selDynL s => exact .qSelDyn (qpreservation_aux h₁ hΓ s) h₂ hl
      | selDynR _ s => exact .qSelDyn h₁ (qpreservation_aux h₂ hΓ s) hl
      | @selDynVal b l _ hbl =>
          rcases qtyped_lab_inv h₂ with he | hu
          case inr => subst hu; cases hl
          rw [he.lab_inv] at hl
          have hl := LookupQ.lab_iff.mp hl
          obtain ⟨ρ', he | hu, hb⟩ := qtyped_rcd_inv h₁
          · obtain ⟨_, hσ, hr⟩ := he.rcd_inv
            cases hσ
            obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm hr) hl
            cases hre with
            | found hty =>
                obtain ⟨e'', hbl', hte⟩ := QTypedBody.lookup_found hb hl'
                rw [hbl] at hbl'
                exact Option.some.inj hbl' ▸ .qEq hte hty.symm
          · cases hu
  -- a blurred one lands at ★: the field is typeable at SOMETHING, then blurred
  | _, _, _, .qSelDynUnk h₁ h₂ hl, hΓ, _ => fun hs => by
      cases hs with
      | selDynL s => exact .qSelDynUnk (qpreservation_aux h₁ hΓ s) h₂ hl
      | selDynR _ s => exact .qSelDynUnk h₁ (qpreservation_aux h₂ hΓ s) hl
      | @selDynVal b l _ hbl =>
          obtain ⟨ρ', -, hb⟩ := qtyped_rcd_inv h₁
          obtain ⟨_, _, hte⟩ := QTypedBody.lookup_some hb hbl
          exact .qUnk hte
  | _, _, _, .qSelDynAbs h₁ h₂ hl, hΓ, _ => fun hs => by
      cases hs with
      | selDynL s => exact .qSelDynAbs (qpreservation_aux h₁ hΓ s) h₂ hl
      | selDynR _ s => exact .qSelDynAbs h₁ (qpreservation_aux h₂ hΓ s) hl
      | @selDynVal b l _ hbl =>
          obtain ⟨ρ', -, hb⟩ := qtyped_rcd_inv h₁
          obtain ⟨_, _, hte⟩ := QTypedBody.lookup_some hb hbl
          exact .qUnk hte
  | _, _, _, .qLet hwf h₁ hne h₂, hΓ, _ => fun hs => by
      cases hs with
      | letCong s =>
          exact .qLet hwf (fun τ' hi => qpreservation_aux (h₁ τ' hi) hΓ s) hne h₂
      | letBeta hval =>
          subst hΓ
          exact qsubst_scheme_preserves_typing _ _ _ _ _ _ _
            (fun τ' hi => h₁ τ' hi) h₂

--========================= L2 METATHEORY: PROGRESS ==========================--
--   If ⊢_Q e : τ  then  e ∈ Value  ∨  (∃ e', e → e')  ∨  e ↯
-- Step/Value/Err/Progress are reused verbatim from minimal — only the typing
-- relation changed — so this tracks `progress` rule-for-rule. Two differences,
-- both real:
--   * the context must be empty, exactly as in L1. This used to read "only
--     tyEnv must be empty — Γ's ROW-solutions stay available, because L2
--     lookups (qSel*) read them"; qSel* read nothing but the row, so the
--     difference has closed.
--   * the qLet case consumes the INHABITATION premise instead of
--     Scheme.Inst.self. With Q ≠ ∅ a scheme need not instantiate at all, and a
--     vacuous one would let `let x = e₁ in e₂` type without saying anything
--     about e₁ — which is exactly where progress would break.

-- Selecting on a record literal always progresses: hit steps, miss errors.
-- (minimal's twin is private; the L2 development needs its own copy.)
private theorem qsel_rcd_progress {C : Type} (b : RecBody (Expr C)) (l : Label) :
    Progress (.sel (.rcd b) l) := by
  cases hbl : RecBody.lookup l b with
  | some e' => exact .step (.selVal hbl)
  | none    => exact .err (.selAbsent hbl)

-- …and so does a dynamic one on a record literal and a VALUE key, whatever its
-- type: a label steps or misses, anything else is not a label and errs.
private theorem qselDyn_rcd_progress {C : Type} (b : RecBody (Expr C)) {v : Expr C}
    (hv : Value v) : Progress (.selDyn (.rcd b) v) := by
  by_cases hl : ∃ l, v = .lab l
  · obtain ⟨l, rfl⟩ := hl
    cases hbl : RecBody.lookup l b with
    | some e' => exact .step (.selDynVal hbl)
    | none    => exact .err (.selDynAbsent hbl)
  · exact .err (.selDynKey hv (fun l he => hl ⟨l, he⟩))

private def qselDyn_progress {C : Type} {e₁ e₂ : Expr C}
    (p₁ : Progress e₁) (p₂ : Progress e₂)
    (hrcd : Value e₁ → ∃ b, e₁ = .rcd b) : Progress (.selDyn e₁ e₂) :=
  match p₁ with
  | .step s => .step (.selDynL s)
  | .err er => .err (.selDynL er)
  | .done v₁ =>
      match p₂ with
      | .step s => .step (.selDynR v₁ s)
      | .err er => .err (.selDynR v₁ er)
      | .done v₂ => by
          obtain ⟨b, rfl⟩ := hrcd v₁
          exact qselDyn_rcd_progress b v₂

def qprogress {B C : Type} {constTy : C → B} {Γ : QCtx B} {e : Expr C} {τ : Ty B}
    (hΓ : Γ.tyEnv = []) (ht : QTyped constTy Γ e τ) : Progress e :=
  match ht with
  | .qCon         => .done .con
  | .qVar h _     => by simp [QCtx.lookup, hΓ] at h
  | .qEq h _      => qprogress hΓ h
  | .qLam _       => .done .lam
  | .qRcd _       => .done .rcd
  | .qUnk h       => qprogress hΓ h
  | .qApp h₁ h₂   =>
      match qprogress hΓ h₁ with
      | .step s  => .step (.appFun s)
      | .err er  => .err (.appFun er)
      | .done v₁ =>
          match qprogress hΓ h₂ with
          | .step s  => .step (.appArg v₁ s)
          | .err er  => .err (.appArg v₁ er)
          | .done v₂ => by
              obtain ⟨x, e₀, rfl⟩ := qcanonical_fn v₁ h₁
              exact .step (.beta v₂)
  | .qCat h₁ h₂   =>
      match qprogress hΓ h₁ with
      | .step s  => .step (.catLeft s)
      | .err er  => .err (.catLeft er)
      | .done v₁ =>
          match qprogress hΓ h₂ with
          | .step s  => .step (.catRight v₁ s)
          | .err er  => .err (.catRight v₁ er)
          | .done v₂ => by
              obtain ⟨b₁, ρ₁', rfl, -, -⟩ := qcanonical_rcd v₁ h₁
              obtain ⟨b₂, ρ₂', rfl, -, -⟩ := qcanonical_rcd v₂ h₂
              exact .step .catVal
  | .qSel he hl   =>
      match qprogress hΓ he with
      | .step s => .step (.selStep s)
      | .err er => .err (.sel er)
      | .done v => by
          -- carry the found-lookup across ≈ᵣ onto the literal's row, then read
          -- the field off the body
          obtain ⟨b, ρ', rfl, heq, hb⟩ := qcanonical_rcd v he
          obtain ⟨r', hl', hre⟩ := lookup_equiv (RowEquiv.symm heq) hl
          cases hre
          obtain ⟨e', hbl, -⟩ := QTypedBody.lookup_found hb hl'
          exact .step (.selVal hbl)
  | .qSelUnk he _ =>
      match qprogress hΓ he with
      | .step s => .step (.selStep s)
      | .err er => .err (.sel er)
      | .done v => by
          obtain ⟨b, ρ', rfl, -, -⟩ := qcanonical_rcd v he
          exact qsel_rcd_progress b _
  -- T-sel-⊥ one level up: the ↯-disjunct at work — typed (at ★) and errs.
  | .qSelAbs he _ =>
      match qprogress hΓ he with
      | .step s => .step (.selStep s)
      | .err er => .err (.sel er)
      | .done v => by
          obtain ⟨b, ρ', rfl, -, -⟩ := qcanonical_rcd v he
          exact qsel_rcd_progress b _
  -- the inhabitation premise picks ONE discharged instance; the binding types
  -- there, which is all progress needs to drive it to a value, error or step
  | .qLet hwf h₁ hne h₂ =>
      match qprogress hΓ (h₁ _ hne.choose_spec) with
      | .step s  => .step (.letCong s)
      | .err er  => .err (.letBind er)
      | .done v₁ => .step (.letBeta v₁)
  | .qLab => .done .lab
  | .qSelDyn h₁ h₂ _ => qselDyn_progress (qprogress hΓ h₁) (qprogress hΓ h₂)
      (fun v => let ⟨b, _, he, _⟩ := qcanonical_rcd v h₁; ⟨b, he⟩)
  | .qSelDynUnk h₁ h₂ _ => qselDyn_progress (qprogress hΓ h₁) (qprogress hΓ h₂)
      (fun v => let ⟨b, _, he, _⟩ := qcanonical_rcd v h₁; ⟨b, he⟩)
  | .qSelDynAbs h₁ h₂ _ => qselDyn_progress (qprogress hΓ h₁) (qprogress hΓ h₂)
      (fun v => let ⟨b, _, he, _⟩ := qcanonical_rcd v h₁; ⟨b, he⟩)

-- ⊢  ⊢_Q e : τ   ⟹   e is a value, steps, or is a lookup-error
theorem qProgress {B C : Type} (constTy : C → B) (e : Expr C) (τ : Ty B)
    (ht : QTyped constTy QCtx.empty e τ) : Progress e :=
  qprogress rfl ht


theorem qPreservation
    {B C : Type} (constTy : C → B)
    (e e' : Expr C) (τ : Ty B)
    (ht : QTyped constTy QCtx.empty e τ)
    (hs : Step e e') :
    QTyped constTy QCtx.empty e' τ :=
  qpreservation_aux ht rfl hs

--====================== PRINCIPALITY IN THE ⊴⊑ ORDER ========================--
-- The other half of selQ_no_blurred_inst: the type it refuses IS a typing.
-- T-★-intro blurs the found-selection, so the typing set of λx.x.l contains
-- {(l: 𝓫)} → ★ while the instance set of selQ does not.
-- ⊢  ∅ ⊢_Q λx.x.l : {(l: 𝓫_c)} → ★
theorem selEx_blurred_typing {B C : Type} (constTy : C → B) (c : C) :
    QTyped constTy QCtx.empty (selEx C)
      (.fn (.rcd (.sing "l" (.base (constTy c)))) .unk) :=
  by
  refine .qLam (.qUnk (τ := .base (constTy c)) (.qSel
    (.qVar (σ := ⟨[], [], .rcd (.sing "l" (.base (constTy c)))⟩) ?_ ?_) .hit))
  · simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup]
  · exact QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], _⟩)

-- The two halves together: selQ is instance-closed, yet a typing of λx.x.l
-- escapes its instance set. "Instances = typings" is therefore unreachable for
-- this scheme, and principality MUST be stated up to precision.
-- ⊢  (∅ ⊢_Q λx.x.l : {(l: 𝓫_c)} → ★)  ∧  ¬ (selQ ≥_∅ {(l: 𝓫_c)} → ★)
theorem selQ_misses_a_typing {B C : Type} (constTy : C → B) (c : C) :
    QTyped constTy QCtx.empty (selEx C)
        (.fn (.rcd (.sing "l" (.base (constTy c)))) .unk) ∧
    ¬ QScheme.Inst (selQ B)
        (.fn (.rcd (.sing "l" (.base (constTy c)))) .unk) :=
  ⟨selEx_blurred_typing constTy c, selQ_no_blurred_inst (constTy c)⟩

-- PRINCIPALITY. Three conjuncts, one per failure mode the development has
-- already exhibited:
--   (1) SOUND       every instance is a typing        (selQ_instance_closed)
--   (2) INHABITED   at least one instance exists      (qLet's premise; ⊴ is
--                   blind to vacuity, coveredAt_of_uninhabited)
--   (3) COVERING    every typing is answered by an instance AT LEAST AS
--                   PRECISE — up to precision, because the typing set is
--                   blur-closed and no instance set is (selQ_misses_a_typing)
--
-- CONJUNCT 3 IN THIS ⊑-ONLY FORM IS REFUTED: selQ_not_principalStrict. The
-- typing set is closed under T-eq as well, and ⊑ cannot absorb ≈. Principal
-- (below, with ≼) is the corrected statement; this one is kept because it is
-- what the refutation is about.
def QScheme.PrincipalStrict {B C : Type} (constTy : C → B) (Γ : QCtx B)
    (e : Expr C) (σ : QScheme B) : Prop :=
  (∀ τ, QScheme.Inst σ τ → QTyped constTy Γ e τ) ∧
  (∃ τ, QScheme.Inst σ τ) ∧
  (∀ τ, QTyped constTy Γ e τ → ∃ τ', QScheme.Inst σ τ' ∧ TyPrec τ' τ)

-- A principal scheme is ⊴⊑-greatest among the SOUND schemes for e: any scheme
-- whose instances are all typings sits below it. This is what makes the
-- definition an order-theoretic one rather than three unrelated clauses.
-- ⊢  σ principal for e,  every σ''-instance a typing  ⟹  σ'' ⊴⊑ σ
theorem QScheme.PrincipalStrict.greatest {B C : Type} {constTy : C → B}
    {Γ : QCtx B} {e : Expr C} {σ σ'' : QScheme B}
    (hp : QScheme.PrincipalStrict constTy Γ e σ)
    (hcl : ∀ τ, QScheme.Inst σ'' τ → QTyped constTy Γ e τ) :
    σ'' ⊴⊑ σ :=
  fun τ hτ => hp.2.2 τ (hcl τ hτ)

-- Where selQ stands today: conjuncts (1) and (2) are discharged here. (3) is
-- REFUTED in its ⊑-only form (selQ_not_principalStrict) and OPEN in the
-- corrected ≼ form, where it needs an L2 inversion for λx.x.l the way
-- minimal.lean's sel_var_unk is the L1 one.
-- ⊢  (∀ τ. selQ ≥_∅ τ ⟹ ∅ ⊢_Q λx.x.l : τ)  ∧  ∃ τ. selQ ≥_∅ τ
theorem selQ_sound_and_inhabited {B C : Type} (constTy : C → B) :
    (∀ τ, QScheme.Inst (selQ B) τ →
      QTyped constTy (QCtx.empty : QCtx B) (selEx C) τ) ∧
    (∃ τ, QScheme.Inst (selQ B) τ) :=
  ⟨fun τ hτ => (selQ_instance_closed constTy Ctx.empty τ hτ).toQ,
   ⟨_, selQ_inst_absent⟩⟩


--=================== (1) ⊴ ACTS ON CONTEXTS: SCHEME WEAKENING =================--
-- The order earns its keep here. A context binding is consumed by qVar ALONE,
-- and qVar instantiates — so replacing a bound scheme by a ⊴-larger one can
-- only add instances and every use survives. QCtx.Sub demands EQUAL schemes;
-- this is the same preorder with ⊴ in its place.
--
-- CONTRAVARIANCE WARNING. Generality helps only in the CONTEXT. The scheme a
-- `let` generalizes to sits in qLet's instance-closed premise, which quantifies
-- over ALL its instances, so making THAT scheme larger makes the premise
-- HARDER. `let` is antitone in the scheme it binds and monotone in the context
-- it types under; only the latter is what this section is about, which is why
-- the qLet case below rebinds the SAME σ on both sides.

def QCtx.SubCov {B : Type} (Γ₁ Γ₂ : QCtx B) : Prop :=
  ∀ x σ, Γ₁.lookup x = some σ → ∃ σ', Γ₂.lookup x = some σ' ∧ σ ⊴ σ'

-- Equal schemes are a special case: ⊴ is reflexive.
theorem QCtx.Sub.toSubCov {B : Type} {Γ₁ Γ₂ : QCtx B} (h : QCtx.Sub Γ₁ Γ₂) :
    QCtx.SubCov Γ₁ Γ₂ :=
  fun x σ hx => ⟨σ, h x σ hx, QScheme.Covered.refl σ⟩

theorem QCtx.SubCov.refl {B : Type} (Γ : QCtx B) : QCtx.SubCov Γ Γ :=
  (QCtx.Sub.refl Γ).toSubCov

-- Binding the same scheme on both sides respects it.
theorem QCtx.SubCov.bindScheme {B : Type} {Γ₁ Γ₂ : QCtx B}
    (h : QCtx.SubCov Γ₁ Γ₂) (x : Var) (σ : QScheme B) :
    QCtx.SubCov (Γ₁.bindScheme x σ) (Γ₂.bindScheme x σ) := by
  intro y σ' hy
  rw [QCtx.lookup_bindScheme] at hy ⊢
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact h y σ' hy
  · simp only [hxy, if_true] at hy ⊢
    exact ⟨σ', hy, QScheme.Covered.refl σ'⟩

theorem QCtx.SubCov.bindTy {B : Type} {Γ₁ Γ₂ : QCtx B} (h : QCtx.SubCov Γ₁ Γ₂)
    (x : Var) (τ : Ty B) :
    QCtx.SubCov (Γ₁.bindTy x τ) (Γ₂.bindTy x τ) :=
  h.bindScheme x ⟨[], [], τ⟩

-- ⊢  Γ₁ ⊴-sub Γ₂,  Γ₁ ⊢_Q e : τ   ⟹   Γ₂ ⊢_Q e : τ
mutual
theorem qtyped_cov {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : QCtx B} → {e : Expr C} → {τ : Ty B} → QCtx.SubCov Γ₁ Γ₂ →
    QTyped constTy Γ₁ e τ → QTyped constTy Γ₂ e τ
  | _, _, _, _, _,  .qCon         => .qCon
  | _, _, _, _, hs, .qVar h hi    =>
      let ⟨_, h', hcov⟩ := hs _ _ h
      .qVar h' (hcov _ hi)
  | _, _, _, _, hs, .qEq h heq    => .qEq (qtyped_cov hs h) heq
  | _, _, _, _, hs, .qLam h       => .qLam (qtyped_cov (hs.bindTy _ _) h)
  | _, _, _, _, hs, .qApp h₁ h₂   => .qApp (qtyped_cov hs h₁) (qtyped_cov hs h₂)
  | _, _, _, _, hs, .qCat h₁ h₂   => .qCat (qtyped_cov hs h₁) (qtyped_cov hs h₂)
  | _, _, _, _, hs, .qSel h hl    =>
      .qSel (qtyped_cov hs h) hl
  | _, _, _, _, hs, .qSelUnk h hl =>
      .qSelUnk (qtyped_cov hs h) hl
  | _, _, _, _, hs, .qSelAbs h hl =>
      .qSelAbs (qtyped_cov hs h) hl
  | _, _, _, _, hs, .qUnk h       => .qUnk (qtyped_cov hs h)
  | _, _, _, _, hs, .qLet hwf h₁ hne h₂ =>
      .qLet hwf (fun τ' hi =>
              qtyped_cov hs (h₁ τ' hi))
            hne
            (qtyped_cov (hs.bindScheme _ _) h₂)
  | _, _, _, _, hs, .qRcd h       => .qRcd (qtypedBody_cov hs h)
  | _, _, _, _, _,  .qLab         => .qLab
  | _, _, _, _, hs, .qSelDyn h₁ h₂ hl =>
      .qSelDyn (qtyped_cov hs h₁) (qtyped_cov hs h₂) hl
  | _, _, _, _, hs, .qSelDynUnk h₁ h₂ hl =>
      .qSelDynUnk (qtyped_cov hs h₁) (qtyped_cov hs h₂) hl
  | _, _, _, _, hs, .qSelDynAbs h₁ h₂ hl =>
      .qSelDynAbs (qtyped_cov hs h₁) (qtyped_cov hs h₂) hl

theorem qtypedBody_cov {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    QCtx.SubCov Γ₁ Γ₂ → QTypedBody constTy Γ₁ b ρ → QTypedBody constTy Γ₂ b ρ
  | _, _, _, _, _,  .empty     => .empty
  | _, _, _, _, hs, .field h   => .field (qtyped_cov hs h)
  | _, _, _, _, hs, .cat h₁ h₂ =>
      .cat (qtypedBody_cov hs h₁) (qtypedBody_cov hs h₂)
end

-- The headline corollary: swap ONE binding for a ⊴-larger scheme.
-- ⊢  Γ, x:σ ⊢_Q e : τ,  σ ⊴ σ'   ⟹   Γ, x:σ' ⊢_Q e : τ
theorem qtyped_bind_cov {B C : Type} {constTy : C → B} {Γ : QCtx B} {x : Var}
    {e : Expr C} {τ : Ty B} {σ σ' : QScheme B}
    (h : QTyped constTy (Γ.bindScheme x σ) e τ)
    (hcov : σ ⊴ σ') :
    QTyped constTy (Γ.bindScheme x σ') e τ := by
  refine qtyped_cov (Γ₁ := Γ.bindScheme x σ) (Γ₂ := Γ.bindScheme x σ')
    (fun y μ hy => ?_) h
  rw [QCtx.lookup_bindScheme] at hy ⊢
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact ⟨μ, hy, QScheme.Covered.refl μ⟩
  · simp only [hxy, if_true] at hy ⊢
    cases hy
    exact ⟨σ', rfl, hcov⟩


--============ (3) ⊴ IS NOT STABLE UNDER SUBSTITUTION =========================--
-- The question the order has to answer: does refining the solution preserve
-- σ ⊴ σ'? NO. Before the row environment was removed this was stated as
-- "Γ ⊑ Γ' does not preserve σ ⊴[Γ] σ'", and it was the reason the order came
-- in a Γ-relative and a uniform flavor. Refining the solution is APPLYING A
-- SUBSTITUTION, so the same counterexample now says what it always meant: ⊴ is
-- not a congruence for `QScheme.applySubst`, and a generality claim is a claim
-- about the substitution it was made under, nothing more.
--
-- The mechanism is exactly the ?-arm of discharge that `lookup_applySubst`
-- refuses to transport: σ below parks on a FREE row-var, so it can only
-- discharge at ★ and its instance set is {★} — matched by the monotype ★.
-- Substituting α turns the same stump into a definite hit, the instance set
-- moves to {𝓫}, and the monotype cannot follow.

-- ∀δ. ⟨α.l ↓ δ⟩ ⇒ δ      (α FREE — the stump parks on a free variable, not on a binder)
private def parkQ (B : Type) : QScheme B :=
  ⟨["δ"], [⟨.var "α", .lab "l", .var "δ"⟩], .var "δ"⟩

-- χ = [α ↦ (l: 𝓫)] at the row sort — "the solver solved α"
private def solveAlpha {B : Type} (b : B) : TySubst B :=
  ⟨(.var ·), fun γ => if γ == "α" then .sing "l" (.base b) else .var γ⟩

-- With α free the lookup is ? and discharge can only finalize at ★.
-- ⊢  parkQ ≥ τ  ⟹  τ = ★
private theorem parkQ_inst_free {B : Type} {τ : Ty B}
    (h : QScheme.Inst (parkQ B) τ) : τ = .unk := by
  obtain ⟨θ, hfix, hdis, hbody⟩ := h
  have hrow : (Row.var "α").applySubst θ = Row.var "α" := by
    simp only [Row.applySubst]
    exact hfix.2 "α" (by simp [parkQ])
  have hs := hdis ⟨.var "α", .lab "l", .var "δ"⟩ (by simp [parkQ])
  simp only [parkQ, Ty.applySubst] at hbody
  cases hs with
  | hit hl _ => rw [hrow] at hl; cases LookupQ.lab_iff.mp hl
  | abs hl _ => rw [hrow] at hl; cases LookupQ.lab_iff.mp hl
  | unk _ hres => rw [← hbody, show θ.ty "δ" = _ from hres]

-- Substitute α and the SAME scheme instantiates to 𝓫 instead.
-- ⊢  χ·parkQ ≥ 𝓫
private theorem parkQ_inst_solved {B : Type} (b : B) :
    QScheme.Inst ((parkQ B).applySubst (solveAlpha b)) (.base b) := by
  refine ⟨⟨fun γ => if γ = "δ" then .base b else .var γ, fun γ => .var γ⟩,
          ⟨fun γ hγ => ?_, fun _ _ => rfl⟩, fun st hst => ?_, ?_⟩
  · have hne : γ ≠ "δ" := by
      rintro rfl; exact hγ (by simp [parkQ, QScheme.applySubst])
    simp [hne]
  · simp only [parkQ, QScheme.applySubst, List.map_cons, List.map_nil,
               List.mem_singleton] at hst
    subst hst
    refine .hit (τ := .base b) ?_ (by simp [solveAlpha, Ty.applySubst])
    show LookupQ ((Row.applySubst (solveAlpha b) (.var "α")).applySubst _) _ _
    simp only [solveAlpha, Row.applySubst, Ty.applySubst, beq_self_eq_true,
               if_true]
    exact .lit Lookup.hit
  · simp [parkQ, QScheme.applySubst, solveAlpha, Ty.applySubst]

-- ⊢  χ·⟨★⟩ = ⟨★⟩   (a monotype ★ has nothing for χ to act on)
private theorem unkQ_applySubst {B : Type} (b : B) :
    ((⟨[], [], .unk⟩ : QScheme B)).applySubst (solveAlpha b) = ⟨[], [], .unk⟩ :=
  rfl

-- The counterexample, both orders at once.
-- ⊢  parkQ ⊴ ⟨★⟩  ∧  ¬ (χ·parkQ ⊴ χ·⟨★⟩)  ∧  ¬ (χ·parkQ ⊴⊑ χ·⟨★⟩)
theorem covered_not_applySubst_stable {B : Type} (b : B) :
    (parkQ B ⊴ ⟨[], [], .unk⟩) ∧
    ¬ ((parkQ B).applySubst (solveAlpha b) ⊴
        (⟨[], [], .unk⟩ : QScheme B).applySubst (solveAlpha b)) ∧
    ¬ ((parkQ B).applySubst (solveAlpha b) ⊴⊑
        (⟨[], [], .unk⟩ : QScheme B).applySubst (solveAlpha b)) := by
  refine ⟨fun τ hτ => ?_, fun hcov => ?_, fun hcov => ?_⟩
  · rw [parkQ_inst_free hτ]
    exact QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], .unk⟩)
  · rw [unkQ_applySubst b] at hcov
    have := QScheme.Inst.mono (hcov _ (parkQ_inst_solved b))
    cases this
  · rw [unkQ_applySubst b] at hcov
    obtain ⟨τ', hτ', hp⟩ := hcov _ (parkQ_inst_solved b)
    cases QScheme.Inst.mono hτ'
    cases TyPrec.unk_below hp


--================ (2) A SYNTACTIC WITNESS FOR ⊴ ==============================--
-- ⊴ as defined quantifies over instances, so nothing can ever PRODUCE evidence
-- of it — a solver needs a certificate. This is the Jones-style generic-instance
-- condition adapted to stumps: one substitution θ that instantiates σ' to σ,
-- a freshness side condition, and entailment of σ''s stumps by σ's.
--
-- The entailment premise is where this calculus differs from the qualified-type
-- frameworks it borrows from. There, entailment is a PARAMETER with no solving
-- procedure. Here discharge is "run the lookup and compare", so the premise is
-- a decidable check on concrete stumps, not an assumption about a relation.

-- θ cut down to `vars` — the standard fix for composing an instantiation with a
-- substitution whose domain overlaps the other scheme's binders.
def TySubst.restrict {B : Type} (θ : TySubst B) (vars : List TyVar) : TySubst B :=
  ⟨fun γ => if γ ∈ vars then θ.ty γ else .var γ,
   fun γ => if γ ∈ vars then θ.row γ else .var γ⟩

theorem TySubst.restrict_fixedOutside {B : Type} (θ : TySubst B)
    (vars : List TyVar) : (θ.restrict vars).FixedOutside vars :=
  ⟨fun _ h => by simp [TySubst.restrict, h], fun _ h => by simp [TySubst.restrict, h]⟩

-- σ ⊴ σ' certified by θ.
structure QScheme.Witness {B : Type} (σ σ' : QScheme B)
    (θ : TySubst B) : Prop where
  -- θ only moves σ''s own binders
  fixed  : θ.FixedOutside σ'.vars
  -- ... and instantiates σ' to σ
  body   : σ'.body.applySubst θ = σ.body
  -- σ's binders are fresh for σ' (Jones' side condition; without it the two
  -- binder groups capture each other and the composite is not an instantiation)
  fresh  : ∀ γ ∈ σ'.body.ftv, γ ∉ σ'.vars → γ ∉ σ.vars
  -- every instantiation of σ that discharges σ's stumps discharges σ''s
  entail : ∀ χ : TySubst B, χ.FixedOutside σ.vars →
             (∀ s ∈ σ.constraints, s.Discharge χ) →
             ∀ s' ∈ σ'.constraints,
               s'.Discharge ((χ.comp θ).restrict σ'.vars)

-- ⊢  Witness σ σ' θ   ⟹   σ ⊴ σ'
theorem QScheme.covered_of_witness {B : Type} {σ σ' : QScheme B}
    {θ : TySubst B} (h : QScheme.Witness σ σ' θ) : σ ⊴ σ' := by
  rintro τ ⟨χ, hχfix, hχdis, hχbody⟩
  refine ⟨(χ.comp θ).restrict σ'.vars,
          TySubst.restrict_fixedOutside _ _,
          h.entail χ hχfix hχdis, ?_⟩
  -- the restriction is invisible to σ'.body: outside σ'.vars the composite is
  -- already the identity, by θ's fixedness and σ's binders being fresh
  have hcongr : σ'.body.applySubst ((χ.comp θ).restrict σ'.vars)
      = σ'.body.applySubst (χ.comp θ) := by
    refine Ty.applySubst_congr _ (fun γ hγ => ?_)
    by_cases hv : γ ∈ σ'.vars
    · simp [TySubst.restrict, hv]
    · have hσ : γ ∉ σ.vars := h.fresh γ hγ hv
      constructor
      · simp [TySubst.restrict, hv, TySubst.comp, h.fixed.1 γ hv,
              Ty.applySubst, hχfix.1 γ hσ]
      · simp [TySubst.restrict, hv, TySubst.comp, h.fixed.2 γ hv,
              Row.applySubst, hχfix.2 γ hσ]
  rw [hcongr, ← Ty.applySubst_applySubst, h.body, hχbody]

-- PLAIN SCHEMES: with no stumps the entailment premise is empty and the
-- condition collapses to the textbook generic-instance rule — σ' covers σ as
-- soon as one substitution on σ''s binders reaches σ's body.
-- ⊢  θ(σ'.body) = σ.body,  θ fixed outside σ'.vars,  σ.vars fresh for σ'
--        ⟹  σ.toQ ⊴ σ'.toQ
theorem Scheme.covered_of_inst {B : Type} {σ σ' : Scheme B} {θ : TySubst B}
    (hfix : θ.FixedOutside σ'.vars)
    (hbody : σ'.body.applySubst θ = σ.body)
    (hfresh : ∀ γ ∈ σ'.body.ftv, γ ∉ σ'.vars → γ ∉ σ.vars) :
    σ.toQ ⊴ σ'.toQ :=
  QScheme.covered_of_witness
    { fixed := hfix, body := hbody, fresh := hfresh,
      entail := fun _ _ _ s' hs' => by simp [Scheme.toQ] at hs' }


-- A WITNESS WITH A REAL STUMP. selQ with its row-variable already solved:
--   ∀δ. ⟨(l: 𝓫).l ↓ δ⟩ ⇒ {(l: 𝓫)} → δ      ⊴      ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ
-- θ = [β ↦ (l: 𝓫)] certifies it, and the entailment premise is discharged by
-- RUNNING the lookup: on the solved row it is a definite hit, so χ's image of δ
-- is pinned at 𝓫 and the composite discharges selQ's stump at the same type.
-- This is the step a solver would emit when it instantiates a parked scheme.
private def selQb {B : Type} (b : B) : QScheme B :=
  ⟨["δ"], [⟨.sing "l" (.base b), .lab "l", .var "δ"⟩],
   .fn (.rcd (.sing "l" (.base b))) (.var "δ")⟩

private def solveβ {B : Type} (b : B) : TySubst B :=
  ⟨fun γ => .var γ, fun γ => if γ = "β" then .sing "l" (.base b) else .var γ⟩

-- ⊢  selQb ⊴ selQ
theorem selQ_covers_selQb {B : Type} (b : B) : selQb b ⊴ selQ B := by
  refine QScheme.covered_of_witness (θ := solveβ b) ⟨⟨fun _ _ => rfl, ?_⟩, ?_, ?_, ?_⟩
  · intro γ hγ
    have hne : γ ≠ "β" := by rintro rfl; exact hγ (by simp [selQ])
    simp [solveβ, hne]
  · simp [selQ, selQb, solveβ, Ty.applySubst, Row.applySubst]
  · intro γ _ hv hc
    simp [selQb] at hc
    exact hv (by simp [selQ, hc])
  · intro χ hχfix hdis s' hs'
    simp only [selQ, List.mem_singleton] at hs'
    subst hs'
    have hs := hdis ⟨.sing "l" (.base b), .lab "l", .var "δ"⟩ (by simp [selQb])
    have hrowχ : (Row.sing "l" (Ty.base b)).applySubst χ = .sing "l" (.base b) := by
      simp [Row.applySubst, Ty.applySubst]
    have hδ : χ.ty "δ" = .base b := by
      cases hs with
      | hit hl hres =>
          rw [hrowχ] at hl
          cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit (l := "l") (τ := .base b))
          exact hres
      | abs hl _ =>
          rw [hrowχ] at hl
          cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit (l := "l") (τ := .base b))
      | unk hl _ =>
          rw [hrowχ] at hl
          cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit (l := "l") (τ := .base b))
    refine .hit (τ := .base b) ?_ ?_
    · have hrow : (Row.var "β").applySubst
            ((χ.comp (solveβ b)).restrict (selQ B).vars) = .sing "l" (.base b) := by
        simp [Row.applySubst, TySubst.restrict, selQ, TySubst.comp, solveβ,
              Ty.applySubst]
      rw [hrow]
      exact .lit .hit
    · simp [TySubst.restrict, selQ, TySubst.comp, solveβ, Ty.applySubst, hδ]


--========= (4) CONJUNCT 3 IS FALSE AS STATED — ≈ MUST JOIN ⊑ ================--
-- Attempting `Principal selQ (λx.x.l)` refutes its own third conjunct. The
-- typing set has TWO closure properties, not one: T-★-intro blurs (which ⊴⊑
-- absorbs) and T-eq re-associates rows (which it does NOT). ⊑ is pure
-- congruence on rows — it can sharpen a payload but can never move a field
-- past a unit — so a typing obtained by ≈ on the RESULT is out of reach of
-- every instance, however imprecise.
--
-- Witness: x.l selects a payload {ε | m: 𝓬} and T-eq retypes it as {m: 𝓬}.
--   λx. x.l  :  {(l: {ε | m: 𝓬})} → {m: 𝓬}
-- Any selQ instance ⊑-below this must, by the DOMAIN, put the same payload τ_p
-- under l, hence have RESULT τ_p (the lookup is a definite hit), and τ_p would
-- have to be ⊑-below both {ε | m: 𝓬} and {m: 𝓬}. The first forces τ_p's row to
-- be a `cat`, the second forces it to be a `sing`. Nothing satisfies both.

private def blurA {B : Type} (b : B) : Ty B :=
  .rcd (.cat .empty (.sing "m" (.base b)))

private def blurB {B : Type} (b : B) : Ty B :=
  .rcd (.sing "m" (.base b))

-- ⊢  ∅ ⊢_Q λx.x.l : {(l: {ε | m: 𝓫})} → {m: 𝓫}
theorem selEx_equiv_typing {B C : Type} (constTy : C → B) (c : C) :
    QTyped constTy QCtx.empty (selEx C)
      (.fn (.rcd (.sing "l" (blurA (constTy c)))) (blurB (constTy c))) := by
  refine .qLam (.qEq (τ₁ := blurA (constTy c)) (.qSel
    (.qVar (σ := ⟨[], [], .rcd (.sing "l" (blurA (constTy c)))⟩) ?_ ?_) .hit) ?_)
  · simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup]
  · exact QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], _⟩)
  · exact .rcd .unitL

-- ⊢  ¬ ∃ τ'. selQ ≥_∅ τ' ∧ τ' ⊑ₜ ({(l: {ε | m: 𝓫})} → {m: 𝓫})
theorem selQ_no_prec_answer {B : Type} (b : B) :
    ¬ ∃ τ', QScheme.Inst (selQ B) τ' ∧
        TyPrec τ' (.fn (.rcd (.sing "l" (blurA b))) (blurB b)) := by
  rintro ⟨τ', ⟨θ, hfix, hdis, hbody⟩, hprec⟩
  simp only [selQ, Ty.applySubst, Row.applySubst] at hbody
  subst hbody
  obtain ⟨d, r, hfn, hd, hr⟩ := TyPrec.fn_inv hprec
  injection hfn with h1 h2
  subst h1; subst h2
  -- the domain pins θβ to one l-field whose payload τp is ⊑-below blurA
  obtain ⟨ρp, hρp, hρprec⟩ := TyPrec.rcd_inv hd
  injection hρp with hβ
  obtain ⟨τp, hsing, hτp⟩ := RowPrec.sing_inv hρprec
  subst hsing
  -- the lookup on that row is a definite hit, so discharge pins θδ = τp
  have hs := hdis ⟨.var "β", .lab "l", .var "δ"⟩ (by simp [selQ])
  have hrow : (Row.var "β").applySubst θ = Row.sing "l" τp := by
    simp only [Row.applySubst]; exact hβ
  have hδ : θ.ty "δ" = τp := by
    cases hs with
    | hit hl hres =>
        rw [hrow] at hl
        cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit (l := "l") (τ := τp))
        exact hres
    | abs hl _ =>
        rw [hrow] at hl
        cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit (l := "l") (τ := τp))
    | unk hl _ =>
        rw [hrow] at hl
        cases lookup_det (LookupQ.lab_iff.mp hl) (Lookup.hit (l := "l") (τ := τp))
  rw [hδ] at hr
  -- τp ⊑ blurA forces a `cat` row, τp ⊑ blurB a `sing` row: no row is both
  simp only [blurA] at hτp
  simp only [blurB] at hr
  obtain ⟨ρ₁, hρ₁, hp₁⟩ := TyPrec.rcd_inv hτp
  subst hρ₁
  obtain ⟨ρ₂, hρ₂, hp₂⟩ := TyPrec.rcd_inv hr
  injection hρ₂ with hρeq
  subst hρeq
  obtain ⟨a, a', hcatshape, -, -⟩ := RowPrec.cat_inv hp₁
  obtain ⟨t, hsingshape, -⟩ := RowPrec.sing_inv hp₂
  rw [hcatshape] at hsingshape
  cases hsingshape

-- The corollary, and the correction it forces: principality must be stated
-- modulo BOTH closure properties. τ' ≼ τ — "τ' is at least as informative as
-- τ, up to row equivalence" — is the relation that does it.
def TyBelow {B : Type} (τ' τ : Ty B) : Prop :=
  ∃ τ₀, TyEquiv τ' τ₀ ∧ TyPrec τ₀ τ

infix:50 " ≼ₜ " => TyBelow

theorem TyBelow.refl {B : Type} (τ : Ty B) : τ ≼ₜ τ := ⟨τ, .refl τ, .refl τ⟩

theorem TyBelow.of_prec {B : Type} {τ' τ : Ty B} (h : TyPrec τ' τ) : τ' ≼ₜ τ :=
  ⟨τ', .refl τ', h⟩

theorem TyBelow.of_equiv {B : Type} {τ' τ : Ty B} (h : TyEquiv τ' τ) : τ' ≼ₜ τ :=
  ⟨τ, h, .refl τ⟩

-- ≼ IS TRANSITIVE — the composite ≈;⊑;≈;⊑ collapses back to ≈;⊑ because the
-- middle ⊑;≈ can be swapped (TyPrec.comm_equiv). Without that swap ≼ would be
-- a relation with no composition law, and ⊴≼ could not be an order at all.
-- ⊢  τ₁ ≼ₜ τ₂ ⟹ τ₂ ≼ₜ τ₃ ⟹ τ₁ ≼ₜ τ₃
theorem TyBelow.trans {B : Type} {τ₁ τ₂ τ₃ : Ty B}
    (h₁ : τ₁ ≼ₜ τ₂) (h₂ : τ₂ ≼ₜ τ₃) : τ₁ ≼ₜ τ₃ := by
  obtain ⟨a, he₁, hp₁⟩ := h₁
  obtain ⟨b, he₂, hp₂⟩ := h₂
  obtain ⟨a', ha', hp'⟩ := (TyPrec.comm_equiv he₂).1 a hp₁
  exact ⟨a', he₁.trans ha', hp'.trans hp₂⟩

-- ⊑-covering fails for selQ at λx.x.l; ≼-covering does not (the same instance
-- answers, now via ≈ on the result instead of ⊑).
-- ⊢  ¬(⊑-answer)  ∧  ∃ τ'. selQ ≥_∅ τ' ∧ τ' ≼ₜ ({(l: {ε | m: 𝓫})} → {m: 𝓫})
theorem selQ_needs_equiv {B : Type} (b : B) :
    (¬ ∃ τ', QScheme.Inst (selQ B) τ' ∧
        TyPrec τ' (.fn (.rcd (.sing "l" (blurA b))) (blurB b))) ∧
    (∃ τ', QScheme.Inst (selQ B) τ' ∧
        τ' ≼ₜ (.fn (.rcd (.sing "l" (blurA b))) (blurB b))) :=
  ⟨selQ_no_prec_answer b,
   ⟨_, selQ_inst_found (blurA b),
    TyBelow.of_equiv (.fn (.refl _) (.rcd .unitL))⟩⟩


-- selQ does NOT satisfy the ⊑-only principality — conjunct 3 fails on the
-- T-eq witness. This is the statement that had to be corrected, not a defect
-- of selQ: no instance set is closed under ≈.
-- ⊢  ¬ PrincipalStrict selQ (λx.x.l)
theorem selQ_not_principalStrict {B C : Type} (constTy : C → B) (c : C) :
    ¬ QScheme.PrincipalStrict constTy (QCtx.empty : QCtx B) (selEx C) (selQ B) :=
  fun hp => selQ_no_prec_answer (constTy c)
    (hp.2.2 _ (selEx_equiv_typing constTy c))

-- The corrected order: covering up to ≈-then-⊑.
def QScheme.BelowCovered {B : Type} (σ σ' : QScheme B) : Prop :=
  ∀ τ, QScheme.Inst σ τ → ∃ τ', QScheme.Inst σ' τ' ∧ τ' ≼ₜ τ

infix:50 " ⊴≼ " => QScheme.BelowCovered

theorem QScheme.BelowCovered.refl {B : Type} (σ : QScheme B) :
    σ ⊴≼ σ := fun τ hτ => ⟨τ, hτ, TyBelow.refl τ⟩

theorem QScheme.PrecCovered.toBelow {B : Type} {σ σ' : QScheme B}
    (h : σ ⊴⊑ σ') : σ ⊴≼ σ' :=
  fun τ hτ => let ⟨τ', hτ', hp⟩ := h τ hτ; ⟨τ', hτ', TyBelow.of_prec hp⟩

-- ... and with ≼ transitive, ⊴≼ is a PREORDER: the order principality is
-- stated in composes, so "σ'' is below the principal scheme" can be chained.
theorem QScheme.BelowCovered.trans {B : Type}
    {σ₁ σ₂ σ₃ : QScheme B} (h₁ : σ₁ ⊴≼ σ₂) (h₂ : σ₂ ⊴≼ σ₃) :
    σ₁ ⊴≼ σ₃ := by
  intro τ hτ
  obtain ⟨τ', hτ', hb'⟩ := h₁ τ hτ
  obtain ⟨τ'', hτ'', hb''⟩ := h₂ τ' hτ'
  exact ⟨τ'', hτ'', hb''.trans hb'⟩

-- PRINCIPALITY, CORRECTED. Same three conjuncts; the covering one now reads
-- the typing set's OTHER closure property as well.
def QScheme.Principal {B C : Type} (constTy : C → B) (Γ : QCtx B) (e : Expr C)
    (σ : QScheme B) : Prop :=
  (∀ τ, QScheme.Inst σ τ → QTyped constTy Γ e τ) ∧
  (∃ τ, QScheme.Inst σ τ) ∧
  (∀ τ, QTyped constTy Γ e τ → ∃ τ', QScheme.Inst σ τ' ∧ τ' ≼ₜ τ)

-- ⊢  σ principal for e,  every σ''-instance a typing  ⟹  σ'' ⊴≼ σ
theorem QScheme.Principal.greatest {B C : Type} {constTy : C → B} {Γ : QCtx B}
    {e : Expr C} {σ σ'' : QScheme B} (hp : QScheme.Principal constTy Γ e σ)
    (hcl : ∀ τ, QScheme.Inst σ'' τ → QTyped constTy Γ e τ) :
    σ'' ⊴≼ σ :=
  fun τ hτ => hp.2.2 τ (hcl τ hτ)




--============ (4) λx.x.l HAS A PRINCIPAL QUALIFIED SCHEME ===================--
-- Conjunct 3 of `Principal`, in the corrected ≼ form — the open item that
-- `selQ_sound_and_inhabited` left. It needs the L2 inversion for λx.x.l, the
-- counterpart of minimal.lean's `sel_var_unk`: every typing of a selection on a
-- monotype-bound variable factors through ONE lookup, and the typing sits
-- ≼-above that lookup's collapse. `selQ_inst_of_lookup` then answers it with the
-- instance for exactly that lookup.

-- L2 counterpart of minimal.lean's `var_inst_inv`.
private theorem qvar_inst_inv {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    ∀ {x : Var} {τx : Ty B}, e = .var x → Γ.lookup x = some ⟨[], [], τx⟩ →
    τ = .unk ∨ TyEquiv τx τ
  | _, _, _, .qVar h hi => fun he hx => by
      cases he
      rw [h] at hx
      cases Option.some.inj hx
      rw [hi.mono]
      exact .inr (.refl _)
  | _, _, _, .qEq h heq => fun he hx =>
      match qvar_inst_inv h he hx with
      | .inl hu => .inl (hu ▸ heq).unk_inv
      | .inr ht => .inr (ht.trans heq)
  | _, _, _, .qUnk _ => fun _ _ => .inl rfl
  | _, _, _, .qCon => fun he _ => nomatch he
  | _, _, _, .qLam _ => fun he _ => nomatch he
  | _, _, _, .qApp _ _ => fun he _ => nomatch he
  | _, _, _, .qCat _ _ => fun he _ => nomatch he
  | _, _, _, .qSel _ _ => fun he _ => nomatch he
  | _, _, _, .qSelUnk _ _ => fun he _ => nomatch he
  | _, _, _, .qSelAbs _ _ => fun he _ => nomatch he
  | _, _, _, .qLet _ _ _ _ => fun he _ => nomatch he
  | _, _, _, .qRcd _ => fun he _ => nomatch he
  | _, _, _, .qLab => fun he _ => nomatch he
  | _, _, _, .qSelDyn _ _ _ => fun he _ => nomatch he
  | _, _, _, .qSelDynUnk _ _ _ => fun he _ => nomatch he
  | _, _, _, .qSelDynAbs _ _ _ => fun he _ => nomatch he

-- L2 counterpart of `sel_var_unk`, but full: every typing of `x.l` on a
-- monotype-bound x factors through ONE lookup, and the typing sits ≼-above
-- that lookup's collapse.
theorem qsel_var_inv {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    ∀ {x : Var} {l : Label} {τx : Ty B}, e = .sel (.var x) l →
    Γ.lookup x = some ⟨[], [], τx⟩ →
    ∃ ρ r, TyEquiv τx (.rcd ρ) ∧ Lookup ρ l r ∧ TyBelow r.collapse τ
  | _, _, _, .qSel h hl => fun he hx => by
      cases he
      rcases qvar_inst_inv h rfl hx with hu | ht
      · cases hu
      · exact ⟨_, _, ht, hl, TyBelow.refl _⟩
  | _, _, _, .qSelUnk h hl => fun he hx => by
      cases he
      rcases qvar_inst_inv h rfl hx with hu | ht
      · cases hu
      · exact ⟨_, .unknown, ht, hl, TyBelow.refl _⟩
  | _, _, _, .qSelAbs h hl => fun he hx => by
      cases he
      rcases qvar_inst_inv h rfl hx with hu | ht
      · cases hu
      · exact ⟨_, .absent, ht, hl, TyBelow.refl _⟩
  | _, _, _, .qEq h heq => fun he hx =>
      let ⟨ρ, r, ht, hl, hb⟩ := qsel_var_inv h he hx
      ⟨ρ, r, ht, hl, hb.trans (TyBelow.of_equiv heq)⟩
  | _, _, _, .qUnk h => fun he hx =>
      let ⟨ρ, r, ht, hl, _⟩ := qsel_var_inv h he hx
      ⟨ρ, r, ht, hl, TyBelow.of_prec (.unk _)⟩
  | _, _, _, .qCon => fun he _ => nomatch he
  | _, _, _, .qVar _ _ => fun he _ => nomatch he
  | _, _, _, .qLam _ => fun he _ => nomatch he
  | _, _, _, .qApp _ _ => fun he _ => nomatch he
  | _, _, _, .qCat _ _ => fun he _ => nomatch he
  | _, _, _, .qLet _ _ _ _ => fun he _ => nomatch he
  | _, _, _, .qRcd _ => fun he _ => nomatch he
  | _, _, _, .qLab => fun he _ => nomatch he
  | _, _, _, .qSelDyn _ _ _ => fun he _ => nomatch he
  | _, _, _, .qSelDynUnk _ _ _ => fun he _ => nomatch he
  | _, _, _, .qSelDynAbs _ _ _ => fun he _ => nomatch he

-- CONJUNCT 3, in the ≼ form.
theorem selQ_covers_typings {B C : Type} (constTy : C → B) :
    ∀ τ, QTyped constTy (QCtx.empty : QCtx B) (selEx C) τ →
      ∃ τ', QScheme.Inst (selQ B) τ' ∧ τ' ≼ₜ τ := by
  intro τ h
  obtain ⟨τ₁, τ₂, heq | hu, hbody⟩ := qtyped_lam_inv h
  · obtain ⟨ρ, r, hτ₁, hl, hb⟩ :=
      qsel_var_inv hbody (x := "x") (l := "l") (τx := τ₁) rfl
        (by simp [QCtx.bindTy, QCtx.lookup_bindScheme])
    obtain ⟨σ₀, hc, hp⟩ := hb
    exact ⟨_, selQ_inst_of_lookup hl,
      TyBelow.trans ⟨.fn τ₁ σ₀, .fn hτ₁.symm hc, .fn (.refl _) hp⟩
        (TyBelow.of_equiv heq)⟩
  · subst hu
    exact ⟨_, selQ_inst_absent, TyBelow.of_prec (.unk _)⟩

-- λx.x.l HAS a principal qualified scheme.
theorem selQ_principal {B C : Type} (constTy : C → B) :
    QScheme.Principal constTy (QCtx.empty : QCtx B) (selEx C) (selQ B) :=
  ⟨(selQ_sound_and_inhabited constTy).1, (selQ_sound_and_inhabited constTy).2,
   selQ_covers_typings constTy⟩

-- ... and therefore ⊴≼-greatest among every scheme that is sound for λx.x.l.
-- ⊢  (∀τ. σ ≥_∅ τ ⟹ ∅ ⊢_Q λx.x.l : τ)  ⟹  σ ⊴≼ selQ
theorem selQ_greatest {B C : Type} (constTy : C → B) {σ : QScheme B}
    (hcl : ∀ τ, QScheme.Inst σ τ →
      QTyped constTy (QCtx.empty : QCtx B) (selEx C) τ) :
    σ ⊴≼ selQ B :=
  (selQ_principal constTy).greatest hcl

--------------------- FIELD-FREE ROWS (what ≈ ε forces) ------------------------
-- A row that is ≈-equivalent to ε carries no field ANYWHERE: `≈` never creates
-- or destroys a `.sing` node, only permutes and re-brackets them. That is the
-- fact the mixed-instance construction needs — a field-free row has no TYPE
-- positions, so its substitution image cannot depend on the ty-component of the
-- substitution, and re-pointing a result variable leaves the domain alone.

/-- does the row carry a field at a position not hidden behind a row-variable? -/
def Row.hasSing {B : Type} : Row B → Bool
  | .empty     => false
  | .var _     => false
  | .sing _ _  => true
  | .cat ρ₁ ρ₂ => ρ₁.hasSing || ρ₂.hasSing

-- ⊢  ≈ᵣ preserves it: every constructor is a congruence, a re-bracketing or a
--    permutation of the same `.sing` nodes.
theorem rowEquiv_hasSing {B : Type} :
    {ρ₁ ρ₂ : Row B} → RowEquiv ρ₁ ρ₂ → ρ₁.hasSing = ρ₂.hasSing
  | _, _, .refl _        => rfl
  | _, _, .symm h        => (rowEquiv_hasSing h).symm
  | _, _, .trans h₁ h₂   => (rowEquiv_hasSing h₁).trans (rowEquiv_hasSing h₂)
  | _, _, .sing _        => rfl
  | _, _, .cat h₁ h₂     => by
      simp [Row.hasSing, rowEquiv_hasSing h₁, rowEquiv_hasSing h₂]
  | _, _, .assoc         => by simp [Row.hasSing, Bool.or_assoc]
  | _, _, .unitL         => by simp [Row.hasSing]
  | _, _, .unitR         => by simp [Row.hasSing]
  | _, _, .comm _        => by simp [Row.hasSing]

-- ⊢  substitution never DELETES a field: it maps `.sing` to `.sing`.
theorem hasSing_applySubst {B : Type} (θ : TySubst B) :
    (ρ : Row B) → ρ.hasSing = true → (ρ.applySubst θ).hasSing = true
  | .empty,     h => by simp [Row.hasSing] at h
  | .var _,     h => by simp [Row.hasSing] at h
  | .sing _ _,  _ => rfl
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.applySubst, Row.hasSing, Bool.or_eq_true] at h ⊢
      rcases h with h | h
      · exact .inl (hasSing_applySubst θ ρ₁ h)
      · exact .inr (hasSing_applySubst θ ρ₂ h)

-- ⊢  a field-free row has no TYPE position, so its image reads only θ.row.
theorem applySubst_rowOnly {B : Type} {θ₁ θ₂ : TySubst B} (hr : θ₁.row = θ₂.row) :
    (ρ : Row B) → ρ.hasSing = false → ρ.applySubst θ₁ = ρ.applySubst θ₂
  | .empty,     _ => rfl
  | .var α,     _ => by simp only [Row.applySubst, hr]
  | .sing _ _,  h => by simp [Row.hasSing] at h
  | .cat ρ₁ ρ₂, h => by
      simp only [Row.hasSing, Bool.or_eq_false_iff] at h
      simp only [Row.applySubst, applySubst_rowOnly hr ρ₁ h.1,
                 applySubst_rowOnly hr ρ₂ h.2]


--------------------- L1 INVERSION: let, app, and a general var ---------------
-- `typed_inv_aux` (minimal.lean) covers con/lam/rcd. Refuting the two-use
-- program needs the other three formers, in the same mod-≈/mod-★ shape.

private theorem typed_let_inv' {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    ∀ {x : Var} {e₁ e₂ : Expr C}, e = .letE x e₁ e₂ →
    ∃ (σ : Scheme B) (τ₂ : Ty B), (TyEquiv τ₂ τ ∨ τ = .unk) ∧
      (∀ τ', σ.Inst τ' → Typed constTy Γ e₁ τ') ∧
      Typed constTy (Γ.bindScheme x σ) e₂ τ₂
  | _, _, _, .tLet h hb => fun he => by cases he; exact ⟨_, _, .inl (.refl _), h, hb⟩
  | _, _, _, .tEq h heq => fun he =>
      match typed_let_inv' h he with
      | ⟨σ, τ₂, .inl ht, hc, hb⟩ => ⟨σ, τ₂, .inl (ht.trans heq), hc, hb⟩
      | ⟨σ, τ₂, .inr hu, hc, hb⟩ => ⟨σ, τ₂, .inr (hu ▸ heq).unk_inv, hc, hb⟩
  | _, _, _, .tUnk h => fun he =>
      match typed_let_inv' h he with
      | ⟨σ, τ₂, _, hc, hb⟩ => ⟨σ, τ₂, .inr rfl, hc, hb⟩
  | _, _, _, .tCon => fun he => nomatch he
  | _, _, _, .tVar _ _ => fun he => nomatch he
  | _, _, _, .tLam _ => fun he => nomatch he
  | _, _, _, .tApp _ _ => fun he => nomatch he
  | _, _, _, .tCat _ _ => fun he => nomatch he
  | _, _, _, .tSel _ _ => fun he => nomatch he
  | _, _, _, .tSelUnk _ _ => fun he => nomatch he
  | _, _, _, .tSelAbs _ _ => fun he => nomatch he
  | _, _, _, .tRcd _ => fun he => nomatch he

private theorem typed_app_inv' {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    ∀ {e₁ e₂ : Expr C}, e = .app e₁ e₂ →
    ∃ τ₁ τ₂, (TyEquiv τ₂ τ ∨ τ = .unk) ∧
      Typed constTy Γ e₁ (.fn τ₁ τ₂) ∧ Typed constTy Γ e₂ τ₁
  | _, _, _, .tApp h₁ h₂ => fun he => by
      cases he; exact ⟨_, _, .inl (.refl _), h₁, h₂⟩
  | _, _, _, .tEq h heq => fun he =>
      match typed_app_inv' h he with
      | ⟨τ₁, τ₂, .inl ht, h₁, h₂⟩ => ⟨τ₁, τ₂, .inl (ht.trans heq), h₁, h₂⟩
      | ⟨τ₁, τ₂, .inr hu, h₁, h₂⟩ => ⟨τ₁, τ₂, .inr (hu ▸ heq).unk_inv, h₁, h₂⟩
  | _, _, _, .tUnk h => fun he =>
      match typed_app_inv' h he with
      | ⟨τ₁, τ₂, _, h₁, h₂⟩ => ⟨τ₁, τ₂, .inr rfl, h₁, h₂⟩
  | _, _, _, .tCon => fun he => nomatch he
  | _, _, _, .tVar _ _ => fun he => nomatch he
  | _, _, _, .tLam _ => fun he => nomatch he
  | _, _, _, .tCat _ _ => fun he => nomatch he
  | _, _, _, .tSel _ _ => fun he => nomatch he
  | _, _, _, .tSelUnk _ _ => fun he => nomatch he
  | _, _, _, .tSelAbs _ _ => fun he => nomatch he
  | _, _, _, .tLet _ _ => fun he => nomatch he
  | _, _, _, .tRcd _ => fun he => nomatch he

-- `var_inst_inv` at a GENERAL scheme: a use of x reads some instance of the
-- scheme Γ binds it to, up to ≈ (and ★ from tUnk).
private theorem tvar_inv {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    ∀ {x : Var} {σ : Scheme B}, e = .var x → Γ.lookup x = some σ →
    τ = .unk ∨ ∃ τ', σ.Inst τ' ∧ TyEquiv τ' τ
  | _, _, _, .tVar h hi => fun he hx => by
      cases he
      rw [h] at hx
      cases Option.some.inj hx
      exact .inr ⟨_, hi, .refl _⟩
  | _, _, _, .tEq h heq => fun he hx =>
      match tvar_inv h he hx with
      | .inl hu => .inl (hu ▸ heq).unk_inv
      | .inr ⟨τ', hi, ht⟩ => .inr ⟨τ', hi, ht.trans heq⟩
  | _, _, _, .tUnk _ => fun _ _ => .inl rfl
  | _, _, _, .tCon => fun he _ => nomatch he
  | _, _, _, .tLam _ => fun he _ => nomatch he
  | _, _, _, .tApp _ _ => fun he _ => nomatch he
  | _, _, _, .tCat _ _ => fun he _ => nomatch he
  | _, _, _, .tSel _ _ => fun he _ => nomatch he
  | _, _, _, .tSelUnk _ _ => fun he _ => nomatch he
  | _, _, _, .tSelAbs _ _ => fun he _ => nomatch he
  | _, _, _, .tLet _ _ => fun he _ => nomatch he
  | _, _, _, .tRcd _ => fun he _ => nomatch he

-- A selection on a ★-bound variable has NO typing at all: every selection rule
-- demands the scrutinee at a record type, and ★ is ≈-rigid.
private theorem sel_var_of_unk {B C : Type} {constTy : C → B} :
    {Γ : Ctx B} → {e : Expr C} → {τ : Ty B} → Typed constTy Γ e τ →
    ∀ {x : Var} {l : Label}, e = .sel (.var x) l →
    Γ.lookup x = some ⟨[], .unk⟩ → False
  | _, _, _, .tSel h _ => fun he hx => by
      cases he
      rcases var_inst_inv h rfl hx with hu | ht
      · cases hu
      · cases ht.unk_inv
  | _, _, _, .tSelUnk h _ => fun he hx => by
      cases he
      rcases var_inst_inv h rfl hx with hu | ht
      · cases hu
      · cases ht.unk_inv
  | _, _, _, .tSelAbs h _ => fun he hx => by
      cases he
      rcases var_inst_inv h rfl hx with hu | ht
      · cases hu
      · cases ht.unk_inv
  | _, _, _, .tEq h _ => fun he hx => sel_var_of_unk h he hx
  | _, _, _, .tUnk h => fun he hx => sel_var_of_unk h he hx
  | _, _, _, .tCon => fun he _ => nomatch he
  | _, _, _, .tVar _ _ => fun he _ => nomatch he
  | _, _, _, .tLam _ => fun he _ => nomatch he
  | _, _, _, .tApp _ _ => fun he _ => nomatch he
  | _, _, _, .tCat _ _ => fun he _ => nomatch he
  | _, _, _, .tLet _ _ => fun he _ => nomatch he
  | _, _, _, .tRcd _ => fun he _ => nomatch he


------------- WHAT INSTANCE-CLOSEDNESS FORCES ON A SCHEME FOR λx.x.l ----------

-- A ★ domain admits NO typing: the body would be a selection on a ★-bound x.
private theorem selEx_dom_unk {B C : Type} (constTy : C → B) (R : Ty B) :
    ¬ Typed constTy Ctx.empty (selEx C) (.fn .unk R) := by
  intro h
  obtain ⟨τ₁, τ₂, hd, hbody⟩ := typed_lam_inv h
  rcases hd with heq | hu
  · obtain ⟨σ₁, σ₂, hsh, h₁, -⟩ := heq.fn_inv
    cases hsh
    cases h₁.symm.unk_inv
    exact sel_var_of_unk hbody rfl (by simp [Ctx.lookup_bindTy])
  · cases hu

-- An ε domain forces a ★ result — `sel_var_unk` read through the λ.
-- ⊢  D ≈ₜ {ε},  ∅ ⊢ λx.x.l : D → R   ⟹   R = ★
private theorem selEx_dom_empty_res {B C : Type} {constTy : C → B} {D R : Ty B}
    (hD : TyEquiv (.rcd .empty) D)
    (h : Typed constTy Ctx.empty (selEx C) (.fn D R)) : R = .unk := by
  obtain ⟨τ₁, τ₂, hd, hbody⟩ := typed_lam_inv h
  rcases hd with heq | hu
  · obtain ⟨σ₁, σ₂, hsh, h₁, h₂⟩ := heq.fn_inv
    cases hsh
    have hτ₁ : TyEquiv τ₁ (.rcd .empty) := h₁.trans hD.symm
    have : τ₂ = .unk :=
      sel_var_unk hbody rfl (by simp [Ctx.lookup_bindTy]) hτ₁
    exact (this ▸ h₂).unk_inv
  · cases hu

-- {ε} is not a typing of λx.x.l either (the head is wrong) — what a fully
-- quantified body would have to accept.
private theorem selEx_not_rcd_empty {B C : Type} (constTy : C → B) :
    ¬ Typed constTy Ctx.empty (selEx C) (.rcd (.empty : Row B)) := by
  intro h
  obtain ⟨τ₁, τ₂, hd, -⟩ := typed_lam_inv h
  rcases hd with heq | hu
  · obtain ⟨σ₁, σ₂, hsh, -, -⟩ := heq.fn_inv
    cases hsh
  · cases hu


--======================= L1 ⊊ L2: THE TWO-USE PROGRAM =======================--
-- `qtyped_two_use` types the program in L2. L1 CANNOT type it, and the reason
-- is exactly `no_plain_principal_scheme`'s: a plain scheme bound by `let` must
-- serve both uses, so its result position is a bare quantified variable, and
-- then the domain cannot depend on it — re-pointing that variable inside the
-- ⊥-use's substitution yields the underivable instance {ε} → 𝓫_c.

private theorem typedBody_two_fields {B C : Type} {constTy : C → B} {Γ : Ctx B}
    {la lb : Label} {ea eb : Expr C} {ρ : Row B}
    (h : TypedBody constTy Γ (.cat (.field la ea) (.field lb eb)) ρ) :
    ∃ τa τb, ρ = .cat (.sing la τa) (.sing lb τb) ∧
      Typed constTy Γ ea τa ∧ Typed constTy Γ eb τb := by
  cases h with
  | cat hA hB =>
    cases hA with
    | field ha =>
      cases hB with
      | field hb => exact ⟨_, _, rfl, ha, hb⟩

-- ⊢  ¬ ( ∅ ⊢ let f = λx.x.l in { a = f {l = c} | b = f {} }
--              : { a: 𝓫_c | b: ★ } )
theorem l1_rejects_two_use {B C : Type} (constTy : C → B) (c : C) :
    ¬ Typed constTy Ctx.empty
      (.letE "f" (selEx C)
        (.rcd (.cat
          (.field "a" (.app (.var "f") (.rcd (.field "l" (.con c)))))
          (.field "b" (.app (.var "f") (.rcd .empty))))))
      (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" .unk))) := by
  intro h
  obtain ⟨σ, τ₂, hd, hclosed, hbody⟩ := typed_let_inv' h rfl
  have heq : TyEquiv τ₂
      (.rcd (.cat (.sing "a" (.base (constTy c))) (.sing "b" (.unk : Ty B)))) := by
    rcases hd with h' | h'
    · exact h'
    · cases h'
  obtain ⟨ρ, hr, hbd⟩ := typed_rcd_inv hbody
  obtain ⟨τa, τb, hρeq, ha, hb⟩ := typedBody_two_fields hbd
  subst hρeq
  have hrow : RowEquiv (.cat (.sing "a" τa) (.sing "b" τb))
      (.cat (.sing "a" (.base (constTy c))) (.sing "b" (.unk : Ty B))) := by
    rcases hr with h' | h'
    · obtain ⟨ρ', hρ', hre⟩ := (h'.trans heq).rcd_inv
      injection hρ' with hρ''
      exact hρ'' ▸ hre
    · subst h'; cases heq.unk_inv
  -- ## the `a` use pins a DEFINITE result 𝓫_c
  have hta : TyEquiv τa (.base (constTy c)) := by
    obtain ⟨r₂, hr₂, hres⟩ :=
      lookup_equiv hrow (.catHit .hit)
    have hr₂' : r₂ = .found (.base (constTy c)) := lookup_det hr₂ (.catHit .hit)
    subst hr₂'
    cases hres with
    | found ht => exact ht
  obtain ⟨τ₁a, τ₂a, hda, hfa, -⟩ := typed_app_inv' ha rfl
  have heqa : TyEquiv τ₂a τa := by
    rcases hda with h' | h'
    · exact h'
    · subst h'; cases hta.unk_inv
  rcases tvar_inv hfa (x := "f") (σ := σ) rfl
    (by simp [Ctx.lookup_bindScheme]) with hu' | ⟨τ'a, hIa, hta'⟩
  · cases hu'
  obtain ⟨Aa, Ba, hsa, -, hBa⟩ := hta'.symm.fn_inv
  subst hsa
  have hBa' : Ba = .base (constTy c) :=
    ((hBa.symm.trans heqa).trans hta).symm.base_inv
  subst hBa'
  -- ## the `b` use forces an ε DOMAIN
  obtain ⟨τ₁b, τ₂b, -, hfb, hargb⟩ := typed_app_inv' hb rfl
  obtain ⟨ρb, hrb, hbe⟩ := typed_rcd_inv hargb
  cases hbe
  rcases tvar_inv hfb (x := "f") (σ := σ) rfl
    (by simp [Ctx.lookup_bindScheme]) with hu' | ⟨τ'b, hIb, htb'⟩
  · cases hu'
  obtain ⟨Ab, Bb, hsb, hAb, -⟩ := htb'.symm.fn_inv
  subst hsb
  have hAb' : TyEquiv (.rcd (.empty : Row B)) Ab := by
    rcases hrb with h' | h'
    · exact h'.trans hAb
    · subst h'
      have hAbu : Ab = .unk := hAb.unk_inv
      subst hAbu
      exact absurd (hclosed _ hIb) (selEx_dom_unk constTy Bb)
  obtain ⟨ρ', hρ'shape, hρ'e⟩ := hAb'.rcd_inv
  subst hρ'shape
  have hρ'free : ρ'.hasSing = false := by
    have hh := rowEquiv_hasSing hρ'e
    simpa [Row.hasSing] using hh.symm
  -- instance-closedness turns the ε domain into a ★ result
  have hBb : Bb = .unk :=
    selEx_dom_empty_res (TyEquiv.rcd hρ'e) (hclosed _ hIb)
  subst hBb
  -- ## the endgame: the result position is a bare binder, so mixing is legal
  obtain ⟨θa, -, hba⟩ := hIa
  obtain ⟨θb, hfixb, hbb⟩ := hIb
  cases hbodyσ : σ.body with
  | base b' => rw [hbodyσ] at hba; simp only [Ty.applySubst] at hba; cases hba
  | lab b' => rw [hbodyσ] at hba; simp only [Ty.applySubst] at hba; cases hba
  | unk     => rw [hbodyσ] at hba; simp only [Ty.applySubst] at hba; cases hba
  | rcd ρd  => rw [hbodyσ] at hba; simp only [Ty.applySubst] at hba; cases hba
  | var α   =>
      rw [hbodyσ] at hbb
      simp only [Ty.applySubst] at hbb
      by_cases hα : α ∈ σ.vars
      · refine selEx_not_rcd_empty constTy (hclosed _ ⟨⟨fun δ =>
          if δ = α then .rcd .empty else .var δ, fun δ => .var δ⟩,
          ⟨fun δ hδ => ?_, fun _ _ => rfl⟩, ?_⟩)
        · have hne : ¬δ = α := by rintro rfl; exact hδ hα
          simp [hne]
        · rw [hbodyσ]; simp [Ty.applySubst]
      · rw [hfixb.1 α hα] at hbb; cases hbb
  | fn dom res =>
      rw [hbodyσ] at hba hbb
      simp only [Ty.applySubst] at hba hbb
      injection hba with hda hra
      injection hbb with hdb hrb
      cases res with
      | base b' => simp only [Ty.applySubst] at hrb; cases hrb
      | lab b' => simp only [Ty.applySubst] at hrb; cases hrb
      | unk     => simp only [Ty.applySubst] at hra; cases hra
      | fn _ _  => simp only [Ty.applySubst] at hrb; cases hrb
      | rcd _   => simp only [Ty.applySubst] at hrb; cases hrb
      | var α   =>
          simp only [Ty.applySubst] at hra hrb
          have hα : α ∈ σ.vars := by
            by_cases hα : α ∈ σ.vars
            · exact hα
            · rw [hfixb.1 α hα] at hrb; cases hrb
          -- θb with the result variable re-pointed at 𝓫_c
          have hdm : dom.applySubst
              (⟨fun δ => if δ = α then .base (constTy c) else θb.ty δ, θb.row⟩ :
                TySubst B) = .rcd ρ' := by
            cases dom with
            | var γ =>
                simp only [Ty.applySubst] at hdb
                have hne : ¬γ = α := by
                  rintro rfl; rw [hdb] at hrb; cases hrb
                simpa [Ty.applySubst, hne] using hdb
            | rcd ρd =>
                simp only [Ty.applySubst] at hdb
                injection hdb with hρd
                have hfree : ρd.hasSing = false := by
                  cases hcon : ρd.hasSing with
                  | false => rfl
                  | true =>
                      have hc := hasSing_applySubst θb ρd hcon
                      rw [hρd] at hc
                      simp [hρ'free] at hc
                simp only [Ty.applySubst, Ty.rcd.injEq]
                rw [applySubst_rowOnly
                      (θ₁ := (⟨fun δ => if δ = α then .base (constTy c)
                                        else θb.ty δ, θb.row⟩ : TySubst B))
                      (θ₂ := θb) rfl ρd hfree, hρd]
            | base b' => simp only [Ty.applySubst] at hdb; cases hdb
            | lab b' => simp only [Ty.applySubst] at hdb; cases hdb
            | unk     => simp only [Ty.applySubst] at hdb; cases hdb
            | fn _ _  => simp only [Ty.applySubst] at hdb; cases hdb
          have hmix : σ.Inst (.fn (.rcd ρ') (.base (constTy c))) := by
            refine ⟨⟨fun δ => if δ = α then .base (constTy c) else θb.ty δ, θb.row⟩,
              ⟨fun δ hδ => ?_, fun δ hδ => hfixb.2 δ hδ⟩, ?_⟩
            · have hne : ¬δ = α := by rintro rfl; exact hδ hα
              simp [hne, hfixb.1 δ hδ]
            · rw [hbodyσ]
              simp [Ty.applySubst, hdm]
          have := selEx_dom_empty_res (TyEquiv.rcd hρ'e) (hclosed _ hmix)
          cases this

-- L1 ⊊ L2, mechanized: the two-use program is in L2 and not in L1.
theorem l1_strictly_weaker {B C : Type} (constTy : C → B) (c : C) :
    ∃ (e : Expr C) (τ : Ty B),
      QTyped constTy QCtx.empty e τ ∧ ¬ Typed constTy Ctx.empty e τ :=
  ⟨_, _, qtyped_two_use constTy c, l1_rejects_two_use constTy c⟩


--------------------- THE FC-LABEL TWIN OF selQ --------------------------------
-- The key of a selection is a VALUE, so a selector can take it as an argument:
-- λa. λx. x.(a). Its qualified scheme has a label variable as the stump's key —
-- two blockers, two sorts, one stump — and every instance is a typing, the three
-- discharge arms replaying T-sel-dyn / -⊥ / -★ exactly as `selQ_instance_closed`
-- replays T-sel. A non-label instance of α (say `int`) is no exception: L-junk
-- answers ⊥, discharge sends δ to ★, and T-sel-dyn-⊥ types it.

/-- λa. λx. x.(a) -/
def selDynEx (C : Type) : Expr C := .lam "a" (.lam "x" (.selDyn (.var "x") (.var "a")))

/-- ∀(α : Label)(β : Row)(δ : Type). ⟨β.α ↓ δ⟩ ⇒ α → {β} → δ -/
def selDynQ (B : Type) : QScheme B :=
  ⟨["α", "β", "δ"], [⟨.var "β", .var "α", .var "δ"⟩],
   .fn (.var "α") (.fn (.rcd (.var "β")) (.var "δ"))⟩

/-- ⊢  **every instance of `selDynQ` types λa. λx. x.(a)**, in any context. -/
theorem selDynQ_instance_closed {B C : Type} (constTy : C → B) (Γ : QCtx B) :
    ∀ τ, QScheme.Inst (selDynQ B) τ → QTyped constTy Γ (selDynEx C) τ := by
  rintro τ ⟨θ, -, hQ, hbody⟩
  simp only [selDynQ, Ty.applySubst] at hbody
  subst hbody
  have hs := hQ ⟨.var "β", .var "α", .var "δ"⟩ (by simp [selDynQ])
  -- the two λ-bound variables type at their annotations …
  have hx : QTyped constTy ((Γ.bindTy "a" (θ.ty "α")).bindTy "x" (.rcd (θ.row "β")))
      (.var "x" : Expr C) (.rcd (θ.row "β")) :=
    .qVar (σ := ⟨[], [], .rcd (θ.row "β")⟩)
      (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup])
      (QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], _⟩))
  have ha : QTyped constTy ((Γ.bindTy "a" (θ.ty "α")).bindTy "x" (.rcd (θ.row "β")))
      (.var "a" : Expr C) (θ.ty "α") :=
    .qVar (σ := ⟨[], [], θ.ty "α"⟩)
      (by simp [QCtx.bindTy, QCtx.bindScheme, QCtx.lookup])
      (QScheme.inst_toQ.mpr (Scheme.Inst.self ⟨[], _⟩))
  -- … and the discharge's lookup is the selection's, verbatim
  cases hs with
  | hit hl hδ =>
      rw [show θ.ty "δ" = _ from hδ]; exact .qLam (.qLam (.qSelDyn hx ha hl))
  | abs hl hδ =>
      rw [show θ.ty "δ" = _ from hδ]; exact .qLam (.qLam (.qSelDynAbs hx ha hl))
  | unk hl hδ =>
      rw [show θ.ty "δ" = _ from hδ]; exact .qLam (.qLam (.qSelDynUnk hx ha hl))

end MinimalCalculus
