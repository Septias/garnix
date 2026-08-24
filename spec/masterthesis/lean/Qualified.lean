-- Qualified schemes (L2): stumps, discharge, discharge metatheory, the
-- principal qualified scheme of λx.x.l, and the L2 QTyped relation over QCtx.
-- Independent of the row-unification algorithm (imports only `minimal`).
-- Split out of the former monolithic algorithmic.lean (see proof-state.md).

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

end MinimalCalculus
