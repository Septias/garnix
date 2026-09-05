-- Qualified schemes: stumps, discharge, discharge metatheory, the
-- principal qualified scheme of λx.x.l, and the QTyped relation over QCtx.
-- Independent of the row-unification algorithm (imports only `minimal`).

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
-- Γ ⊢ (θρ).l ↓ r  replayed per instantiation θ:
--
--   D-hit    r = τ_r  ⟹  θδ = τ_r    (the T-sel moment)
--   D-⊥      r = ⊥    ⟹  θδ = ★      (T-sel-⊥; W-flag on the algo side)
--   D-?      r = ?    ⟹  θδ = ★      (T-sel-★: still-unknown stays blurred;
--                                     algorithmically this case re-parks
--                                     instead — only finalization commits ★)

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

-- Instantiation-with-discharge  σ ≥_Γ τ
-- Replaces Scheme.Inst at (a future qualified) T-var. Γ-relative — the price
-- of cross-instantiation refinement: discharge reads Γ's row-solutions.

def QScheme.Inst {B : Type} (Γ : Ctx B) (σ : QScheme B) (τ : Ty B) : Prop :=
  ∃ θ : TySubst B, θ.FixedOutside σ.vars ∧
    (∀ s ∈ σ.constraints, s.Discharge Γ θ) ∧
    σ.body.applySubst θ = τ

-- Plain schemes embed: with Q = ∅ the discharge condition is vacuous and
-- ≥_Γ degenerates to the Γ-independent Scheme.Inst. This is the seam between
-- the two sysstems (decl. & alg.) — everything minimal.lean knows about plain
-- schemes lifts across this equivalence.
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
-- ⊢ Γ ⊢ ρ.l ↓ r          ⟹  selQ ≥_Γ ({ρ} → collapse r)
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
-- pins δ at ★, never at {ε}. *Discharge is exactly the mechanism that plugs the instance-closedness leak.*
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
--              ∧ σ ≥_∅ ({l: {}} → {}) ∧ σ ≥_∅ ({} → ★)
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

-- ⊢  (Γ, x:σ).ctx = Γ.ctx    and    (Γ, x:τ).ctx = Γ.ctx
theorem ctx_bindScheme (Γ : QCtx B) (x : Var) (σ : QScheme B) :
    (Γ.bindScheme x σ).ctx = Γ.ctx := rfl

theorem ctx_bindTy (Γ : QCtx B) (x : Var) (τ : Ty B) :
    (Γ.bindTy x τ).ctx = Γ.ctx := rfl

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
    -- The INHABITATION premise is not bureaucracy: with Q ≠ ∅ a scheme can have
    -- NO Γ-instance, and then the instance-closed premise says nothing at all
    -- about e₁ — `let x = (3 4) in 5` would type while being stuck, and progress
    -- would be false. Plain schemes satisfy it by Scheme.Inst.self; the solver
    -- satisfies it by construction (a parked stump discharges at ★ if nothing
    -- better), so this is the declarative shadow of "stumps always finalize".
    | qLet : (∀ τ₁, QScheme.Inst Γ.ctx σ τ₁ → QTyped constTy Γ e₁ τ₁) →
             (∃ τ₁, QScheme.Inst Γ.ctx σ τ₁) →
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
  | _, _, _, .qLet _ _ _ =>
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

-- Embedding: every declarative typing is an L2 typing
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
            ⟨_, QScheme.inst_toQ.mpr (Scheme.Inst.self _)⟩
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
  refine .qLet (σ := selQ B) (fun τ₁ hq => ?_) ⟨_, selQ_inst_absent _⟩ ?_
  · exact (selQ_instance_closed constTy _ τ₁ hq).toQ
  · refine .qRcd (.cat (.field ?_) (.field ?_))
    · exact .qApp
        (.qVar (by simp [QCtx.lookup_bindScheme])
               (selQ_inst_found _ (.base (constTy c))))
        (.qRcd (.field .qCon))
    · exact .qApp
        (.qVar (by simp [QCtx.lookup_bindScheme]) (selQ_inst_absent _))
        (.qRcd .empty)


--======================= L2 METATHEORY: SUBSTITUTION ========================--
-- Progress and preservation for the qualified system. Step/Value/Err/Progress
-- are reused verbatim from minimal — only the typing relation changed — so every
-- lemma here tracks its minimal.lean twin rule-for-rule. The one new ingredient
-- is that qVar/qLet instantiate Γ-RELATIVELY (QScheme.Inst Γ.ctx), so weakening
-- must transport those premises across binders; ctx_bindScheme/ctx_bindTy make
-- that free (binding never touches rowEnv, hence never touches Γ.ctx).

-- Discharge sees Γ only through `Lookup Γ …`, which consults rowEnv alone.
theorem Stump.Discharge.congr_rowEnv {B : Type} {Γ₁ Γ₂ : Ctx B} {θ : TySubst B}
    {s : Stump B} (hrow : ∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α)
    (h : s.Discharge Γ₁ θ) : s.Discharge Γ₂ θ := by
  cases h with
  | hit hl hδ => exact .hit (Lookup.congr_rowEnv hrow hl) hδ
  | abs hl hδ => exact .abs (Lookup.congr_rowEnv hrow hl) hδ
  | unk hl hδ => exact .unk (Lookup.congr_rowEnv hrow hl) hδ

-- …hence so does instantiation-with-discharge: it is a function of Γ.rowEnv only.
theorem QScheme.Inst.congr_rowEnv {B : Type} {Γ₁ Γ₂ : Ctx B} {σ : QScheme B}
    {τ : Ty B} (hrow : ∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α)
    (h : QScheme.Inst Γ₁ σ τ) : QScheme.Inst Γ₂ σ τ := by
  obtain ⟨θ, hfix, hQ, hbody⟩ := h
  exact ⟨θ, hfix, fun s hs => (hQ s hs).congr_rowEnv hrow, hbody⟩

-- ## The QCtx weakening preorder  Γ₁ ⊑ Γ₂
-- Tracks Ctx.Sub: term-lookups only grow, row-solutions (read through .ctx)
-- agree on the nose. Subsumes weakening, exchange and shadowing.
def QCtx.Sub {B : Type} (Γ₁ Γ₂ : QCtx B) : Prop :=
  (∀ x σ, Γ₁.lookup x = some σ → Γ₂.lookup x = some σ) ∧
  (∀ α, Γ₁.ctx.lookupRow α = Γ₂.ctx.lookupRow α)

theorem QCtx.Sub.refl {B : Type} (Γ : QCtx B) : QCtx.Sub Γ Γ :=
  ⟨fun _ _ h => h, fun _ => rfl⟩

theorem QCtx.Sub.trans {B : Type} {Γ₁ Γ₂ Γ₃ : QCtx B}
    (h₁ : QCtx.Sub Γ₁ Γ₂) (h₂ : QCtx.Sub Γ₂ Γ₃) : QCtx.Sub Γ₁ Γ₃ :=
  ⟨fun x τ h => h₂.1 x τ (h₁.1 x τ h), fun α => (h₁.2 α).trans (h₂.2 α)⟩

-- Binding respects the preorder — and leaves the row-view untouched (ctx_bind*).
theorem QCtx.Sub.bindScheme {B : Type} {Γ₁ Γ₂ : QCtx B} (h : QCtx.Sub Γ₁ Γ₂)
    (x : Var) (σ : QScheme B) :
    QCtx.Sub (Γ₁.bindScheme x σ) (Γ₂.bindScheme x σ) := by
  refine ⟨fun y σ' hy => ?_, h.2⟩
  rw [QCtx.lookup_bindScheme] at hy ⊢
  cases hxy : (x == y)
  · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
    exact h.1 y σ' hy
  · simpa [hxy] using hy

theorem QCtx.Sub.bindTy {B : Type} {Γ₁ Γ₂ : QCtx B} (h : QCtx.Sub Γ₁ Γ₂)
    (x : Var) (τ : Ty B) : QCtx.Sub (Γ₁.bindTy x τ) (Γ₂.bindTy x τ) :=
  h.bindScheme x ⟨[], [], τ⟩

theorem QCtx.Sub.exchange {B : Type} (Γ : QCtx B) {x y : Var} (hne : x ≠ y)
    (σ₁ σ₂ : QScheme B) :
    QCtx.Sub ((Γ.bindScheme x σ₁).bindScheme y σ₂)
             ((Γ.bindScheme y σ₂).bindScheme x σ₁) := by
  refine ⟨fun z μ hz => ?_, fun _ => rfl⟩
  simp only [QCtx.lookup_bindScheme] at hz ⊢
  cases hyz : (y == z) <;> cases hxz : (x == z) <;>
    simp only [hyz, hxz, Bool.false_eq_true, if_false, if_true] at hz ⊢ <;>
    try exact hz
  exact absurd ((eq_of_beq hxz).trans (eq_of_beq hyz).symm) hne

theorem QCtx.Sub.shadowed {B : Type} {Δ Γ : QCtx B} {x : Var} {σ₁ : QScheme B}
    (h : QCtx.Sub Δ (Γ.bindScheme x σ₁)) (σ : QScheme B) :
    QCtx.Sub (Δ.bindScheme x σ) (Γ.bindScheme x σ) := by
  refine ⟨fun z μ hz => ?_, fun α => h.2 α⟩
  rw [QCtx.lookup_bindScheme] at hz ⊢
  cases hxz : (x == z)
  · simp only [hxz, Bool.false_eq_true, if_false] at hz ⊢
    have := h.1 z μ hz
    rwa [QCtx.lookup_bindScheme, hxz, if_neg (by simp)] at this
  · simpa [hxz] using hz

-- A closed term (empty tyEnv) types in any context over the same row-solutions.
theorem QCtx.Sub.ofEmptyTyEnv {B : Type} (Γ : QCtx B) :
    QCtx.Sub ⟨[], Γ.rowEnv⟩ Γ :=
  ⟨fun x τ h => by simp [QCtx.lookup] at h, fun _ => rfl⟩

-- ## Typing transports along ⊑ (mutual over QTyped/QTypedBody)
-- The qVar/qLet Inst premises and the qSel lookups ride across on the row-view
-- congruences above; everything else is a plain constructor rebuild.
mutual
theorem qtyped_sub {B C : Type} {constTy : C → B} :
    {Γ₁ Γ₂ : QCtx B} → {e : Expr C} → {τ : Ty B} → QCtx.Sub Γ₁ Γ₂ →
    QTyped constTy Γ₁ e τ → QTyped constTy Γ₂ e τ
  | _, _, _, _, _,  .qCon         => .qCon
  | _, _, _, _, hs, .qVar h hi    => .qVar (hs.1 _ _ h) (hi.congr_rowEnv hs.2)
  | _, _, _, _, hs, .qEq h heq    => .qEq (qtyped_sub hs h) heq
  | _, _, _, _, hs, .qLam h       => .qLam (qtyped_sub (hs.bindTy _ _) h)
  | _, _, _, _, hs, .qApp h₁ h₂   => .qApp (qtyped_sub hs h₁) (qtyped_sub hs h₂)
  | _, _, _, _, hs, .qCat h₁ h₂   => .qCat (qtyped_sub hs h₁) (qtyped_sub hs h₂)
  | _, _, _, _, hs, .qSel h hl    =>
      .qSel (qtyped_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .qSelUnk h hl =>
      .qSelUnk (qtyped_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .qSelAbs h hl =>
      .qSelAbs (qtyped_sub hs h) (Lookup.congr_rowEnv hs.2 hl)
  | _, _, _, _, hs, .qUnk h       => .qUnk (qtyped_sub hs h)
  | _, _, _, _, hs, .qLet h₁ hne h₂   =>
      .qLet (fun τ' hi =>
              qtyped_sub hs (h₁ τ' (hi.congr_rowEnv (fun α => (hs.2 α).symm))))
            (let ⟨τ', hi⟩ := hne; ⟨τ', hi.congr_rowEnv hs.2⟩)
            (qtyped_sub (hs.bindScheme _ _) h₂)
  | _, _, _, _, hs, .qRcd h       => .qRcd (qtypedBody_sub hs h)

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
-- premise qLet supplies at let-β. Verbatim subst_aux one level up; the qVar and
-- qLet-premise cases add a row-view congruence to move Inst between Δ.ctx and
-- Γ.ctx (defeq, since binding leaves rowEnv fixed).
mutual
private theorem qsubst_aux {B C : Type} {constTy : C → B} :
    {Δ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Δ e τ →
    ∀ {Γ : QCtx B} {x : Var} {v : Expr C} {σ : QScheme B},
      QCtx.Sub Δ (Γ.bindScheme x σ) →
      (∀ τ', QScheme.Inst Γ.ctx σ τ' → QTyped constTy ⟨[], Γ.rowEnv⟩ v τ') →
      QTyped constTy Γ (subst x v e) τ
  | _, _, _, .qCon, _, _, _, _, _, _ => .qCon
  | _, .var y, _, .qVar h hi, _, x, _, _, hsub, hv => by
      have hy := hsub.1 _ _ h
      rw [QCtx.lookup_bindScheme] at hy
      simp only [subst]
      cases hxy : (x == y)
      · simp only [hxy, Bool.false_eq_true, if_false] at hy ⊢
        exact .qVar hy (hi.congr_rowEnv hsub.2)
      · simp only [hxy, if_true] at hy ⊢
        cases Option.some.inj hy
        exact qtyped_sub (QCtx.Sub.ofEmptyTyEnv _) (hv _ (hi.congr_rowEnv hsub.2))
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
      .qSel (qsubst_aux h hsub hv) (Lookup.congr_rowEnv (fun α => hsub.2 α) hl)
  | _, _, _, .qSelUnk h hl, _, _, _, _, hsub, hv =>
      .qSelUnk (qsubst_aux h hsub hv) (Lookup.congr_rowEnv (fun α => hsub.2 α) hl)
  | _, _, _, .qSelAbs h hl, _, _, _, _, hsub, hv =>
      .qSelAbs (qsubst_aux h hsub hv) (Lookup.congr_rowEnv (fun α => hsub.2 α) hl)
  | _, _, _, .qUnk h, _, _, _, _, hsub, hv =>
      .qUnk (qsubst_aux h hsub hv)
  | _, .letE y e₁ e₂, _, .qLet h₁ hne h₂, _, x, _, _, hsub, hv => by
      simp only [subst]
      cases hxy : (x == y)
      · simp only [Bool.false_eq_true, if_false]
        exact .qLet
          (fun τ' hi =>
            qsubst_aux (h₁ τ' (hi.congr_rowEnv (fun α => (hsub.2 α).symm))) hsub hv)
          (let ⟨τ', hi⟩ := hne; ⟨τ', hi.congr_rowEnv (fun α => hsub.2 α)⟩)
          (qsubst_aux h₂
            ((hsub.bindScheme _ _).trans
              (QCtx.Sub.exchange _ (by simpa using hxy) _ _)) hv)
      · simp only [if_true]
        exact .qLet
          (fun τ' hi =>
            qsubst_aux (h₁ τ' (hi.congr_rowEnv (fun α => (hsub.2 α).symm))) hsub hv)
          (let ⟨τ', hi⟩ := hne; ⟨τ', hi.congr_rowEnv (fun α => hsub.2 α)⟩)
          (qtyped_sub ((eq_of_beq hxy) ▸ hsub.shadowed _) h₂)
  | _, _, _, .qRcd h, _, _, _, _, hsub, hv =>
      .qRcd (qsubstBody_aux h hsub hv)

private theorem qsubstBody_aux {B C : Type} {constTy : C → B} :
    {Δ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    QTypedBody constTy Δ b ρ →
    ∀ {Γ : QCtx B} {x : Var} {v : Expr C} {σ : QScheme B},
      QCtx.Sub Δ (Γ.bindScheme x σ) →
      (∀ τ', QScheme.Inst Γ.ctx σ τ' → QTyped constTy ⟨[], Γ.rowEnv⟩ v τ') →
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
    (hv : ∀ τ', QScheme.Inst Γ.ctx σ τ' → QTyped constTy ⟨[], Γ.rowEnv⟩ v τ')
    (he : QTyped constTy (Γ.bindScheme x σ) e τ₂) :
    QTyped constTy Γ (subst x v e) τ₂ :=
  qsubst_aux he (QCtx.Sub.refl _) hv

-- Monotype-bound variables (λ): the singleton case.
theorem qsubst_preserves_typing
    {B C : Type} (constTy : C → B)
    (Γ : QCtx B) (x : Var) (v : Expr C) (τ₁ τ₂ : Ty B) (e : Expr C)
    (hv : QTyped constTy ⟨[], Γ.rowEnv⟩ v τ₁)
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
    ∀ {l : Label}, Lookup Γ.ctx ρ l .absent → RecBody.lookup l b = none
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
    ∀ {l : Label} {τ : Ty B}, Lookup Γ.ctx ρ l (.found τ) →
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

-- ## Preservation  (closed programs)
--   If ⊢_Q e : τ  and  e → e'  then  ⊢_Q e' : τ.
-- Stated at the empty QCtx so the substitution lemmas' closed-value premise
-- lines up (β needs a closed argument). qApp/qLet-β route through Phase 2;
-- the qSel* cases reuse minimal's lookup-across-≈ᵣ helpers on Γ.ctx unchanged.
private theorem qpreservation_aux {B C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} → QTyped constTy Γ e τ →
    Γ = ⟨[], []⟩ → ∀ {e' : Expr C}, Step e e' → QTyped constTy Γ e' τ
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
  | _, _, _, .qLet h₁ hne h₂, hΓ, _ => fun hs => by
      cases hs with
      | letCong s =>
          exact .qLet (fun τ' hi => qpreservation_aux (h₁ τ' hi) hΓ s) hne h₂
      | letBeta hval =>
          subst hΓ
          exact qsubst_scheme_preserves_typing _ _ _ _ _ _ _
            (fun τ' hi => h₁ τ' hi) h₂

--========================= L2 METATHEORY: PROGRESS ==========================--
--   If ⊢_Q e : τ  then  e ∈ Value  ∨  (∃ e', e → e')  ∨  e ↯
-- Step/Value/Err/Progress are reused verbatim from minimal — only the typing
-- relation changed — so this tracks `progress` rule-for-rule. Two differences,
-- both real:
--   * only tyEnv must be empty. Γ's ROW-solutions stay available, because L2
--     lookups (qSel*) read them; L1 could demand the whole context empty since
--     nothing consulted rowEnv at ∅.
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
          -- the field off the body (Γ.ctx keeps the row-solutions)
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
  | .qLet h₁ hne h₂ =>
      match qprogress hΓ (h₁ _ hne.choose_spec) with
      | .step s  => .step (.letCong s)
      | .err er  => .err (.letBind er)
      | .done v₁ => .step (.letBeta v₁)

-- ⊢  ⊢_Q e : τ   ⟹   e is a value, steps, or is a lookup-error
theorem qProgress {B C : Type} (constTy : C → B) (e : Expr C) (τ : Ty B)
    (ht : QTyped constTy ⟨[], []⟩ e τ) : Progress e :=
  qprogress rfl ht


theorem qPreservation
    {B C : Type} (constTy : C → B)
    (e e' : Expr C) (τ : Ty B)
    (ht : QTyped constTy ⟨[], []⟩ e τ)
    (hs : Step e e') :
    QTyped constTy ⟨[], []⟩ e' τ :=
  qpreservation_aux ht rfl hs

end MinimalCalculus
