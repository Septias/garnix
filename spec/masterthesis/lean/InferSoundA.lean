-- THE SOUNDNESS STATEMENT, RESTATED.
--
-- `InferSoundC` is false (`inferSoundC_false`, LetSound.lean): it read Γ under
-- ⟦S′⟧ and τ under σ. Fixing only that is not enough, and this file is the
-- three changes the fix turned out to need.
--
--   1. Γ IS READ UNDER σ, with the row environment discharged: `CtxRead σ Γ Γ′`.
--      Every step lemma already assumed `Γ'.rowEnv = []`, so the per-rule work
--      carries over.
--   2. A SCHEME ENTERS Γ′ WITH ITS BINDERS RENAMED (`QScheme.renameBinders`),
--      so that `Avoiding` holds by construction rather than by luck. This is the
--      L2 counterpart of L1's `renameScheme`, and what A-var's forward covering
--      (`QCovers.forward_of_avoiding`) consumes.
--   3. ASSUMPTIONS ARE TYPED, NOT PROMISES. `QTypedC` kept a stump's result
--      variable RAW and needed "σ has no opinion at δ" (`hδ`). That hypothesis
--      is not monotone: a stump parked by e₁ can be woken by a later equation,
--      and then σ ⊨ S′ DOES have an opinion at δ, so the IH for e₁ is unusable
--      at the σ the conclusion is stated at. Here an assumption is a lookup
--      whose answer is assumed — `Assume` = ρ.l ↓ τ with τ a TYPE — and a
--      parked stump contributes `p.stump.at σ`, its result read under σ too.
--      Then a woken stump's assumption simply HOLDS (`Assume.Holds`), and
--      `QTypedA.weaken` removes held assumptions. That one lemma is both the
--      monotonicity the induction needs and the cash-in `QTypedCDischarge` was
--      meant to be — there is no χ-transport left in it.
--      A-var's constraints may be assumed as well (`QScheme.InstA`): an
--      instantiated stump that stays parked has no discharge yet, and `qVar`
--      now accepts it as an assumption instead.

import ParkedInv
import Absorb

namespace MinimalCalculus

--------------------- 3. ASSUMED LOOKUPS -------------------------------------

/-- `ρ.l ↓ τ` assumed: `e : {ρ}` is taken to give `e.l : τ`. -/
structure Assume (B : Type) where
  row   : Row B
  label : Label
  ty    : Ty B

/-- a stump read under σ — row AND result. -/
def Stump.at {B : Type} (σ : TySubst B) (st : Stump B) : Assume B :=
  ⟨st.row.applySubst σ, st.label, σ.ty st.res⟩

/-- the assumption is true of Γ: the lookup, performed, agrees — found up to ≈,
or ⊥/? at ★. `Stump.DischargeEquiv` is exactly this at `st.at θ`. -/
inductive Assume.Holds {B : Type} (Γ : Ctx B) (a : Assume B) : Prop where
  | hit {τ : Ty B} : Lookup Γ a.row a.label (.found τ) → TyEquiv a.ty τ → Holds Γ a
  | abs : Lookup Γ a.row a.label .absent → a.ty = .unk → Holds Γ a
  | unk : Lookup Γ a.row a.label .unknown → a.ty = .unk → Holds Γ a

-- ⊢  a stump discharges at θ (up to ≈) iff its θ-reading holds
theorem Stump.dischargeEquiv_iff_holds {B : Type} {Γ : Ctx B} {θ : TySubst B}
    {st : Stump B} : st.DischargeEquiv Γ θ ↔ (st.at θ).Holds Γ :=
  ⟨fun | .hit h e => .hit h e | .abs h e => .abs h e | .unk h e => .unk h e,
   fun | .hit h e => .hit h e | .abs h e => .abs h e | .unk h e => .unk h e⟩

/-- instantiation under assumptions: every constraint discharges (up to ≈) or
its reading is assumed. -/
def QScheme.InstA {B : Type} (Δ : List (Assume B)) (Γ : Ctx B) (sc : QScheme B)
    (τ : Ty B) : Prop :=
  ∃ χ : TySubst B, χ.FixedOutside sc.vars ∧
    (∀ st ∈ sc.constraints, st.DischargeEquiv Γ χ ∨ st.at χ ∈ Δ) ∧
    sc.body.applySubst χ = τ

/-- a scheme whose constraints can be discharged ONE AT A TIME: result variables
bound, one constraint per result variable, and no constraint's row mentions any
result variable. Then changing an instance at the result variables changes no
lookup (`QScheme.Correctable.correct`). A-let builds only such schemes. -/
def QScheme.Correctable {B : Type} (sc : QScheme B) : Prop :=
  sc.WF ∧
  (∀ a ∈ sc.constraints, ∀ b ∈ sc.constraints, a.res = b.res → a = b) ∧
  (∀ a ∈ sc.constraints, ∀ b ∈ sc.constraints, b.res ∉ a.row.ftv)

mutual
  /-- `Δ; Γ ⊢ e : τ` — L2 typing under assumed lookups. Δ is an INDEX: qLet's
  premise extends it with the scheme's own constraints, read at the instance. -/
  inductive QTypedA {B C : Type} (constTy : C → B) :
      List (Assume B) → QCtx B → Expr C → Ty B → Prop where
    | qCon : QTypedA constTy Δ Γ (.con c) (.base (constTy c))
    | qVar : Γ.lookup x = some σ → QScheme.InstA Δ Γ.ctx σ τ →
             QTypedA constTy Δ Γ (.var x) τ
    | qEq  : QTypedA constTy Δ Γ e τ₁ → TyEquiv τ₁ τ₂ → QTypedA constTy Δ Γ e τ₂
    | qLam : QTypedA constTy Δ (Γ.bindTy x τ₁) e τ₂ →
             QTypedA constTy Δ Γ (.lam x e) (.fn τ₁ τ₂)
    | qApp : QTypedA constTy Δ Γ e₁ (.fn τ₁ τ₂) → QTypedA constTy Δ Γ e₂ τ₁ →
             QTypedA constTy Δ Γ (.app e₁ e₂) τ₂
    -- e₁ at EVERY instance, assuming the constraints there. That is what
    -- generalization produces; which instances discharge is decided at the use.
    | qLet : σ.Correctable →
             (∀ χ : TySubst B, χ.FixedOutside σ.vars →
               QTypedA constTy (σ.constraints.map (Stump.at χ) ++ Δ) Γ e₁
                 (σ.body.applySubst χ)) →
             (∃ τ₁, QScheme.Inst Γ.ctx σ τ₁) →
             QTypedA constTy Δ (Γ.bindScheme x σ) e₂ τ₂ →
             QTypedA constTy Δ Γ (.letE x e₁ e₂) τ₂
    | qCat : QTypedA constTy Δ Γ e₁ (.rcd ρ₁) → QTypedA constTy Δ Γ e₂ (.rcd ρ₂) →
             QTypedA constTy Δ Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))
    | qSel : QTypedA constTy Δ Γ e (.rcd ρ) → Lookup Γ.ctx ρ l (.found τ) →
             QTypedA constTy Δ Γ (.sel e l) τ
    | qSelUnk : QTypedA constTy Δ Γ e (.rcd ρ) → Lookup Γ.ctx ρ l .unknown →
                QTypedA constTy Δ Γ (.sel e l) .unk
    | qSelAbs : QTypedA constTy Δ Γ e (.rcd ρ) → Lookup Γ.ctx ρ l .absent →
                QTypedA constTy Δ Γ (.sel e l) .unk
    | qUnk : QTypedA constTy Δ Γ e τ → QTypedA constTy Δ Γ e .unk
    | qRcd : QTypedABody constTy Δ Γ b ρ → QTypedA constTy Δ Γ (.rcd b) (.rcd ρ)
    -- A-sel-? read declaratively: the lookup's answer is assumed
    | assume {ρ : Row B} {l : Label} {τ : Ty B} :
             QTypedA constTy Δ Γ e (.rcd ρ) → (⟨ρ, l, τ⟩ : Assume B) ∈ Δ →
             QTypedA constTy Δ Γ (.sel e l) τ

  inductive QTypedABody {B C : Type} (constTy : C → B) :
      List (Assume B) → QCtx B → RecBody (Expr C) → Row B → Prop where
    | empty : QTypedABody constTy Δ Γ .empty .empty
    | field : QTypedA constTy Δ Γ e τ →
              QTypedABody constTy Δ Γ (.field l e) (.sing l τ)
    | cat : QTypedABody constTy Δ Γ b₁ ρ₁ → QTypedABody constTy Δ Γ b₂ ρ₂ →
            QTypedABody constTy Δ Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)
end

-- a held assumption, used at a selection, is one of the three T-sel rules
private theorem sel_of_holds {B C : Type} {constTy : C → B} {Δ : List (Assume B)}
    {Γ : QCtx B} {e : Expr C} {ρ : Row B} {l : Label} {τ : Ty B}
    (h : QTypedA constTy Δ Γ e (.rcd ρ)) (hh : (⟨ρ, l, τ⟩ : Assume B).Holds Γ.ctx) :
    QTypedA constTy Δ Γ (.sel e l) τ := by
  cases hh with
  | hit hl he => exact .qEq (.qSel h hl) he.symm
  | abs hl he => cases he; exact .qSelAbs h hl
  | unk hl he => cases he; exact .qSelUnk h hl

mutual
  /-- ⊢  **held assumptions can be dropped.** Monotonicity for the induction —
  an assumption from an earlier state is either still parked or has been woken,
  and a woken one holds — and, at Δ₂ = [], the cash-in at the end of a run. -/
  theorem QTypedA.weaken {B C : Type} {constTy : C → B} :
      {Δ₁ Δ₂ : List (Assume B)} → {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} →
      QTypedA constTy Δ₁ Γ e τ →
      (∀ a ∈ Δ₁, a ∈ Δ₂ ∨ a.Holds Γ.ctx) → QTypedA constTy Δ₂ Γ e τ
    | _, _, _, _, _, .qCon, _ => .qCon
    | _, _, _, _, _, .qVar hl ⟨χ, hfix, hc, hb⟩, hΔ =>
        .qVar hl ⟨χ, hfix, fun st hst =>
          match hc st hst with
          | .inl hd => .inl hd
          | .inr hm => match hΔ _ hm with
            | .inl hm' => .inr hm'
            | .inr hh  => .inl (Stump.dischargeEquiv_iff_holds.mpr hh), hb⟩
    | _, _, _, _, _, .qEq h he, hΔ => .qEq (QTypedA.weaken h hΔ) he
    | _, _, _, _, _, .qLam h, hΔ => .qLam (QTypedA.weaken h hΔ)
    | _, _, _, _, _, .qApp h₁ h₂, hΔ =>
        .qApp (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ)
    | _, _, _, _, _, .qLet hcs hi hin hb, hΔ =>
        .qLet hcs (fun χ hfix => QTypedA.weaken (hi χ hfix) (fun a ha =>
            match List.mem_append.mp ha with
            | .inl hc => .inl (List.mem_append_left _ hc)
            | .inr hd => match hΔ a hd with
              | .inl hm => .inl (List.mem_append_right _ hm)
              | .inr hh => .inr hh))
          hin (QTypedA.weaken hb hΔ)
    | _, _, _, _, _, .qCat h₁ h₂, hΔ =>
        .qCat (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ)
    | _, _, _, _, _, .qSel h hl, hΔ => .qSel (QTypedA.weaken h hΔ) hl
    | _, _, _, _, _, .qSelUnk h hl, hΔ => .qSelUnk (QTypedA.weaken h hΔ) hl
    | _, _, _, _, _, .qSelAbs h hl, hΔ => .qSelAbs (QTypedA.weaken h hΔ) hl
    | _, _, _, _, _, .qUnk h, hΔ => .qUnk (QTypedA.weaken h hΔ)
    | _, _, _, _, _, .qRcd h, hΔ => .qRcd (QTypedABody.weaken h hΔ)
    | _, _, _, _, _, .assume h hm, hΔ =>
        match hΔ _ hm with
        | .inl hm' => .assume (QTypedA.weaken h hΔ) hm'
        | .inr hh  => sel_of_holds (QTypedA.weaken h hΔ) hh

  theorem QTypedABody.weaken {B C : Type} {constTy : C → B} :
      {Δ₁ Δ₂ : List (Assume B)} → {Γ : QCtx B} → {b : RecBody (Expr C)} →
      {ρ : Row B} → QTypedABody constTy Δ₁ Γ b ρ →
      (∀ a ∈ Δ₁, a ∈ Δ₂ ∨ a.Holds Γ.ctx) → QTypedABody constTy Δ₂ Γ b ρ
    | _, _, _, _, _, .empty, _ => .empty
    | _, _, _, _, _, .field h, hΔ => .field (QTypedA.weaken h hΔ)
    | _, _, _, _, _, .cat h₁ h₂, hΔ =>
        .cat (QTypedABody.weaken h₁ hΔ) (QTypedABody.weaken h₂ hΔ)
end

-- ⊢  the special case the induction uses: a sublist, up to held entries
theorem QTypedA.weaken_sub {B C : Type} {constTy : C → B} {Δ₁ Δ₂ : List (Assume B)}
    {Γ : QCtx B} {e : Expr C} {τ : Ty B} (h : QTypedA constTy Δ₁ Γ e τ)
    (hs : ∀ a ∈ Δ₁, a ∈ Δ₂) : QTypedA constTy Δ₂ Γ e τ :=
  h.weaken (fun a ha => .inl (hs a ha))

--------------------- FROM ASSUMED TO PLAIN ----------------------------------
-- With every assumption held, a `QTypedA` derivation is a `QTyped` one — except
-- at `qVar`, where the constraints discharge only UP TO ≈ and `QScheme.Inst`
-- wants them on the nose. Closing that gap is correcting χ at the constraints'
-- result variables to the types the lookups found, which moves the body by an ≈
-- T-eq absorbs.
--
-- For an ARBITRARY scheme that correction does not exist
-- (`instEquivCorrects_false`, Finalization.lean): two constraints on one result
-- variable can find ≈-equal but different types. For a `Correctable` scheme it
-- does, in one step: no row mentions a result variable, so the correction
-- changes no lookup.

/-- the χ-correction for EVERY scheme — refuted (`instEquivCorrects_false`). -/
def InstEquivCorrects (B : Type) : Prop :=
  ∀ (Γ : Ctx B) (sc : QScheme B) (χ : TySubst B), χ.FixedOutside sc.vars →
    (∀ st ∈ sc.constraints, st.DischargeEquiv Γ χ) →
    ∃ τ', QScheme.Inst Γ sc τ' ∧ TyEquiv τ' (sc.body.applySubst χ)

open Classical in
/-- ⊢  **the χ-correction, for correctable schemes.** Send each result variable
to the type its constraint's lookup found; everything else stays. -/
theorem QScheme.Correctable.correct {B : Type} {Γ : Ctx B} {sc : QScheme B}
    (hc : sc.Correctable) {χ : TySubst B} (hfix : χ.FixedOutside sc.vars)
    (hd : ∀ st ∈ sc.constraints, st.DischargeEquiv Γ χ) :
    ∃ τ', QScheme.Inst Γ sc τ' ∧ TyEquiv τ' (sc.body.applySubst χ) := by
  obtain ⟨hwf, hfun, hind⟩ := hc
  let P : TyVar → Ty B → Prop := fun β τ =>
    ∃ st ∈ sc.constraints, st.res = β ∧
      Lookup Γ (st.row.applySubst χ) st.label (.found τ)
  let χ' : TySubst B :=
    ⟨fun β => if h : ∃ τ, P β τ then Classical.choose h else χ.ty β, χ.row⟩
  -- the chosen type is the one the constraint on β finds
  have hchoose : ∀ st ∈ sc.constraints, ∀ τ,
      Lookup Γ (st.row.applySubst χ) st.label (.found τ) → χ'.ty st.res = τ := by
    intro st hst τ hl
    have hex : ∃ τ, P st.res τ := ⟨τ, st, hst, rfl, hl⟩
    show (if h : ∃ τ, P st.res τ then Classical.choose h else χ.ty st.res) = τ
    rw [dif_pos hex]
    obtain ⟨st', hst', hres, hl'⟩ := Classical.choose_spec hex
    have := hfun st' hst' st hst hres
    subst this
    cases lookup_det hl' hl; rfl
  have hnone : ∀ β, (¬ ∃ τ, P β τ) → χ'.ty β = χ.ty β := by
    intro β hn
    show (if h : ∃ τ, P β τ then Classical.choose h else χ.ty β) = χ.ty β
    rw [dif_neg hn]
  -- rows are untouched: no row mentions a result variable
  have hrow : ∀ st ∈ sc.constraints, st.row.applySubst χ' = st.row.applySubst χ := by
    intro st hst
    refine Row.applySubst_congr _ (fun α hα => ⟨hnone α ?_, rfl⟩)
    rintro ⟨τ, st', hst', rfl, -⟩
    exact hind st hst st' hst' hα
  refine ⟨sc.body.applySubst χ', ⟨χ', ⟨fun β hβ => ?_, hfix.2⟩, fun st hst => ?_, rfl⟩, ?_⟩
  · rw [hnone β ?_]; exact hfix.1 β hβ
    rintro ⟨τ, st, hst, rfl, -⟩; exact hβ (hwf st hst)
  · rcases hd st hst with ⟨hl, -⟩ | ⟨hl, hδ⟩ | ⟨hl, hδ⟩
    · exact .hit ((hrow st hst) ▸ hl) (hchoose st hst _ hl)
    · refine .abs ((hrow st hst) ▸ hl) ?_
      rw [hnone _ ?_]; exact hδ
      rintro ⟨τ, st', hst', hres, hl'⟩
      have := hfun st' hst' st hst hres; subst this
      exact nomatch lookup_det hl hl'
    · refine .unk ((hrow st hst) ▸ hl) ?_
      rw [hnone _ ?_]; exact hδ
      rintro ⟨τ, st', hst', hres, hl'⟩
      have := hfun st' hst' st hst hres; subst this
      exact nomatch lookup_det hl hl'
  · refine Ty.applySubst_substEquiv (θ₁ := χ') (θ₂ := χ)
      ⟨fun β => ?_, fun β => RowEquiv.refl (χ.row β)⟩ _
    by_cases hex : ∃ τ, P β τ
    · obtain ⟨τ, st, hst, rfl, hl⟩ := hex
      rw [hchoose st hst τ hl]
      rcases hd st hst with ⟨hl', he⟩ | ⟨hl', _⟩ | ⟨hl', _⟩
      · cases lookup_det hl hl'; exact he.symm
      · exact nomatch lookup_det hl hl'
      · exact nomatch lookup_det hl hl'
    · rw [hnone β hex]; exact .refl _

/-- every scheme a context binds is correctable. -/
def QCtx.Correctable {B : Type} (Γ : QCtx B) : Prop :=
  ∀ x sc, Γ.lookup x = some sc → sc.Correctable

theorem QCtx.Correctable.bindScheme {B : Type} {Γ : QCtx B} (h : Γ.Correctable)
    (x : Var) {sc : QScheme B} (hs : sc.Correctable) : (Γ.bindScheme x sc).Correctable := by
  intro y sc' hy
  rw [QCtx.lookup_bindScheme] at hy
  by_cases hxy : (x == y) = true
  · rw [if_pos hxy] at hy; injection hy with hy; subst hy; exact hs
  · rw [if_neg hxy] at hy; exact h y sc' hy

theorem QCtx.Correctable.bindTy {B : Type} {Γ : QCtx B} (h : Γ.Correctable)
    (x : Var) (τ : Ty B) : (Γ.bindTy x τ).Correctable :=
  h.bindScheme x ⟨fun _ h => (nomatch h), fun _ h => (nomatch h), fun _ h => (nomatch h)⟩

mutual
  /-- ⊢  **cash-in**: all assumptions held, every scheme in sight correctable ⟹
  a plain L2 typing. -/
  theorem QTypedA.toQTyped {B C : Type} {constTy : C → B} :
      {Δ : List (Assume B)} → {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} →
      QTypedA constTy Δ Γ e τ → Γ.Correctable → (∀ a ∈ Δ, a.Holds Γ.ctx) →
      QTyped constTy Γ e τ
    | _, _, _, _, .qCon, _, _ => .qCon
    | _, _, _, _, .qVar hl ⟨χ, hfix, hc, hb⟩, hΓ, hΔ => by
        obtain ⟨τ', hi, he⟩ := (hΓ _ _ hl).correct hfix (fun st hst =>
          match hc st hst with
          | .inl hd => hd
          | .inr hm => Stump.dischargeEquiv_iff_holds.mpr (hΔ _ hm))
        exact .qEq (.qVar hl hi) (hb ▸ he)
    | _, _, _, _, .qEq h he, hΓ, hΔ => .qEq (QTypedA.toQTyped h hΓ hΔ) he
    | _, _, _, _, .qLam h, hΓ, hΔ => .qLam (QTypedA.toQTyped h (hΓ.bindTy _ _) hΔ)
    | _, _, _, _, .qApp h₁ h₂, hΓ, hΔ =>
        .qApp (QTypedA.toQTyped h₁ hΓ hΔ) (QTypedA.toQTyped h₂ hΓ hΔ)
    | _, _, _, _, .qLet hcs hi hin hb, hΓ, hΔ =>
        .qLet (fun τ₁ ⟨χ, hfix, hdis, hbody⟩ => hbody ▸
            QTypedA.toQTyped (hi χ hfix) hΓ (fun a ha =>
              match List.mem_append.mp ha with
              | .inl hc => by
                  obtain ⟨st, hst, rfl⟩ := List.mem_map.mp hc
                  exact Stump.dischargeEquiv_iff_holds.mp (hdis st hst).toEquiv
              | .inr hd => hΔ a hd))
          hin (QTypedA.toQTyped hb (hΓ.bindScheme _ hcs) hΔ)
    | _, _, _, _, .qCat h₁ h₂, hΓ, hΔ =>
        .qCat (QTypedA.toQTyped h₁ hΓ hΔ) (QTypedA.toQTyped h₂ hΓ hΔ)
    | _, _, _, _, .qSel h hl, hΓ, hΔ => .qSel (QTypedA.toQTyped h hΓ hΔ) hl
    | _, _, _, _, .qSelUnk h hl, hΓ, hΔ => .qSelUnk (QTypedA.toQTyped h hΓ hΔ) hl
    | _, _, _, _, .qSelAbs h hl, hΓ, hΔ => .qSelAbs (QTypedA.toQTyped h hΓ hΔ) hl
    | _, _, _, _, .qUnk h, hΓ, hΔ => .qUnk (QTypedA.toQTyped h hΓ hΔ)
    | _, _, _, _, .qRcd h, hΓ, hΔ => .qRcd (QTypedABody.toQTyped h hΓ hΔ)
    | _, _, _, _, .assume h hm, hΓ, hΔ => by
        have h' := QTypedA.toQTyped h hΓ hΔ
        cases hΔ _ hm with
        | hit hl he => exact .qEq (.qSel h' hl) he.symm
        | abs hl he => cases he; exact .qSelAbs h' hl
        | unk hl he => cases he; exact .qSelUnk h' hl

  theorem QTypedABody.toQTyped {B C : Type} {constTy : C → B} :
      {Δ : List (Assume B)} → {Γ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
      QTypedABody constTy Δ Γ b ρ → Γ.Correctable → (∀ a ∈ Δ, a.Holds Γ.ctx) →
      QTypedBody constTy Γ b ρ
    | _, _, _, _, .empty, _, _ => .empty
    | _, _, _, _, .field h, hΓ, hΔ => .field (QTypedA.toQTyped h hΓ hΔ)
    | _, _, _, _, .cat h₁ h₂, hΓ, hΔ =>
        .cat (QTypedABody.toQTyped h₁ hΓ hΔ) (QTypedABody.toQTyped h₂ hΓ hΔ)
end

--------------------- 2. A SCHEME ENTERS Γ′ RENAMED --------------------------
-- `QScheme.applySubst` pushes σ under the binders without renaming, which is
-- right exactly when σ is `Avoiding`. An arbitrary σ ⊨ S′ is not: it may send a
-- free variable of the scheme to a type mentioning a binder. So Γ′ holds the
-- scheme READ under σ: one substitution renames each binder α to f α and applies
-- σ everywhere else (`readSub`). The only condition is that σ's image of the
-- scheme's free variables avoids the new names — satisfiable for any σ, since
-- that image is finite (`SchemeRead.exists`).

/-- rename the binders `vs` by f; apply σ elsewhere. -/
def readSub {B : Type} (σ : TySubst B) (vs : List TyVar) (f : TyVar → TyVar) :
    TySubst B :=
  ⟨fun α => if α ∈ vs then .var (f α) else σ.ty α,
   fun α => if α ∈ vs then .var (f α) else σ.row α⟩

/-- `sc` read under σ, binders renamed by f. -/
def QScheme.readAt {B : Type} (sc : QScheme B) (σ : TySubst B) (f : TyVar → TyVar) :
    QScheme B :=
  ⟨sc.vars.map f,
   sc.constraints.map (fun st =>
     (⟨st.row.applySubst (readSub σ sc.vars f), st.label,
       if st.res ∈ sc.vars then f st.res else st.res⟩ : Stump B)),
   sc.body.applySubst (readSub σ sc.vars f)⟩

/-- `sc′` is `sc` read under σ, with binders renamed apart by an injective f
whose names σ's image of the free variables does not reach. -/
def SchemeRead {B : Type} (σ : TySubst B) (sc sc' : QScheme B) : Prop :=
  ∃ f : TyVar → TyVar,
    (∀ α ∈ sc.vars, ∀ β ∈ sc.vars, f α = f β → α = β) ∧
    (∀ α ∈ sc.freeFtv, α ∉ sc.vars →
       (∀ β ∈ (σ.ty α).ftv, β ∉ sc.vars.map f) ∧
       (∀ β ∈ (σ.row α).ftv, β ∉ sc.vars.map f)) ∧
    sc' = sc.readAt σ f

/-- `Γ′` is `Γ` read under σ: row environment discharged, each scheme read. -/
structure CtxRead {B : Type} (σ : TySubst B) (Γ Γ' : QCtx B) : Prop where
  row   : Γ'.rowEnv = []
  schem : ∀ x sc, Γ.lookup x = some sc →
            ∃ sc', Γ'.lookup x = some sc' ∧ SchemeRead σ sc sc'

theorem CtxRead.ctx_eq {B : Type} {σ : TySubst B} {Γ Γ' : QCtx B}
    (h : CtxRead σ Γ Γ') : Γ'.ctx = (⟨[], []⟩ : Ctx B) := by
  unfold QCtx.ctx; rw [h.row]

theorem CtxRead.nil {B : Type} (σ : TySubst B) :
    CtxRead σ (⟨[], []⟩ : QCtx B) ⟨[], []⟩ :=
  ⟨rfl, fun _ _ h => nomatch h⟩

-- ⊢  a monotype is read by substituting: nothing to rename
theorem SchemeRead.mono {B : Type} (σ : TySubst B) (τ : Ty B) :
    SchemeRead σ ⟨[], [], τ⟩ ⟨[], [], τ.applySubst σ⟩ := by
  refine ⟨id, fun _ h => absurd h List.not_mem_nil,
    fun _ _ _ => ⟨fun _ _ => by simp, fun _ _ => by simp⟩, ?_⟩
  have hid : τ.applySubst (readSub σ ([] : List TyVar) id) = τ.applySubst σ :=
    Ty.applySubst_congr τ (fun _ _ => ⟨by simp [readSub], by simp [readSub]⟩)
  simp [QScheme.readAt, hid]

theorem CtxRead.bindScheme {B : Type} {σ : TySubst B} {Γ Γ' : QCtx B}
    (h : CtxRead σ Γ Γ') (x : Var) {sc sc' : QScheme B} (hs : SchemeRead σ sc sc') :
    CtxRead σ (Γ.bindScheme x sc) (Γ'.bindScheme x sc') where
  row := h.row
  schem := by
    intro y sc₁ hy
    simp only [QCtx.lookup_bindScheme] at hy ⊢
    by_cases hxy : (x == y) = true
    · rw [if_pos hxy] at hy ⊢
      injection hy with hy; subst hy
      exact ⟨sc', rfl, hs⟩
    · rw [if_neg hxy] at hy ⊢
      exact h.schem y sc₁ hy

theorem CtxRead.bindTy {B : Type} {σ : TySubst B} {Γ Γ' : QCtx B}
    (h : CtxRead σ Γ Γ') (x : Var) (τ : Ty B) :
    CtxRead σ (Γ.bindTy x τ) (Γ'.bindTy x (τ.applySubst σ)) :=
  h.bindScheme x (SchemeRead.mono σ τ)

-- ⊢  reading depends on σ only at the scheme's free variables
theorem SchemeRead.congr {B : Type} {σ σ₁ : TySubst B} {sc sc' : QScheme B}
    (hr : SchemeRead σ sc sc')
    (hag : ∀ α ∈ sc.freeFtv, α ∉ sc.vars → σ₁.ty α = σ.ty α ∧ σ₁.row α = σ.row α) :
    SchemeRead σ₁ sc sc' := by
  obtain ⟨f, hinj, hav, rfl⟩ := hr
  have hsub : ∀ α ∈ sc.freeFtv,
      (readSub σ₁ sc.vars f).ty α = (readSub σ sc.vars f).ty α ∧
      (readSub σ₁ sc.vars f).row α = (readSub σ sc.vars f).row α := by
    intro α hα
    by_cases hv : α ∈ sc.vars
    · simp [readSub, hv]
    · simp [readSub, hv, (hag α hα hv).1, (hag α hα hv).2]
  refine ⟨f, hinj, fun α hα hv => ?_, ?_⟩
  · rw [(hag α hα hv).1, (hag α hα hv).2]; exact hav α hα hv
  · simp only [QScheme.readAt]
    congr 1
    · apply List.map_congr_left
      intro st hst
      rw [Row.applySubst_congr st.row (fun α hα =>
        hsub α (List.mem_append_left _ (List.mem_flatMap.mpr ⟨st, hst, hα⟩)))]
    · exact Ty.applySubst_congr _ (fun α hα =>
        ⟨(hsub α (List.mem_append_right _ hα)).1.symm, (hsub α (List.mem_append_right _ hα)).2.symm⟩)

--------------------- FRESH BINDER NAMES EXIST ---------------------------------
-- Append a long enough run of `a`s: the result is longer than every name to
-- avoid, and appending the same suffix is injective.

theorem fresh_renaming_exists {B : Type} (σ : TySubst B) (vs L : List TyVar) :
    ∃ f : TyVar → TyVar,
      (∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β) ∧
      (∀ α ∈ vs, f α ∉ L) ∧
      (∀ α ∈ L, (∀ β ∈ (σ.ty α).ftv, β ∉ vs.map f) ∧
                (∀ β ∈ (σ.row α).ftv, β ∉ vs.map f)) := by
  let A : List TyVar := L ++ L.flatMap (fun α => (σ.ty α).ftv ++ (σ.row α).ftv)
  let N := lenBound A + 1
  refine ⟨fun α => α ++ natName N, fun α _ β _ h => (String.append_left_inj _).mp h,
    fun α _ hm => ?_, fun α hα => ⟨fun β hβ hm => ?_, fun β hβ hm => ?_⟩⟩
  · have := length_le_lenBound (l := A) (List.mem_append_left _ hm)
    simp [String.length_append, natName_length, N] at this; omega
  · obtain ⟨γ, -, rfl⟩ := List.mem_map.mp hm
    have := length_le_lenBound (l := A) (List.mem_append_right _
      (List.mem_flatMap.mpr ⟨α, hα, List.mem_append_left _ hβ⟩))
    simp [String.length_append, natName_length, N] at this; omega
  · obtain ⟨γ, -, rfl⟩ := List.mem_map.mp hm
    have := length_le_lenBound (l := A) (List.mem_append_right _
      (List.mem_flatMap.mpr ⟨α, hα, List.mem_append_right _ hβ⟩))
    simp [String.length_append, natName_length, N] at this; omega

theorem SchemeRead.exists {B : Type} (σ : TySubst B) (sc : QScheme B) :
    ∃ sc', SchemeRead σ sc sc' := by
  obtain ⟨f, hinj, -, hav⟩ := fresh_renaming_exists σ sc.vars sc.freeFtv
  exact ⟨_, f, hinj, fun α hα _ => hav α hα, rfl⟩

--------------------- WHAT THE READING BUYS -----------------------------------
-- Any instance κ of the ALGORITHMIC scheme that agrees with σ off the binders
-- is matched by one instance χ of the read scheme: on a new binder f α it is
-- κ α, elsewhere the identity. The body lands on the nose, and each read
-- constraint at χ is the κ-reading of the original one.

private def finv (vs : List TyVar) (f : TyVar → TyVar) (β : TyVar) : TyVar :=
  (vs.find? (fun a => decide (f a = β))).getD β

private theorem finv_f {vs : List TyVar} {f : TyVar → TyVar}
    (hinj : ∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β) {α : TyVar} (hα : α ∈ vs) :
    finv vs f (f α) = α := by
  unfold finv
  cases h : vs.find? (fun a => decide (f a = f α)) with
  | none => exact absurd (List.find?_eq_none.mp h α hα) (by simp)
  | some a =>
      have h1 := List.find?_some h
      have h2 := List.mem_of_find?_eq_some h
      simp only [decide_eq_true_eq] at h1
      exact hinj a h2 α hα h1

/-- the instance of the read scheme matching κ. -/
def readInst {B : Type} (vs : List TyVar) (f : TyVar → TyVar) (κ : TySubst B) :
    TySubst B :=
  ⟨fun β => if β ∈ vs.map f then κ.ty (finv vs f β) else .var β,
   fun β => if β ∈ vs.map f then κ.row (finv vs f β) else .var β⟩

private theorem readInst_agree {B : Type} {vs : List TyVar} {f : TyVar → TyVar}
    {σ κ : TySubst B}
    (hinj : ∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β)
    (hκ : ∀ α, α ∉ vs → κ.ty α = σ.ty α ∧ κ.row α = σ.row α) {α : TyVar}
    (hav : α ∉ vs → (∀ β ∈ (σ.ty α).ftv, β ∉ vs.map f) ∧
                    (∀ β ∈ (σ.row α).ftv, β ∉ vs.map f)) :
    ((readInst vs f κ).comp (readSub σ vs f)).ty α = κ.ty α ∧
    ((readInst vs f κ).comp (readSub σ vs f)).row α = κ.row α := by
  have hmem : ∀ a ∈ vs, f a ∈ vs.map f := fun a ha => List.mem_map_of_mem ha
  by_cases hα : α ∈ vs
  · refine ⟨?_, ?_⟩
    · simp only [TySubst.comp, readSub, if_pos hα, Ty.applySubst]
      simp only [readInst, if_pos (hmem α hα), finv_f hinj hα]
    · simp only [TySubst.comp, readSub, if_pos hα, Row.applySubst]
      simp only [readInst, if_pos (hmem α hα), finv_f hinj hα]
  · obtain ⟨hvt, hvr⟩ := hav hα
    refine ⟨?_, ?_⟩
    · simp only [TySubst.comp, readSub, if_neg hα]
      rw [(hκ α hα).1]
      exact Ty.applySubst_fixed_ftv _ (fun β hβ =>
        ⟨by simp only [readInst, if_neg (hvt β hβ)], by simp only [readInst, if_neg (hvt β hβ)]⟩)
    · simp only [TySubst.comp, readSub, if_neg hα]
      rw [(hκ α hα).2]
      exact Row.applySubst_fixed_ftv _ (fun β hβ =>
        ⟨by simp only [readInst, if_neg (hvr β hβ)], by simp only [readInst, if_neg (hvr β hβ)]⟩)

/-- ⊢  **every instance of the scheme that agrees with σ off its binders is an
instance of the read scheme** — body and constraint readings on the nose. -/
theorem SchemeRead.instAt {B : Type} {σ κ : TySubst B} {sc sc' : QScheme B}
    (hr : SchemeRead σ sc sc') (hwf : sc.WF)
    (hκ : ∀ α, α ∉ sc.vars → κ.ty α = σ.ty α ∧ κ.row α = σ.row α) :
    ∃ χ : TySubst B, χ.FixedOutside sc'.vars ∧
      sc'.body.applySubst χ = sc.body.applySubst κ ∧
      sc'.constraints.map (Stump.at χ) = sc.constraints.map (Stump.at κ) := by
  obtain ⟨f, hinj, hav, rfl⟩ := hr
  refine ⟨readInst sc.vars f κ, ⟨fun β hβ => ?_, fun β hβ => ?_⟩, ?_, ?_⟩
  · have hβ' : β ∉ sc.vars.map f := hβ
    simp only [readInst, if_neg hβ']
  · have hβ' : β ∉ sc.vars.map f := hβ
    simp only [readInst, if_neg hβ']
  · show (sc.body.applySubst (readSub σ sc.vars f)).applySubst _ = _
    rw [Ty.applySubst_applySubst]
    exact Ty.applySubst_congr _ (fun α hα => readInst_agree hinj hκ
      (hav α (List.mem_append_right _ hα)))
  · simp only [QScheme.readAt, List.map_map]
    apply List.map_congr_left
    intro st hst
    have hres := hwf st hst
    simp only [Function.comp, Stump.at, if_pos hres]
    congr 1
    · rw [Row.applySubst_applySubst]
      exact Row.applySubst_congr _ (fun α hα => readInst_agree hinj hκ
        (hav α (List.mem_append_left _ (List.mem_flatMap.mpr ⟨st, hst, hα⟩))))
    · have hm : f st.res ∈ sc.vars.map f := List.mem_map_of_mem hres
      simp only [readInst, if_pos hm, finv_f hinj hres]

/-- ⊢  …and in particular A-var's instance: κ = σ ∘ θ, whose constraint readings
are exactly the σ-readings of the stumps A-var parks. -/
theorem SchemeRead.inst {B : Type} {σ θ : TySubst B} {sc sc' : QScheme B}
    {g : TyVar → TyVar} {ps : List (Parked B)}
    (hr : SchemeRead σ sc sc') (hwf : sc.WF) (hθ : IsRenaming θ sc.vars g)
    (hps : InstStumps θ g sc.constraints ps) :
    ∃ χ : TySubst B, χ.FixedOutside sc'.vars ∧
      sc'.body.applySubst χ = (sc.body.applySubst θ).applySubst σ ∧
      sc'.constraints.map (Stump.at χ) = ps.map (fun p => p.stump.at σ) := by
  obtain ⟨χ, hfix, hb, hc⟩ := hr.instAt hwf (κ := σ.comp θ) (fun α hα =>
    ⟨by show (θ.ty α).applySubst σ = _; rw [hθ.1.1 α hα]; rfl,
     by show (θ.row α).applySubst σ = _; rw [hθ.1.2 α hα]; rfl⟩)
  refine ⟨χ, hfix, by rw [hb, Ty.applySubst_applySubst], ?_⟩
  rw [hc]
  have hps' : ps.map (fun p => p.stump.at σ)
      = (ps.map Parked.stump).map (Stump.at σ) := by simp [List.map_map]
  rw [hps', hps, List.map_map]
  apply List.map_congr_left
  intro st hst
  simp only [Function.comp, Stump.at]
  congr 1
  · rw [Row.applySubst_applySubst]
  · show (θ.ty st.res).applySubst σ = σ.ty (g st.res)
    rw [(hθ.2 st.res (hwf st hst)).1]; rfl

/-- A-var, given what wake-up did with the instantiated constraints: each one
either holds (it was discharged) or is assumed (it is still parked). -/
theorem inferA_sound_var_step {B C : Type} {constTy : C → B} {σ θ : TySubst B}
    {Γ Γ' : QCtx B} {x : Var} {sc : QScheme B} {g : TyVar → TyVar}
    {ps : List (Parked B)} {Δ : List (Assume B)}
    (hread : CtxRead σ Γ Γ') (hl : Γ.lookup x = some sc) (hwf : sc.WF)
    (hθ : IsRenaming θ sc.vars g) (hps : InstStumps θ g sc.constraints ps)
    (hcov : ∀ p ∈ ps, (p.stump.at σ).Holds Γ'.ctx ∨ p.stump.at σ ∈ Δ) :
    QTypedA constTy Δ Γ' (.var x) ((sc.body.applySubst θ).applySubst σ) := by
  obtain ⟨sc', hl', hr⟩ := hread.schem x sc hl
  obtain ⟨χ, hfix, hbody, hcs⟩ := hr.inst hwf hθ hps
  refine .qVar hl' ⟨χ, hfix, fun st' hst' => ?_, hbody⟩
  have hm : st'.at χ ∈ ps.map (fun p => p.stump.at σ) :=
    hcs ▸ List.mem_map_of_mem hst'
  obtain ⟨p, hp, hpe⟩ := List.mem_map.mp hm
  rcases hcov p hp with hh | hd
  · exact .inl (Stump.dischargeEquiv_iff_holds.mpr (hpe ▸ hh))
  · exact .inr (hpe ▸ hd)

--------------------- 1. THE STATEMENT ---------------------------------------
-- Γ under σ, τ under σ, and the parked stumps read under σ as assumptions.

/-- the conclusion the induction carries for one derivation ending in S′. -/
def SoundAt {B C : Type} (constTy : C → B) (Γ : QCtx B) (e : Expr C) (τ : Ty B)
    (S' : SolverState B) : Prop :=
  ∀ σ : TySubst B, Absorbs σ S' → Sol.Sat σ S'.sol → ∀ Γ' : QCtx B, CtxRead σ Γ Γ' →
    QTypedA constTy (S'.parked.map (fun p => p.stump.at σ)) Γ' e (τ.applySubst σ)

def SoundAtRec {B C : Type} (constTy : C → B) (Γ : QCtx B) (ξ : RecBody (Expr C))
    (ρ : Row B) (S' : SolverState B) : Prop :=
  ∀ σ : TySubst B, Absorbs σ S' → Sol.Sat σ S'.sol → ∀ Γ' : QCtx B, CtxRead σ Γ Γ' →
    QTypedABody constTy (S'.parked.map (fun p => p.stump.at σ)) Γ' ξ (ρ.applySubst σ)

/-- **Inference soundness, assumption form** — what `InferSoundC` should have
said. Γ and τ are read under the SAME σ, which is any substitution satisfying the
final state AND absorbs it (σ ∘ ⟦S′⟧ = σ — `Absorbs`, Absorb.lean; A-let needs
the exact form); what is still parked is assumed, read under σ as well. The
start state satisfies the parked-list invariant, is clean and quiescent, and Γ's
schemes are well-formed — all trivially true of a run from nothing. -/
def InferSound (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ (Γ : QCtx B) (S S' : SolverState B) (e : Expr C) (τ : Ty B),
    Infer constTy Γ S e τ S' → S.PInv → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ e τ S'

--------------------- WHAT HAPPENS TO A PARKED STUMP ---------------------------
-- `SolverState.KeepsS` (ParkedInv.lean): a stump parked at S is, at S′, still
-- parked or discharged. That is exactly what moves a typing from S's
-- assumptions to S′'s.

theorem SolverState.KeepsS.lift {B C : Type} [DecidableEq B] {constTy : C → B}
    {S S' : SolverState B} (hk : S.KeepsS S') {σ : TySubst B} (hσ : Sol.Sat σ S'.sol)
    {Γ Γ' : QCtx B} (hr : CtxRead σ Γ Γ') {e : Expr C} {τ : Ty B}
    (h : QTypedA constTy (S.parked.map (fun p => p.stump.at σ)) Γ' e τ) :
    QTypedA constTy (S'.parked.map (fun p => p.stump.at σ)) Γ' e τ :=
  h.weaken (fun a ha => by
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    rw [hr.ctx_eq]
    rcases hk σ hσ p hp with ⟨q, hq, hqs⟩ | hd
    · exact .inl (List.mem_map.mpr ⟨q, hq, by rw [hqs]⟩)
    · exact .inr (Stump.dischargeEquiv_iff_holds.mp hd))

theorem SolverState.KeepsS.liftBody {B C : Type} [DecidableEq B] {constTy : C → B}
    {S S' : SolverState B} (hk : S.KeepsS S') {σ : TySubst B} (hσ : Sol.Sat σ S'.sol)
    {Γ Γ' : QCtx B} (hr : CtxRead σ Γ Γ') {ξ : RecBody (Expr C)} {ρ : Row B}
    (h : QTypedABody constTy (S.parked.map (fun p => p.stump.at σ)) Γ' ξ ρ) :
    QTypedABody constTy (S'.parked.map (fun p => p.stump.at σ)) Γ' ξ ρ :=
  h.weaken (fun a ha => by
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    rw [hr.ctx_eq]
    rcases hk σ hσ p hp with ⟨q, hq, hqs⟩ | hd
    · exact .inl (List.mem_map.mpr ⟨q, hq, by rw [hqs]⟩)
    · exact .inr (Stump.dischargeEquiv_iff_holds.mp hd))

--------------------- THE INDUCTION, MODULO A-var AND A-let --------------------
-- The two cases whose own obligations are elsewhere are hypotheses, stated as
-- the case with its induction hypotheses. The parked-list bookkeeping is no
-- longer one: `Infer.pinv_keeps` (ParkedInv.lean) proves it.

/-- the A-var case. `inferA_sound_var_step` is its typing half. -/
def VarCase (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ {Γ : QCtx B} {S S' : SolverState B} {x : Var} {sc : QScheme B}
    {θ : TySubst B} {f : TyVar → TyVar} {ps : List (Parked B)} {Sup : Supply} {K : KEnv},
    Γ.lookup x = some sc → IsRenaming θ sc.vars f → FreshRenaming f sc.vars Γ S →
    (∀ α ∈ sc.vars, ∃ k, S.supply.next ≤ k ∧ k < Sup.next ∧ f α = natName k) →
    S.supply.next ≤ Sup.next →
    InstStumps θ f sc.constraints ps → WakesSat { S with supply := Sup, kinds := K } ps S' →
    S.PInv → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ (.var x) (sc.body.applySubst θ) S'

/-- the A-let case, given both induction hypotheses. -/
def LetCase (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ {Γ : QCtx B} {S S₁ S₂ : SolverState B} {x : Var} {e₁ e₂ : Expr C}
    {τ₁ τ₂ : Ty B} {Δq Δγ : List (Parked B)} {ᾱ : List TyVar} {κs : List Kind},
    Infer constTy Γ S e₁ τ₁ S₁ →
    S₁.kinds.Assigns ᾱ κs → S₁.parked.Perm (Δq ++ Δγ) →
    (∀ p ∈ Δq, p.blocker ∈ ᾱ) → (∀ p ∈ Δγ, p.blocker ∉ ᾱ) →
    (∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv) →
    (∀ p ∈ Δq, ∀ q ∈ S.parked, p.stump ≠ q.stump) →
    LetResults S₁ ᾱ Δq →
    (∀ α ∈ ᾱ, ∀ p ∈ Δγ, α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧
       α ∉ (S₁.subst.ty p.stump.res).ftv) →
    (∀ α ∈ ᾱ, α ∉ S₁.sol.dom) →
    (∀ p ∈ Δq, ∀ q ∈ Δq, S₁.resVar q.stump.res ∉ (p.stump.row.applySubst S₁.subst).ftv) →
    Infer constTy (Γ.bindScheme x (letScheme S₁ ᾱ Δq τ₁))
      { S₁ with parked := Δγ } e₂ τ₂ S₂ →
    S.PInv → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ e₁ τ₁ S₁ →
    SoundAt constTy (Γ.bindScheme x (letScheme S₁ ᾱ Δq τ₁)) e₂ τ₂ S₂ →
    SoundAt constTy Γ (.letE x e₁ e₂) τ₂ S₂

/-- ⊢  the let scheme is well formed: `LetResults` is exactly `QScheme.WF` read
off the generalized stumps. -/
theorem letScheme_wf {B : Type} [DecidableEq B] {Γ : QCtx B} {S₁ : SolverState B}
    {x : Var} {τ₁ : Ty B} {Δq : List (Parked B)} {ᾱ : List TyVar}
    (hΓ : Γ.SchemesWF) (hres : LetResults S₁ ᾱ Δq) :
    (Γ.bindScheme x (letScheme S₁ ᾱ Δq τ₁)).SchemesWF := by
  refine hΓ.bindScheme x ?_ ?_
  · intro st hst
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst
    exact (hres.1 p hp).2
  · intro a ha b hb he
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hb
    rw [hres.2 p hp q hq he]

-- the let body's context and state satisfy the invariants too (ParkedInv's
-- let case, restated for the induction's use)
theorem let_body_inv {B : Type} [DecidableEq B] {Γ : QCtx B}
    {S₁ : SolverState B} {x : Var} {τ₁ : Ty B} {Δq Δγ : List (Parked B)}
    {ᾱ : List TyVar} (i₁ : S₁.PInv) (hΓ : Γ.SchemesWF) (hsplit : S₁.parked.Perm (Δq ++ Δγ))
    (hres : LetResults S₁ ᾱ Δq) :
    ({ S₁ with parked := Δγ } : SolverState B).PInv ∧
    (Γ.bindScheme x (letScheme S₁ ᾱ Δq τ₁)).SchemesWF := by
  refine ⟨i₁.of_sub (fun p hp => by exact hsplit.mem_iff.mpr <| List.mem_append_right _ hp)
    (Nat.le_refl _), letScheme_wf hΓ hres⟩

private theorem draw_eqs {B : Type} {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.sol = S.sol ∧ S₀.parked = S.parked := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  subst h2; exact ⟨rfl, rfl⟩

/-- ⊢  **the A-var case, proved.** Wake-up either discharges an instantiated
constraint or leaves it parked; saturation keeps it that way or discharges it
later. Either way `inferA_sound_var_step`'s `hcov` holds at the final state. -/
theorem varCase {B C : Type} [DecidableEq B] {constTy : C → B} :
    VarCase B C constTy := by
  intro Γ S S' x sc θ f ps Sup K hl hθ hfr hdr hle hps hw h hΓ _ _ σ _ hσ Γ' hr
  obtain ⟨S₁, hws, hsat⟩ := hw
  obtain ⟨hwf, hfun⟩ := hΓ x sc hl
  have hup := supply_up_pinv (K := K) h hle
  have hok := psOk_of_var (K := K) h hwf hfun hfr.1 hdr hps
  obtain ⟨i₁, -⟩ := hws.pinv_keeps hup hok
  obtain ⟨-, k₂⟩ := hsat.pinv_keeps i₁
  have hfate := hws.fate hup hok σ (hsat.satMono σ hσ)
  refine inferA_sound_var_step hr hl hwf hθ hps (fun p hp => ?_)
  rw [hr.ctx_eq]
  rcases hfate p hp with ⟨q, hq, hqs⟩ | hd
  · rcases k₂ σ hσ q hq with ⟨q', hq', hq's⟩ | hd'
    · exact .inr (List.mem_map.mpr ⟨q', hq', by rw [hq's, hqs]⟩)
    · exact .inl (Stump.dischargeEquiv_iff_holds.mp (hqs ▸ hd'))
  · exact .inl (Stump.dischargeEquiv_iff_holds.mp hd)

private theorem quiescent_of_eq {B : Type} {S S' : SolverState B}
    (hs : S'.sol = S.sol) (hp : S'.parked = S.parked) (hq : S.Quiescent) :
    S'.Quiescent := by
  intro p hpm
  have := hq p (hp ▸ hpm)
  unfold SolverState.ctx SolverState.subst at this ⊢
  rw [hs]; exact this

private theorem quiescent_sub {B : Type} {S : SolverState B} {Δ : List (Parked B)}
    (hq : S.Quiescent) (hs : ∀ p ∈ Δ, p ∈ S.parked) :
    ({ S with parked := Δ } : SolverState B).Quiescent :=
  fun p hp => hq p (hs p hp)

mutual

/-- ⊢  **`InferSound` is inductive**: every rule but A-var and A-let, with those
two as hypotheses. The parked-list bookkeeping is proved (`Infer.pinv_keeps`),
and absorption is carried back to each premise's state (`Absorbs.back`). -/
theorem inferSound_of {B C : Type} [DecidableEq B] {constTy : C → B}
    (hvar : VarCase B C constTy) (hlet : LetCase B C constTy) :
    {Γ : QCtx B} → {S S' : SolverState B} → {e : Expr C} → {τ : Ty B} →
    Infer constTy Γ S e τ S' → S.PInv → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ e τ S'
  | _, _, _, _, _, .con, _, _, _, _ => fun _ _ _ _ _ => .qCon
  | _, _, _, _, _, .var hl hθ hfr hdr hle _ hps hw, h, hΓ, hc, hq =>
      hvar hl hθ hfr hdr hle hps hw h hΓ hc hq
  | _, _, _, _, _, .lam hd hb, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨h₀, -, -, -, -, hp₀⟩ := draw_pinv_keeps hd h
      obtain ⟨hs₀, -⟩ := draw_eqs hd
      exact .qLam (inferSound_of hvar hlet hb h₀ (hΓ.bindTy _ _) (hs₀ ▸ hc)
        (quiescent_of_eq hs₀ hp₀ hq) σ hab hσ _ (hr.bindTy _ (.var _)))
  | _, _, _, _, _, .app h₁ h₂ hd hs, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨i₂, k₂⟩ := Infer.pinv_keeps h₂ i₁ hΓ
      obtain ⟨i₃, k₃, m₃, -, -, -⟩ := draw_pinv_keeps hd i₂
      obtain ⟨-, k₄⟩ := hs.pinv_keeps i₃
      have c₁ := Infer.clean h₁ hc
      have c₂ := Infer.clean h₂ c₁
      have x₂ := (draw_ext hd).trans hs.ext
      have x₁ := (Infer.ext h₂).trans x₂
      have K₂ := k₃.trans k₄ hs.satMono
      have K₁ := k₂.trans K₂ (m₃.trans hs.satMono)
      obtain ⟨S₃', hsolve, hsatu⟩ := hs
      have hσ₃' := hsatu.satMono σ hσ
      have hσ₂ := m₃ σ (hsolve.satMono σ hσ₃')
      have hσ₁ := Infer.sat_mono h₂ σ hσ₂
      have q₁ := Infer.quiescent h₁ hq
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ h hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₂.lift hσ hr (inferSound_of hvar hlet h₂ i₁ hΓ c₁ q₁ σ
        (hab.back x₂ c₂) hσ₂ Γ' hr)
      exact .qApp (.qEq ih₁ (hsolve.unifies_sat hσ₃')) ih₂
  | _, _, _, _, _, .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨i₂, k₂⟩ := Infer.pinv_keeps h₂ i₁ hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd₁ i₂
      obtain ⟨ib, kb, mb, -, -, -⟩ := draw_pinv_keeps hd₂ ia
      obtain ⟨i₃, k₃⟩ := hs₁.pinv_keeps ib
      obtain ⟨-, k₄⟩ := hs₂.pinv_keeps i₃
      have c₁ := Infer.clean h₁ hc
      have c₂ := Infer.clean h₂ c₁
      have x₂ := (draw_ext hd₁).trans ((draw_ext hd₂).trans (hs₁.ext.trans hs₂.ext))
      have x₁ := (Infer.ext h₂).trans x₂
      have m34 := hs₁.satMono.trans hs₂.satMono
      have K₂ := ka.trans (kb.trans (k₃.trans k₄ hs₂.satMono) m34) (mb.trans m34)
      have K₁ := k₂.trans K₂ (ma.trans (mb.trans m34))
      have hσ₃ := hs₂.satMono σ hσ
      obtain ⟨S₃', hsolve₁, hsatu₁⟩ := hs₁
      obtain ⟨S₄', hsolve₂, hsatu₂⟩ := hs₂
      have hσ₄' := hsatu₂.satMono σ hσ
      have hσ₃' := hsatu₁.satMono σ hσ₃
      have hσ₂ := ma σ (mb σ (hsolve₁.satMono σ hσ₃'))
      have hσ₁ := Infer.sat_mono h₂ σ hσ₂
      have q₁ := Infer.quiescent h₁ hq
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ h hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₂.lift hσ hr (inferSound_of hvar hlet h₂ i₁ hΓ c₁ q₁ σ
        (hab.back x₂ c₂) hσ₂ Γ' hr)
      exact .qCat (.qEq ih₁ (hsolve₁.unifies_sat hσ₃')) (.qEq ih₂ (hsolve₂.unifies_sat hσ₄'))
  | _, _, _, _, _, .sel h₁ hd hs hlk, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨-, k₂⟩ := hs.pinv_keeps ia
      have c₁ := Infer.clean h₁ hc
      have x₁ := (draw_ext hd).trans hs.ext
      have K := ka.trans k₂ hs.satMono
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih := K.lift hσ hr (inferSound_of hvar hlet h₁ h hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have hrcd := QTypedA.qEq ih (hsolve.unifies_sat hσ₂')
      obtain ⟨r'', hl'', he''⟩ :=
        Sol.lookup_toCtx_sat (Γ' := Γ'.ctx) hσ hr.row hlk (by intro hh; cases hh)
      cases he'' with
      | found hty => exact .qEq (.qSel hrcd hl'') hty.symm
  | _, _, _, _, _, .selAbs h₁ hd hs hlk, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨-, k₂⟩ := hs.pinv_keeps ia
      have c₁ := Infer.clean h₁ hc
      have x₁ := (draw_ext hd).trans (hs.ext.trans (.of_sol_eq rfl))
      have K := ka.trans k₂ hs.satMono
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih := K.lift hσ hr (inferSound_of hvar hlet h₁ h hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have hrcd := QTypedA.qEq ih (hsolve.unifies_sat hσ₂')
      obtain ⟨r'', hl'', he''⟩ :=
        Sol.lookup_toCtx_sat (Γ' := Γ'.ctx) hσ hr.row hlk (by intro hh; cases hh)
      cases he'' with
      | absent => exact .qSelAbs hrcd hl''
  | _, _, _, _, _, .selUnk (S₂ := S₂) (S₂' := S₂d) (l := l) (r := r) (α := α) (δ := δ)
      h₁ hd hs _ hd₂, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      obtain ⟨-, kb, mb, -, -, -⟩ := draw_pinv_keeps hd₂ i₂
      have c₁ := Infer.clean h₁ hc
      have x₁ := (draw_ext hd).trans (hs.ext.trans ((draw_ext hd₂).trans
        (SolverState.Ext.of_sol_eq (S := S₂d) (S' := S₂d.park ⟨α, ⟨Row.var r, l, δ⟩⟩) rfl)))
      have kp : SolverState.KeepsS S₂d (S₂d.park ⟨α, ⟨Row.var r, l, δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₂d.park ⟨α, ⟨Row.var r, l, δ⟩⟩) (S' := S₂d) rfl
      have K := ka.trans (k₂.trans (kb.trans kp mp) (mb.trans mp))
        (hs.satMono.trans (mb.trans mp))
      obtain ⟨hs₂, -⟩ := draw_eqs hd₂
      have hσ₂ : Sol.Sat σ S₂.sol := hs₂ ▸ hσ
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ₂
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      exact .assume (.qEq (K.lift hσ hr (inferSound_of hvar hlet h₁ h hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)) (hsolve.unifies_sat hσ₂')) List.mem_cons_self
  | _, _, _, _, _, .rcd hb, h, hΓ, hc, hq => fun σ hab hσ Γ' hr =>
      .qRcd (inferRecSound_of hvar hlet hb h hΓ hc hq σ hab hσ Γ' hr)
  | _, _, _, _, _, .letE (S₁ := S₁) (Δγ := Δγ) h₁ hA hsplit hbq hγ hfresh hown hres hdis hdom hind h₂,
      h, hΓ, hc, hq => by
      obtain ⟨i₁, -⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨i₁', hΓ'⟩ := let_body_inv (x := _) (τ₁ := _) i₁ hΓ hsplit hres
      have c₁ := Infer.clean h₁ hc
      have q₁ : ({ S₁ with parked := Δγ } : SolverState B).Quiescent :=
        quiescent_sub (Infer.quiescent h₁ hq)
          (fun p hp => by exact hsplit.mem_iff.mpr <| List.mem_append_right _ hp)
      exact hlet h₁ hA hsplit hbq hγ hfresh hown hres hdis hdom hind h₂ h hΓ hc hq
        (inferSound_of hvar hlet h₁ h hΓ hc hq) (inferSound_of hvar hlet h₂ i₁' hΓ' c₁ q₁)

theorem inferRecSound_of {B C : Type} [DecidableEq B] {constTy : C → B}
    (hvar : VarCase B C constTy) (hlet : LetCase B C constTy) :
    {Γ : QCtx B} → {S S' : SolverState B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    InferRec constTy Γ S ξ ρ S' → S.PInv → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAtRec constTy Γ ξ ρ S'
  | _, _, _, _, _, .empty, _, _, _, _ => fun _ _ _ _ _ => .empty
  | _, _, _, _, _, .field h₁, h, hΓ, hc, hq => fun σ hab hσ Γ' hr =>
      .field (inferSound_of hvar hlet h₁ h hΓ hc hq σ hab hσ Γ' hr)
  | _, _, _, _, _, .cat h₁ h₂, h, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨i₁, -⟩ := InferRec.pinv_keeps h₁ h hΓ
      obtain ⟨-, k₂⟩ := InferRec.pinv_keeps h₂ i₁ hΓ
      have c₁ := InferRec.clean h₁ hc
      have hσ₁ := InferRec.sat_mono h₂ σ hσ
      exact .cat (k₂.liftBody hσ hr (inferRecSound_of hvar hlet h₁ h hΓ hc hq σ
          (hab.back (InferRec.ext h₂) c₁) hσ₁ Γ' hr))
        (inferRecSound_of hvar hlet h₂ i₁ hΓ c₁ (InferRec.quiescent h₁ hq) σ hab hσ Γ' hr)

end

/-- ⊢  so `InferSound` itself follows from the two case obligations. -/
theorem inferSound_of_cases {B C : Type} [DecidableEq B] {constTy : C → B}
    (hvar : VarCase B C constTy) (hlet : LetCase B C constTy) :
    InferSound B C constTy :=
  fun _ _ _ _ _ h => inferSound_of hvar hlet h

/-- ⊢  **…and A-var is discharged: `InferSound` rests on A-let alone.** -/
theorem inferSound_of_let {B C : Type} [DecidableEq B] {constTy : C → B}
    (hlet : LetCase B C constTy) : InferSound B C constTy :=
  inferSound_of_cases varCase hlet

--------------------- …AND WHERE IT JOINS -------------------------------------
/-- ⊢  **the join**: a run from nothing whose parked stumps all hold at σ is a
plain L2 typing at σ. The `hfin` premise is what finalization supplies (F-★ at
⟦S′⟧ — `Finalizes.holds`); the χ-correction is `QScheme.Correctable.correct`.
No transport along χ is left: `QTypedA.weaken` did the cashing-in. -/
theorem runSoundA_of {B C : Type} [DecidableEq B] {constTy : C → B}
    (hs : InferSound B C constTy)
    {e : Expr C} {τ : Ty B} {S₁ : SolverState B}
    (h : Infer constTy ⟨[], []⟩ ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁)
    {σ : TySubst B} (hab : Absorbs σ S₁) (hsat : Sol.Sat σ S₁.sol)
    (hfin : ∀ p ∈ S₁.parked, (p.stump.at σ).Holds (⟨[], []⟩ : Ctx B)) :
    QTyped constTy ⟨[], []⟩ e (τ.applySubst σ) :=
  (hs _ _ _ _ _ h .init .nil Sol.clean_nil (SolverState.Quiescent.nil rfl) σ hab hsat _
    (CtxRead.nil σ)).toQTyped (fun _ _ h => nomatch h) (fun a ha => by
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    exact hfin p hp)

end MinimalCalculus
