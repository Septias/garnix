-- THE SOUNDNESS STATEMENT, RESTATED.

import ParkedInv
import Absorb

namespace MinimalCalculus

/-- `ρ.q ↓ τ` assumed: `e : {ρ}` is taken to give `e.l : τ` (q = ⌊l⌋), or a
dynamic selection keyed by q its type. -/
structure Assume (B : Type) where
  row   : Row B
  label : Key
  ty    : Ty B

/-- a stump read under σ — row, key AND result. -/
def Stump.at {B : Type} (σ : TySubst B) (st : Stump B) : Assume B :=
  ⟨st.row.applySubst σ, st.label.applySubst σ, st.res.applySubst σ⟩

/-- the assumption is true: the lookup, performed, agrees — found up to ≈,
or ⊥/? at ★. `Stump.Discharge` is exactly this at `st.at θ`. -/
inductive Assume.Holds {B : Type} (a : Assume B) : Prop where
  | hit {τ : Ty B} : LookupQ a.row a.label (.found τ) → TyEquiv a.ty τ → Holds a
  | abs : LookupQ a.row a.label .absent → a.ty = .unk → Holds a
  | unk : LookupQ a.row a.label .unknown → a.ty = .unk → Holds a

-- ⊢  a stump discharges at θ (up to ≈) iff its θ-reading holds
theorem Stump.dischargeEquiv_iff_holds {B : Type} {θ : TySubst B}
    {st : Stump B} : st.Discharge θ ↔ (st.at θ).Holds :=
  ⟨fun | .hit h e => .hit h e | .abs h e => .abs h e | .unk h e => .unk h e,
   fun | .hit h e => .hit h e | .abs h e => .abs h e | .unk h e => .unk h e⟩

/-- instantiation under assumptions: every constraint discharges (up to ≈) or
its reading is assumed. -/
def QScheme.InstA {B : Type} (Δ : List (Assume B)) (sc : QScheme B)
    (τ : Ty B) : Prop :=
  ∃ χ : TySubst B, χ.FixedOutside sc.vars ∧
    (∀ st ∈ sc.constraints, st.Discharge χ ∨ st.at χ ∈ Δ) ∧
    sc.body.applySubst χ = τ

mutual
  /-- `Δ; Γ ⊢ e : τ` — L2 typing under assumed lookups. Δ is an INDEX: qLet's
  premise extends it with the scheme's own constraints, read at the instance. -/
  inductive QTypedA {B C : Type} (constTy : C → B) :
      List (Assume B) → QCtx B → Expr C → Ty B → Prop where
    | qCon : QTypedA constTy Δ Γ (.con c) (.base (constTy c))
    | qVar : Γ.lookup x = some σ → QScheme.InstA Δ σ τ →
             QTypedA constTy Δ Γ (.var x) τ
    | qEq  : QTypedA constTy Δ Γ e τ₁ → TyEquiv τ₁ τ₂ → QTypedA constTy Δ Γ e τ₂
    | qLam : QTypedA constTy Δ (Γ.bindTy x τ₁) e τ₂ →
             QTypedA constTy Δ Γ (.lam x e) (.fn τ₁ τ₂)
    | qApp : QTypedA constTy Δ Γ e₁ (.fn τ₁ τ₂) → QTypedA constTy Δ Γ e₂ τ₁ →
             QTypedA constTy Δ Γ (.app e₁ e₂) τ₂
    -- e₁ at EVERY instance, assuming the constraints there. That is what
    -- generalization produces; which instances discharge is decided at the use.
    | qLet : σ.WF →
             (∀ χ : TySubst B, χ.FixedOutside σ.vars →
               QTypedA constTy (σ.constraints.map (Stump.at χ) ++ Δ) Γ e₁
                 (σ.body.applySubst χ)) →
             (∃ τ₁, QScheme.Inst σ τ₁) →
             QTypedA constTy Δ (Γ.bindScheme x σ) e₂ τ₂ →
             QTypedA constTy Δ Γ (.letE x e₁ e₂) τ₂
    | qCat : QTypedA constTy Δ Γ e₁ (.rcd ρ₁) → QTypedA constTy Δ Γ e₂ (.rcd ρ₂) →
             QTypedA constTy Δ Γ (.cat e₁ e₂) (.rcd (.cat ρ₂ ρ₁))
    | qSel : QTypedA constTy Δ Γ e (.rcd ρ) → Lookup ρ l (.found τ) →
             QTypedA constTy Δ Γ (.sel e l) τ
    | qSelUnk : QTypedA constTy Δ Γ e (.rcd ρ) → Lookup ρ l .unknown →
                QTypedA constTy Δ Γ (.sel e l) .unk
    | qSelAbs : QTypedA constTy Δ Γ e (.rcd ρ) → Lookup ρ l .absent →
                QTypedA constTy Δ Γ (.sel e l) .unk
    | qUnk : QTypedA constTy Δ Γ e τ → QTypedA constTy Δ Γ e .unk
    | qRcd : QTypedABody constTy Δ Γ b ρ → QTypedA constTy Δ Γ (.rcd b) (.rcd ρ)
    -- A-sel-? read declaratively: the lookup's answer is assumed
    | assume {ρ : Row B} {l : Label} {τ : Ty B} :
             QTypedA constTy Δ Γ e (.rcd ρ) → (⟨ρ, .lit l, τ⟩ : Assume B) ∈ Δ →
             QTypedA constTy Δ Γ (.sel e l) τ
    -- FC-labels, as in `QTyped`
    | qLab : QTypedA constTy Δ Γ (.lab l) (.lab (.lit l))
    | qSelDyn : QTypedA constTy Δ Γ e₁ (.rcd ρ) → QTypedA constTy Δ Γ e₂ (.lab q) →
                LookupQ ρ q (.found τ) → QTypedA constTy Δ Γ (.selDyn e₁ e₂) τ
    | qSelDynUnk : QTypedA constTy Δ Γ e₁ (.rcd ρ) → QTypedA constTy Δ Γ e₂ (.lab q) →
                   LookupQ ρ q .unknown → QTypedA constTy Δ Γ (.selDyn e₁ e₂) .unk
    | qSelDynAbs : QTypedA constTy Δ Γ e₁ (.rcd ρ) → QTypedA constTy Δ Γ e₂ (.lab q) →
                   LookupQ ρ q .absent → QTypedA constTy Δ Γ (.selDyn e₁ e₂) .unk
    | qRcdDyn : QTypedA constTy Δ Γ e₁ (.lab q) → QTypedA constTy Δ Γ e₂ τ →
                QTypedA constTy Δ Γ (.rcdDyn e₁ e₂) (.rcd (.dsing q τ))
    | qSelDynBase : QTypedA constTy Δ Γ e₁ (.rcd ρ) → QTypedA constTy Δ Γ e₂ (.base b) →
                    QTypedA constTy Δ Γ (.selDyn e₁ e₂) .unk
    | qRcdDynBase : QTypedA constTy Δ Γ e₁ (.base b) → QTypedA constTy Δ Γ e₂ τ →
                    QTypedA constTy Δ Γ (.rcdDyn e₁ e₂) .unk
    -- A-sel-dyn-? read declaratively: the keyed lookup's answer is assumed
    | assumeDyn {ρ : Row B} {q : Key} {τ : Ty B} :
             QTypedA constTy Δ Γ e₁ (.rcd ρ) → QTypedA constTy Δ Γ e₂ (.lab q) →
             (⟨ρ, q, τ⟩ : Assume B) ∈ Δ → QTypedA constTy Δ Γ (.selDyn e₁ e₂) τ

  inductive QTypedABody {B C : Type} (constTy : C → B) :
      List (Assume B) → QCtx B → RecBody (Expr C) → Row B → Prop where
    | empty : QTypedABody constTy Δ Γ .empty .empty
    | field : QTypedA constTy Δ Γ e τ →
              QTypedABody constTy Δ Γ (.field l e) (.sing l τ)
    | cat : QTypedABody constTy Δ Γ b₁ ρ₁ → QTypedABody constTy Δ Γ b₂ ρ₂ →
            QTypedABody constTy Δ Γ (.cat b₁ b₂) (.cat ρ₁ ρ₂)
end

-- a held keyed assumption, used at a dynamic selection, is one of T-sel-dyn's
private theorem selDyn_of_holds {B C : Type} {constTy : C → B} {Δ : List (Assume B)}
    {Γ : QCtx B} {e₁ e₂ : Expr C} {ρ : Row B} {q : Key} {τ : Ty B}
    (h₁ : QTypedA constTy Δ Γ e₁ (.rcd ρ)) (h₂ : QTypedA constTy Δ Γ e₂ (.lab q))
    (hh : (⟨ρ, q, τ⟩ : Assume B).Holds) :
    QTypedA constTy Δ Γ (.selDyn e₁ e₂) τ := by
  cases hh with
  | hit hl he => exact .qEq (.qSelDyn h₁ h₂ hl) he.symm
  | abs hl he => cases he; exact .qSelDynAbs h₁ h₂ hl
  | unk hl he => cases he; exact .qSelDynUnk h₁ h₂ hl

-- a held assumption, used at a selection, is one of the three T-sel rules
private theorem sel_of_holds {B C : Type} {constTy : C → B} {Δ : List (Assume B)}
    {Γ : QCtx B} {e : Expr C} {ρ : Row B} {l : Label} {τ : Ty B}
    (h : QTypedA constTy Δ Γ e (.rcd ρ)) (hh : (⟨ρ, .lit l, τ⟩ : Assume B).Holds) :
    QTypedA constTy Δ Γ (.sel e l) τ := by
  cases hh with
  | hit hl he => exact .qEq (.qSel h (LookupQ.lab_iff.mp hl)) he.symm
  | abs hl he => cases he; exact .qSelAbs h (LookupQ.lab_iff.mp hl)
  | unk hl he => cases he; exact .qSelUnk h (LookupQ.lab_iff.mp hl)

mutual
  /-- ⊢  **held assumptions can be dropped.** Monotonicity for the induction —
  an assumption from an earlier state is either still parked or has been woken,
  and a woken one holds — and, at Δ₂ = [], the cash-in at the end of a run. -/
  theorem QTypedA.weaken {B C : Type} {constTy : C → B} :
      {Δ₁ Δ₂ : List (Assume B)} → {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} →
      QTypedA constTy Δ₁ Γ e τ →
      (∀ a ∈ Δ₁, a ∈ Δ₂ ∨ a.Holds) → QTypedA constTy Δ₂ Γ e τ
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
    | _, _, _, _, _, .qLab, _ => .qLab
    | _, _, _, _, _, .qSelDyn h₁ h₂ hl, hΔ =>
        .qSelDyn (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ) hl
    | _, _, _, _, _, .qSelDynUnk h₁ h₂ hl, hΔ =>
        .qSelDynUnk (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ) hl
    | _, _, _, _, _, .qSelDynAbs h₁ h₂ hl, hΔ =>
        .qSelDynAbs (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ) hl
    | _, _, _, _, _, .qRcdDyn h₁ h₂, hΔ =>
        .qRcdDyn (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ)
    | _, _, _, _, _, .qSelDynBase h₁ h₂, hΔ =>
        .qSelDynBase (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ)
    | _, _, _, _, _, .qRcdDynBase h₁ h₂, hΔ =>
        .qRcdDynBase (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ)
    | _, _, _, _, _, .assumeDyn h₁ h₂ hm, hΔ =>
        match hΔ _ hm with
        | .inl hm' => .assumeDyn (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ) hm'
        | .inr hh  => selDyn_of_holds (QTypedA.weaken h₁ hΔ) (QTypedA.weaken h₂ hΔ) hh

  theorem QTypedABody.weaken {B C : Type} {constTy : C → B} :
      {Δ₁ Δ₂ : List (Assume B)} → {Γ : QCtx B} → {b : RecBody (Expr C)} →
      {ρ : Row B} → QTypedABody constTy Δ₁ Γ b ρ →
      (∀ a ∈ Δ₁, a ∈ Δ₂ ∨ a.Holds) → QTypedABody constTy Δ₂ Γ b ρ
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
-- With every assumption held, a `QTypedA` derivation is a `QTyped` one. At
-- `qVar` the instance χ already IS a `QScheme.Inst` witness: an assumed
-- constraint holds, i.e. discharges, and D-hit asks only ≈. (With an exact
-- D-hit this step needed a χ-correction, and with it linear-pattern, nodup,
-- disjoint-result and independence side conditions on every let-scheme.)

mutual
  /-- ⊢  **cash-in**: all assumptions held ⟹ a plain L2 typing. -/
  theorem QTypedA.toQTyped {B C : Type} {constTy : C → B} :
      {Δ : List (Assume B)} → {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} →
      QTypedA constTy Δ Γ e τ → (∀ a ∈ Δ, a.Holds) →
      QTyped constTy Γ e τ
    | _, _, _, _, .qCon, _ => .qCon
    | _, _, _, _, .qVar hl ⟨χ, hfix, hc, hb⟩, hΔ =>
        .qVar hl ⟨χ, hfix, fun st hst =>
          match hc st hst with
          | .inl hd => hd
          | .inr hm => Stump.dischargeEquiv_iff_holds.mpr (hΔ _ hm), hb⟩
    | _, _, _, _, .qEq h he, hΔ => .qEq (QTypedA.toQTyped h hΔ) he
    | _, _, _, _, .qLam h, hΔ => .qLam (QTypedA.toQTyped h hΔ)
    | _, _, _, _, .qApp h₁ h₂, hΔ =>
        .qApp (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ)
    | _, _, _, _, .qLet hcs hi hin hb, hΔ =>
        .qLet hcs (fun τ₁ ⟨χ, hfix, hdis, hbody⟩ => hbody ▸
            QTypedA.toQTyped (hi χ hfix) (fun a ha =>
              match List.mem_append.mp ha with
              | .inl hc => by
                  obtain ⟨st, hst, rfl⟩ := List.mem_map.mp hc
                  exact Stump.dischargeEquiv_iff_holds.mp (hdis st hst)
              | .inr hd => hΔ a hd))
          hin (QTypedA.toQTyped hb hΔ)
    | _, _, _, _, .qCat h₁ h₂, hΔ =>
        .qCat (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ)
    | _, _, _, _, .qSel h hl, hΔ => .qSel (QTypedA.toQTyped h hΔ) hl
    | _, _, _, _, .qSelUnk h hl, hΔ => .qSelUnk (QTypedA.toQTyped h hΔ) hl
    | _, _, _, _, .qSelAbs h hl, hΔ => .qSelAbs (QTypedA.toQTyped h hΔ) hl
    | _, _, _, _, .qUnk h, hΔ => .qUnk (QTypedA.toQTyped h hΔ)
    | _, _, _, _, .qRcd h, hΔ => .qRcd (QTypedABody.toQTyped h hΔ)
    | _, _, _, _, .assume h hm, hΔ => by
        have h' := QTypedA.toQTyped h hΔ
        cases hΔ _ hm with
        | hit hl he => exact .qEq (.qSel h' (LookupQ.lab_iff.mp hl)) he.symm
        | abs hl he => cases he; exact .qSelAbs h' (LookupQ.lab_iff.mp hl)
        | unk hl he => cases he; exact .qSelUnk h' (LookupQ.lab_iff.mp hl)
    | _, _, _, _, .qLab, _ => .qLab
    | _, _, _, _, .qSelDyn h₁ h₂ hl, hΔ =>
        .qSelDyn (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ) hl
    | _, _, _, _, .qSelDynUnk h₁ h₂ hl, hΔ =>
        .qSelDynUnk (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ) hl
    | _, _, _, _, .qSelDynAbs h₁ h₂ hl, hΔ =>
        .qSelDynAbs (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ) hl
    | _, _, _, _, .qRcdDyn h₁ h₂, hΔ =>
        .qRcdDyn (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ)
    | _, _, _, _, .qSelDynBase h₁ h₂, hΔ =>
        .qSelDynBase (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ)
    | _, _, _, _, .qRcdDynBase h₁ h₂, hΔ =>
        .qRcdDynBase (QTypedA.toQTyped h₁ hΔ) (QTypedA.toQTyped h₂ hΔ)
    | _, _, _, _, .assumeDyn h₁ h₂ hm, hΔ => by
        have h₁' := QTypedA.toQTyped h₁ hΔ
        have h₂' := QTypedA.toQTyped h₂ hΔ
        cases hΔ _ hm with
        | hit hl he => exact .qEq (.qSelDyn h₁' h₂' hl) he.symm
        | abs hl he => cases he; exact .qSelDynAbs h₁' h₂' hl
        | unk hl he => cases he; exact .qSelDynUnk h₁' h₂' hl

  theorem QTypedABody.toQTyped {B C : Type} {constTy : C → B} :
      {Δ : List (Assume B)} → {Γ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
      QTypedABody constTy Δ Γ b ρ → (∀ a ∈ Δ, a.Holds) →
      QTypedBody constTy Γ b ρ
    | _, _, _, _, .empty, _ => .empty
    | _, _, _, _, .field h, hΔ => .field (QTypedA.toQTyped h hΔ)
    | _, _, _, _, .cat h₁ h₂, hΔ =>
        .cat (QTypedABody.toQTyped h₁ hΔ) (QTypedABody.toQTyped h₂ hΔ)
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
   fun α => if α ∈ vs then .var (f α) else σ.row α,
   fun α => if α ∈ vs then .var (f α) else σ.lab α⟩

/-- `sc` read under σ, binders renamed by f. -/
def QScheme.readAt {B : Type} (sc : QScheme B) (σ : TySubst B) (f : TyVar → TyVar) :
    QScheme B :=
  ⟨sc.vars.map f,
   sc.constraints.map (fun st =>
     (⟨st.row.applySubst (readSub σ sc.vars f), st.label.applySubst (readSub σ sc.vars f),
       st.res.applySubst (readSub σ sc.vars f)⟩ : Stump B)),
   sc.body.applySubst (readSub σ sc.vars f)⟩

/-- `sc′` is `sc` read under σ, with binders renamed apart by an injective f
whose names σ's image of the free variables does not reach. -/
def SchemeRead {B : Type} (σ : TySubst B) (sc sc' : QScheme B) : Prop :=
  ∃ f : TyVar → TyVar,
    (∀ α ∈ sc.vars, ∀ β ∈ sc.vars, f α = f β → α = β) ∧
    (∀ α ∈ sc.freeFtv, α ∉ sc.vars →
       (∀ β ∈ (σ.ty α).ftv, β ∉ sc.vars.map f) ∧
       (∀ β ∈ (σ.row α).ftv, β ∉ sc.vars.map f) ∧
       (∀ β ∈ (σ.lab α).ftv, β ∉ sc.vars.map f)) ∧
    sc' = sc.readAt σ f

/-- `Γ′` is `Γ` read under σ: each scheme read. (It used to demand an empty row
environment as well; there is no row environment any more.) -/
structure CtxRead {B : Type} (σ : TySubst B) (Γ Γ' : QCtx B) : Prop where
  schem : ∀ x sc, Γ.lookup x = some sc →
            ∃ sc', Γ'.lookup x = some sc' ∧ SchemeRead σ sc sc'

theorem CtxRead.nil {B : Type} (σ : TySubst B) :
    CtxRead σ (QCtx.empty : QCtx B) QCtx.empty :=
  ⟨fun _ _ h => nomatch h⟩

-- ⊢  a monotype is read by substituting: nothing to rename
theorem SchemeRead.mono {B : Type} (σ : TySubst B) (τ : Ty B) :
    SchemeRead σ ⟨[], [], τ⟩ ⟨[], [], τ.applySubst σ⟩ := by
  refine ⟨id, fun _ h => absurd h List.not_mem_nil,
    fun _ _ _ => ⟨fun _ _ => by simp, fun _ _ => by simp, fun _ _ => by simp⟩, ?_⟩
  have hid : τ.applySubst (readSub σ ([] : List TyVar) id) = τ.applySubst σ :=
    Ty.applySubst_congr τ (fun _ _ => ⟨by simp [readSub], by simp [readSub], by simp [readSub]⟩)
  simp [QScheme.readAt, hid]

theorem CtxRead.bindScheme {B : Type} {σ : TySubst B} {Γ Γ' : QCtx B}
    (h : CtxRead σ Γ Γ') (x : Var) {sc sc' : QScheme B} (hs : SchemeRead σ sc sc') :
    CtxRead σ (Γ.bindScheme x sc) (Γ'.bindScheme x sc') where
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
    (hr : SchemeRead σ sc sc') (hwf : sc.WF)
    (hag : ∀ α ∈ sc.freeFtv, α ∉ sc.vars →
      σ₁.ty α = σ.ty α ∧ σ₁.row α = σ.row α ∧ σ₁.lab α = σ.lab α) :
    SchemeRead σ₁ sc sc' := by
  obtain ⟨f, hinj, hav, rfl⟩ := hr
  have hsub : ∀ α ∈ sc.freeFtv,
      (readSub σ₁ sc.vars f).ty α = (readSub σ sc.vars f).ty α ∧
      (readSub σ₁ sc.vars f).row α = (readSub σ sc.vars f).row α ∧
      (readSub σ₁ sc.vars f).lab α = (readSub σ sc.vars f).lab α := by
    intro α hα
    by_cases hv : α ∈ sc.vars
    · simp [readSub, hv]
    · simp [readSub, hv, (hag α hα hv).1, (hag α hα hv).2.1, (hag α hα hv).2.2]
  refine ⟨f, hinj, fun α hα hv => ?_, ?_⟩
  · rw [(hag α hα hv).1, (hag α hα hv).2.1, (hag α hα hv).2.2]; exact hav α hα hv
  · simp only [QScheme.readAt]
    congr 1
    · apply List.map_congr_left
      intro st hst
      rw [Row.applySubst_congr st.row (fun α hα =>
        hsub α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_left _ hα⟩))),
        Key.applySubst_congr st.label (fun α hα =>
        (hsub α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_right _ hα⟩))).2.2)]
      -- the result mentions binders only, which both readings rename alike
      congr 1
      exact Ty.applySubst_congr st.res (fun α hα => by
        simp [readSub, hwf st hst α hα])
    · exact Ty.applySubst_congr _ (fun α hα =>
        ⟨(hsub α (List.mem_append_right _ hα)).1.symm, (hsub α (List.mem_append_right _ hα)).2.1.symm,
         (hsub α (List.mem_append_right _ hα)).2.2.symm⟩)

--------------------- FRESH BINDER NAMES EXIST ---------------------------------
-- Append a long enough run of `a`s: the result is longer than every name to
-- avoid, and appending the same suffix is injective.

theorem fresh_renaming_exists {B : Type} (σ : TySubst B) (vs L : List TyVar) :
    ∃ f : TyVar → TyVar,
      (∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β) ∧
      (∀ α ∈ vs, f α ∉ L) ∧
      (∀ α ∈ L, (∀ β ∈ (σ.ty α).ftv, β ∉ vs.map f) ∧
                (∀ β ∈ (σ.row α).ftv, β ∉ vs.map f) ∧
                (∀ β ∈ (σ.lab α).ftv, β ∉ vs.map f)) := by
  let A : List TyVar := L ++ L.flatMap (fun α => (σ.ty α).ftv ++ (σ.row α).ftv ++ (σ.lab α).ftv)
  let N := lenBound A + 1
  refine ⟨fun α => α ++ natName N, fun α _ β _ h => (String.append_left_inj _).mp h,
    fun α _ hm => ?_, fun α hα => ⟨fun β hβ hm => ?_, fun β hβ hm => ?_, fun β hβ hm => ?_⟩⟩
  · have := length_le_lenBound (l := A) (List.mem_append_left _ hm)
    simp [String.length_append, natName_length, N] at this; omega
  · obtain ⟨γ, -, rfl⟩ := List.mem_map.mp hm
    have := length_le_lenBound (l := A) (List.mem_append_right _
      (List.mem_flatMap.mpr ⟨α, hα, List.mem_append_left _ (List.mem_append_left _ hβ)⟩))
    simp [String.length_append, natName_length, N] at this; omega
  · obtain ⟨γ, -, rfl⟩ := List.mem_map.mp hm
    have := length_le_lenBound (l := A) (List.mem_append_right _
      (List.mem_flatMap.mpr ⟨α, hα, List.mem_append_left _ (List.mem_append_right _ hβ)⟩))
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
   fun β => if β ∈ vs.map f then κ.row (finv vs f β) else .var β,
   fun β => if β ∈ vs.map f then κ.lab (finv vs f β) else .var β⟩

private theorem readInst_agree {B : Type} {vs : List TyVar} {f : TyVar → TyVar}
    {σ κ : TySubst B}
    (hinj : ∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β)
    (hκ : ∀ α, α ∉ vs → κ.ty α = σ.ty α ∧ κ.row α = σ.row α ∧ κ.lab α = σ.lab α)
    {α : TyVar}
    (hav : α ∉ vs → (∀ β ∈ (σ.ty α).ftv, β ∉ vs.map f) ∧
                    (∀ β ∈ (σ.row α).ftv, β ∉ vs.map f) ∧
                    (∀ β ∈ (σ.lab α).ftv, β ∉ vs.map f)) :
    ((readInst vs f κ).comp (readSub σ vs f)).ty α = κ.ty α ∧
    ((readInst vs f κ).comp (readSub σ vs f)).row α = κ.row α ∧
    ((readInst vs f κ).comp (readSub σ vs f)).lab α = κ.lab α := by
  have hmem : ∀ a ∈ vs, f a ∈ vs.map f := fun a ha => List.mem_map_of_mem ha
  by_cases hα : α ∈ vs
  · refine ⟨?_, ?_, ?_⟩
    · simp only [TySubst.comp, readSub, if_pos hα, Ty.applySubst]
      simp only [readInst, if_pos (hmem α hα), finv_f hinj hα]
    · simp only [TySubst.comp, readSub, if_pos hα, Row.applySubst]
      simp only [readInst, if_pos (hmem α hα), finv_f hinj hα]
    · simp only [TySubst.comp, readSub, if_pos hα, Key.applySubst_var]
      simp only [readInst, if_pos (hmem α hα), finv_f hinj hα]
  · obtain ⟨hvt, hvr, hvl⟩ := hav hα
    have fix : ∀ β, β ∉ vs.map f → (readInst vs f κ).ty β = .var β ∧
        (readInst vs f κ).row β = .var β ∧ (readInst vs f κ).lab β = .var β := fun β hβ =>
      ⟨by simp only [readInst, if_neg hβ], by simp only [readInst, if_neg hβ],
       by simp only [readInst, if_neg hβ]⟩
    refine ⟨?_, ?_, ?_⟩
    · simp only [TySubst.comp, readSub, if_neg hα]
      rw [(hκ α hα).1]
      exact Ty.applySubst_fixed_ftv _ (fun β hβ => fix β (hvt β hβ))
    · simp only [TySubst.comp, readSub, if_neg hα]
      rw [(hκ α hα).2.1]
      exact Row.applySubst_fixed_ftv _ (fun β hβ => fix β (hvr β hβ))
    · simp only [TySubst.comp, readSub, if_neg hα]
      rw [(hκ α hα).2.2]
      cases hk : σ.lab α with
      | lit _ => rfl
      | var γ => exact (fix γ (hvl γ (by simp [hk, Key.ftv]))).2.2

/-- ⊢  **every instance of the scheme that agrees with σ off its binders is an
instance of the read scheme** — body and constraint readings on the nose. -/
theorem SchemeRead.instAt {B : Type} {σ κ : TySubst B} {sc sc' : QScheme B}
    (hr : SchemeRead σ sc sc') (hwf : sc.WF)
    (hκ : ∀ α, α ∉ sc.vars → κ.ty α = σ.ty α ∧ κ.row α = σ.row α ∧ κ.lab α = σ.lab α) :
    ∃ χ : TySubst B, χ.FixedOutside sc'.vars ∧
      sc'.body.applySubst χ = sc.body.applySubst κ ∧
      sc'.constraints.map (Stump.at χ) = sc.constraints.map (Stump.at κ) := by
  obtain ⟨f, hinj, hav, rfl⟩ := hr
  refine ⟨readInst sc.vars f κ, ⟨fun β hβ => ?_, fun β hβ => ?_, fun β hβ => ?_⟩, ?_, ?_⟩
  · have hβ' : β ∉ sc.vars.map f := hβ
    simp only [readInst, if_neg hβ']
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
    simp only [Function.comp, Stump.at]
    congr 1
    · rw [Row.applySubst_applySubst]
      exact Row.applySubst_congr _ (fun α hα => readInst_agree hinj hκ
        (hav α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_left _ hα⟩))))
    · rw [Key.applySubst_applySubst]
      exact Key.applySubst_congr _ (fun α hα => (readInst_agree hinj hκ
        (hav α (List.mem_append_left _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_right _ hα⟩)))).2.2)
    · rw [Ty.applySubst_applySubst]
      exact Ty.applySubst_congr _ (fun α hα =>
        readInst_agree hinj hκ (fun h => absurd (hwf st hst α hα) h))

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
     by show (θ.row α).applySubst σ = _; rw [hθ.1.2.1 α hα]; rfl,
     by show (θ.lab α).applySubst σ = _; rw [hθ.1.2.2 α hα]; rfl⟩)
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
  · rw [Key.applySubst_applySubst]
  · rw [Ty.applySubst_applySubst]

/-- A-var, given what wake-up did with the instantiated constraints: each one
either holds (it was discharged) or is assumed (it is still parked). -/
theorem inferA_sound_var_step {B C : Type} {constTy : C → B} {σ θ : TySubst B}
    {Γ Γ' : QCtx B} {x : Var} {sc : QScheme B} {g : TyVar → TyVar}
    {ps : List (Parked B)} {Δ : List (Assume B)}
    (hread : CtxRead σ Γ Γ') (hl : Γ.lookup x = some sc) (hwf : sc.WF)
    (hθ : IsRenaming θ sc.vars g) (hps : InstStumps θ g sc.constraints ps)
    (hcov : ∀ p ∈ ps, (p.stump.at σ).Holds ∨ p.stump.at σ ∈ Δ) :
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
start state is clean and quiescent, and Γ's
schemes are well-formed — all trivially true of a run from nothing. -/
def InferSound (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ (Γ : QCtx B) (S S' : SolverState B) (e : Expr C) (τ : Ty B),
    Infer constTy Γ S e τ S' → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
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
    rcases hk σ hσ p hp with ⟨q, hq, hqs⟩ | hd
    · exact .inl (List.mem_map.mpr ⟨q, hq, by rw [hqs]⟩)
    · exact .inr (Stump.dischargeEquiv_iff_holds.mp hd))

--------------------- THE INDUCTION, MODULO A-var AND A-let --------------------
-- The two cases whose own obligations are elsewhere are hypotheses, stated as
-- the case with its induction hypotheses. The parked-list bookkeeping is no
-- longer one: `Infer.keeps` (ParkedInv.lean) proves it.

/-- the A-var case. `inferA_sound_var_step` is its typing half. -/
def VarCase (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ {Γ : QCtx B} {S S' : SolverState B} {x : Var} {sc : QScheme B}
    {θ : TySubst B} {f : TyVar → TyVar} {ps : List (Parked B)} {Sup : Supply} {K : KEnv},
    Γ.lookup x = some sc → IsRenaming θ sc.vars f → FreshRenaming f sc.vars Γ S →
    (∀ α ∈ sc.vars, ∃ k, S.supply.next ≤ k ∧ k < Sup.next ∧ f α = natName k) →
    S.supply.next ≤ Sup.next →
    InstStumps θ f sc.constraints ps → WakesSat { S with supply := Sup, kinds := K } ps S' →
    Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ (.var x) (sc.body.applySubst θ) S'

/-- the A-let case, given both induction hypotheses. -/
def LetCase (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ {Γ : QCtx B} {S S₁ S₂ : SolverState B} {x : Var} {e₁ e₂ : Expr C}
    {τ₁ τ₂ : Ty B} {ᾱ : List TyVar},
    Infer constTy Γ S e₁ τ₁ S₁ →
    LetAdmissible Γ S S₁ ᾱ →
    Infer constTy (Γ.bindScheme x (letScheme S₁ ᾱ (letQ S₁ ᾱ) τ₁))
      { S₁ with parked := letG S₁ ᾱ } e₂ τ₂ S₂ →
    Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ e₁ τ₁ S₁ →
    SoundAt constTy (Γ.bindScheme x (letScheme S₁ ᾱ (letQ S₁ ᾱ) τ₁)) e₂ τ₂ S₂ →
    SoundAt constTy Γ (.letE x e₁ e₂) τ₂ S₂

/-- ⊢  the let scheme is well formed: `LetResults` is exactly `QScheme.WF` read
off the generalized stumps. -/
theorem letScheme_wf {B : Type} [DecidableEq B] {Γ : QCtx B} {S₁ : SolverState B}
    {x : Var} {τ₁ : Ty B} {Δq : List (Parked B)} {ᾱ : List TyVar}
    (hΓ : Γ.SchemesWF) (hres : LetResults S₁ ᾱ Δq) :
    (Γ.bindScheme x (letScheme S₁ ᾱ Δq τ₁)).SchemesWF := by
  refine hΓ.bindScheme x ?_
  intro st hst
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst
  exact hres.1 p hp

private theorem draw_eqs {B : Type} {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.sol = S.sol ∧ S₀.parked = S.parked := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  subst h2; exact ⟨rfl, rfl⟩

/-- ⊢  **the A-var case, proved.** Wake-up either discharges an instantiated
constraint or leaves it parked; saturation keeps it that way or discharges it
later. Either way `inferA_sound_var_step`'s `hcov` holds at the final state. -/
theorem varCase {B C : Type} [DecidableEq B] {constTy : C → B} :
    VarCase B C constTy := by
  intro Γ S S' x sc θ f ps Sup K hl hθ hfr hdr hle hps hw hΓ _ _ σ _ hσ Γ' hr
  obtain ⟨S₁, hws, hsat⟩ := hw
  have hwf := hΓ x sc hl
  have k₂ := hsat.keeps
  have hfate := hws.fate σ (hsat.satMono σ hσ)
  refine inferA_sound_var_step hr hl hwf hθ hps (fun p hp => ?_)
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
  unfold SolverState.subst at this ⊢
  rw [hs]; exact this

private theorem quiescent_sub {B : Type} {S : SolverState B} {Δ : List (Parked B)}
    (hq : S.Quiescent) (hs : ∀ p ∈ Δ, p ∈ S.parked) :
    ({ S with parked := Δ } : SolverState B).Quiescent :=
  fun p hp => hq p (hs p hp)

mutual

/-- ⊢  **`InferSound` is inductive**: every rule but A-var and A-let, with those
two as hypotheses. The parked-list bookkeeping is proved (`Infer.keeps`),
and absorption is carried back to each premise's state (`Absorbs.back`). -/
theorem inferSound_of {B C : Type} [DecidableEq B] {constTy : C → B}
    (hvar : VarCase B C constTy) (hlet : LetCase B C constTy) :
    {Γ : QCtx B} → {S S' : SolverState B} → {e : Expr C} → {τ : Ty B} →
    Infer constTy Γ S e τ S' → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAt constTy Γ e τ S'
  | _, _, _, _, _, .con, _, _, _ => fun _ _ _ _ _ => .qCon
  | _, _, _, _, _, .var hl hθ hfr hdr hle _ hps hw, hΓ, hc, hq =>
      hvar hl hθ hfr hdr hle hps hw hΓ hc hq
  | _, _, _, _, _, .lam hd hb, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨-, -, -, -, hp₀⟩ := draw_keeps hd
      obtain ⟨hs₀, -⟩ := draw_eqs hd
      exact .qLam (inferSound_of hvar hlet hb (hΓ.bindTy _ _) (hs₀ ▸ hc)
        (quiescent_of_eq hs₀ hp₀ hq) σ hab hσ _ (hr.bindTy _ (.var _)))
  | _, _, _, _, _, .app h₁ h₂ hd hs, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      have k₂ := Infer.keeps h₂
      obtain ⟨k₃, m₃, -, -, -⟩ := draw_keeps hd
      have k₄ := hs.keeps
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
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₂.lift hσ hr (inferSound_of hvar hlet h₂ hΓ c₁ q₁ σ
        (hab.back x₂ c₂) hσ₂ Γ' hr)
      exact .qApp (.qEq ih₁ (hsolve.unifies_sat hσ₃')) ih₂
  | _, _, _, _, _, .rcdDyn h₁ h₂ hd hs, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      have k₂ := Infer.keeps h₂
      obtain ⟨k₃, m₃, -, -, -⟩ := draw_keeps hd
      have k₄ := hs.keeps
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
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₂.lift hσ hr (inferSound_of hvar hlet h₂ hΓ c₁ q₁ σ
        (hab.back x₂ c₂) hσ₂ Γ' hr)
      exact .qRcdDyn (.qEq ih₁ (hsolve.unifies_sat hσ₃')) ih₂
  -- A-rcd-dyn-𝓫: both IHs; the key's type is 𝓫 under ⟦S₂⟧, hence ≈ 𝓫 under σ
  | _, _, _, _, _, .rcdDynBase (S₂ := S₂) (τ₁ := τ₁) h₁ h₂ hbase, hΓ, hc, hq =>
      fun σ hab hσ Γ' hr => by
      have k₂ := Infer.keeps h₂
      have c₁ := Infer.clean h₁ hc
      have x₁ := Infer.ext h₂
      have hσ₂ : Sol.Sat σ S₂.sol := hσ
      have hσ₁ := Infer.sat_mono h₂ σ hσ₂
      have q₁ := Infer.quiescent h₁ hq
      have ih₁ := k₂.lift hσ₂ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := inferSound_of hvar hlet h₂ hΓ c₁ q₁ σ hab hσ₂ Γ' hr
      have he := Ty.applySubst_sat_equiv hσ₂ τ₁
      rw [show τ₁.applySubst S₂.sol.toSubst = .base _ from hbase] at he
      exact .qRcdDynBase (.qEq ih₁ he.symm) ih₂
  | _, _, _, _, _, .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      have k₂ := Infer.keeps h₂
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd₁
      obtain ⟨kb, mb, -, -, -⟩ := draw_keeps hd₂
      have k₃ := hs₁.keeps
      have k₄ := hs₂.keeps
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
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₂.lift hσ hr (inferSound_of hvar hlet h₂ hΓ c₁ q₁ σ
        (hab.back x₂ c₂) hσ₂ Γ' hr)
      exact .qCat (.qEq ih₁ (hsolve₁.unifies_sat hσ₃')) (.qEq ih₂ (hsolve₂.unifies_sat hσ₄'))
  | _, _, _, _, _, .sel h₁ hd hs hlk, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      have k₂ := hs.keeps
      have c₁ := Infer.clean h₁ hc
      have x₁ := (draw_ext hd).trans hs.ext
      have K := ka.trans k₂ hs.satMono
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih := K.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have hrcd := QTypedA.qEq ih (hsolve.unifies_sat hσ₂')
      obtain ⟨r'', hl'', he''⟩ :=
        Sol.lookup_sat hσ hlk (by intro hh; cases hh)
      cases he'' with
      | found hty => exact .qEq (.qSel hrcd hl'') hty.symm
  | _, _, _, _, _, .selAbs h₁ hd hs hlk, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      have k₂ := hs.keeps
      have c₁ := Infer.clean h₁ hc
      have x₁ := (draw_ext hd).trans (hs.ext.trans (.of_sol_eq rfl))
      have K := ka.trans k₂ hs.satMono
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih := K.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have hrcd := QTypedA.qEq ih (hsolve.unifies_sat hσ₂')
      obtain ⟨r'', hl'', he''⟩ :=
        Sol.lookup_sat hσ hlk (by intro hh; cases hh)
      cases he'' with
      | absent => exact .qSelAbs hrcd hl''
  | _, _, _, _, _, .selUnk (S₂ := S₂) (S₂' := S₂d) (l := l) (r := r) (α := α) (δ := δ)
      h₁ hd hs _ hd₂, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      have k₂ := hs.keeps
      obtain ⟨kb, mb, -, -, -⟩ := draw_keeps hd₂
      have c₁ := Infer.clean h₁ hc
      have x₁ := (draw_ext hd).trans (hs.ext.trans ((draw_ext hd₂).trans
        (SolverState.Ext.of_sol_eq (S := S₂d) (S' := S₂d.park ⟨α, ⟨Row.var r, .lit l, .var δ⟩⟩) rfl)))
      have kp : SolverState.KeepsS S₂d (S₂d.park ⟨α, ⟨Row.var r, .lit l, .var δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₂d.park ⟨α, ⟨Row.var r, .lit l, .var δ⟩⟩) (S' := S₂d) rfl
      have K := ka.trans (k₂.trans (kb.trans kp mp) (mb.trans mp))
        (hs.satMono.trans (mb.trans mp))
      obtain ⟨hs₂, -⟩ := draw_eqs hd₂
      have hσ₂ : Sol.Sat σ S₂.sol := hs₂ ▸ hσ
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ₂
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      exact .assume (.qEq (K.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)) (hsolve.unifies_sat hσ₂')) List.mem_cons_self
  | _, _, _, _, _, .lab, _, _, _ => fun _ _ _ _ _ => .qLab
  -- A-sel-dyn: the record's IH and the key's IH, both lifted to the end, and the
  -- lookup carried over the ⟦S⟧/σ gap on the row AND on the key
  | _, _, _, _, _, .selDyn (τ₂ := τ₂) (κ := κ) h₁ hd hs h₂ hdk hsk hlk, hΓ, hc, hq =>
      fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      obtain ⟨kc, mc, -, -, -⟩ := draw_keeps hdk
      have k₂ := hs.keeps
      have k₃ := Infer.keeps h₂
      have c₁ := Infer.clean h₁ hc
      obtain ⟨hsd, hpd⟩ := draw_eqs hd
      have c₂ := hs.clean (hsd ▸ c₁)
      have q₂ := hs.quiescent
      have c₃ := Infer.clean h₂ c₂
      -- S₃ ⇝ S₄: the key's type is forced to ⌊κ⌋
      have K₃₄ := kc.trans hsk.keeps hsk.satMono
      have m₃₄ := mc.trans hsk.satMono
      have x₃₄ := (draw_ext hdk).trans hsk.ext
      have k₃' := k₃.trans K₃₄ m₃₄
      have m₂₄ := (Infer.sat_mono h₂).trans m₃₄
      have K₁ := ka.trans (k₂.trans k₃' m₂₄) (hs.satMono.trans m₂₄)
      have x₁ := (draw_ext hd).trans (hs.ext.trans ((Infer.ext h₂).trans x₃₄))
      have hσ₃ := m₃₄ σ hσ
      have hσ₂ := Infer.sat_mono h₂ σ hσ₃
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ₂
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₃₄.lift hσ hr (inferSound_of hvar hlet h₂ hΓ c₂ q₂ σ
        (hab.back x₃₄ c₃) hσ₃ Γ' hr)
      obtain ⟨S₃'', hsolveK, hsatK⟩ := hsk
      have hkey := hsolveK.unifies_sat (hsatK.satMono σ hσ)
      have ih₂' := QTypedA.qEq ih₂ hkey
      have hrcd := QTypedA.qEq ih₁ (hsolve.unifies_sat hσ₂')
      obtain ⟨r'', hl'', he''⟩ :=
        Sol.lookupQ_sat hσ hlk (by intro hh; cases hh)
      have hl₃ := hl''
      cases he'' with
      | found hty => exact .qEq (.qSelDyn hrcd ih₂' hl₃) hty.symm
  | _, _, _, _, _, .selDynAbs (τ₂ := τ₂) (κ := κ) h₁ hd hs h₂ hdk hsk hlk, hΓ, hc, hq =>
      fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      obtain ⟨kc, mc, -, -, -⟩ := draw_keeps hdk
      have k₂ := hs.keeps
      have k₃ := Infer.keeps h₂
      have c₁ := Infer.clean h₁ hc
      obtain ⟨hsd, hpd⟩ := draw_eqs hd
      have c₂ := hs.clean (hsd ▸ c₁)
      have q₂ := hs.quiescent
      have c₃ := Infer.clean h₂ c₂
      -- S₃ ⇝ S₄: the key's type is forced to ⌊κ⌋
      have K₃₄ := kc.trans hsk.keeps hsk.satMono
      have m₃₄ := mc.trans hsk.satMono
      have x₃₄ := (draw_ext hdk).trans hsk.ext
      have k₃' := k₃.trans K₃₄ m₃₄
      have m₂₄ := (Infer.sat_mono h₂).trans m₃₄
      have K₁ := ka.trans (k₂.trans k₃' m₂₄) (hs.satMono.trans m₂₄)
      have x₁ := (draw_ext hd).trans (hs.ext.trans ((Infer.ext h₂).trans
        (x₃₄.trans (.of_sol_eq rfl))))
      have hσ₃ := m₃₄ σ hσ
      have hσ₂ := Infer.sat_mono h₂ σ hσ₃
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ₂
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₃₄.lift hσ hr (inferSound_of hvar hlet h₂ hΓ c₂ q₂ σ
        (hab.back (x₃₄.trans (.of_sol_eq rfl)) c₃) hσ₃ Γ' hr)
      obtain ⟨S₃'', hsolveK, hsatK⟩ := hsk
      have hkey := hsolveK.unifies_sat (hsatK.satMono σ hσ)
      have ih₂' := QTypedA.qEq ih₂ hkey
      have hrcd := QTypedA.qEq ih₁ (hsolve.unifies_sat hσ₂')
      obtain ⟨r'', hl'', he''⟩ :=
        Sol.lookupQ_sat hσ hlk (by intro hh; cases hh)
      have hl₃ := hl''
      cases he'' with
      | absent => exact .qSelDynAbs hrcd ih₂' hl₃
  -- A-sel-dyn-𝓫: A-sel-dyn's record half; the key's type is 𝓫 under ⟦S₃⟧,
  -- hence ≈ 𝓫 under σ, and T-sel-dyn-𝓫 answers ★
  | _, _, _, _, _, .selDynBase (S₃ := S₃) (τ₂ := τ₂) h₁ hd hs h₂ hbase, hΓ, hc, hq =>
      fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      have k₂ := hs.keeps
      have k₃ := Infer.keeps h₂
      have c₁ := Infer.clean h₁ hc
      obtain ⟨hsd, hpd⟩ := draw_eqs hd
      have c₂ := hs.clean (hsd ▸ c₁)
      have q₂ := hs.quiescent
      have m₂₃ := Infer.sat_mono h₂
      have K₁ := ka.trans (k₂.trans k₃ m₂₃) (hs.satMono.trans m₂₃)
      have x₁ := (draw_ext hd).trans (hs.ext.trans (Infer.ext h₂))
      have hσ₃ : Sol.Sat σ S₃.sol := hσ
      have hσ₂ := m₂₃ σ hσ₃
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ₂
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih₁ := K₁.lift hσ₃ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := inferSound_of hvar hlet h₂ hΓ c₂ q₂ σ hab hσ₃ Γ' hr
      have hrcd := QTypedA.qEq ih₁ (hsolve.unifies_sat hσ₂')
      have he := Ty.applySubst_sat_equiv hσ₃ τ₂
      rw [show τ₂.applySubst S₃.sol.toSubst = .base _ from hbase] at he
      exact .qSelDynBase hrcd (.qEq ih₂ he.symm)
  | _, _, _, _, _, .selDynUnk (S₄ := S₄) (S₄' := S₄d) (τ₂ := τ₂) (r := r) (κ := κ) (α := α)
      (δ := δ) h₁ hd hs h₂ hdk hsk _ hd₂, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      obtain ⟨ka, ma, -, -, -⟩ := draw_keeps hd
      obtain ⟨kc, mc, -, -, -⟩ := draw_keeps hdk
      have k₂ := hs.keeps
      have k₃ := Infer.keeps h₂
      have c₁ := Infer.clean h₁ hc
      obtain ⟨hsd, hpd⟩ := draw_eqs hd
      have c₂ := hs.clean (hsd ▸ c₁)
      have q₂ := hs.quiescent
      have c₃ := Infer.clean h₂ c₂
      -- S₃ ⇝ S₄: the key's type is forced to ⌊κ⌋
      have K₃₄ := kc.trans hsk.keeps hsk.satMono
      have m₃₄ := mc.trans hsk.satMono
      have x₃₄ := (draw_ext hdk).trans hsk.ext
      obtain ⟨kb, mb, -, -, -⟩ := draw_keeps hd₂
      have kp : SolverState.KeepsS S₄d (S₄d.park ⟨α, ⟨Row.var r, .var κ, .var δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₄d.park ⟨α, ⟨Row.var r, .var κ, .var δ⟩⟩) (S' := S₄d) rfl
      have xp := (draw_ext hd₂).trans
        (SolverState.Ext.of_sol_eq (S := S₄d) (S' := S₄d.park ⟨α, ⟨Row.var r, .var κ, .var δ⟩⟩) rfl)
      have K₄p := kb.trans kp mp
      have m₄p := mb.trans mp
      have K₃p := K₃₄.trans K₄p m₄p
      have m₃p := m₃₄.trans m₄p
      have K₂ := k₃.trans K₃p m₃p
      have m₂ := (Infer.sat_mono h₂).trans m₃p
      have K₁ := ka.trans (k₂.trans K₂ m₂) (hs.satMono.trans m₂)
      have x₁ := (draw_ext hd).trans (hs.ext.trans ((Infer.ext h₂).trans (x₃₄.trans xp)))
      obtain ⟨hs₄, -⟩ := draw_eqs hd₂
      have hσ₄ : Sol.Sat σ S₄.sol := hs₄ ▸ hσ
      have hσ₃ := m₃₄ σ hσ₄
      have hσ₂ := Infer.sat_mono h₂ σ hσ₃
      obtain ⟨S₂', hsolve, hsatu⟩ := hs
      have hσ₂' := hsatu.satMono σ hσ₂
      have hσ₁ := ma σ (hsolve.satMono σ hσ₂')
      have ih₁ := K₁.lift hσ hr (inferSound_of hvar hlet h₁ hΓ hc hq σ
        (hab.back x₁ c₁) hσ₁ Γ' hr)
      have ih₂ := K₃p.lift hσ hr (inferSound_of hvar hlet h₂ hΓ c₂ q₂ σ
        (hab.back (x₃₄.trans xp) c₃) hσ₃ Γ' hr)
      obtain ⟨S₃'', hsolveK, hsatK⟩ := hsk
      have hkey := hsolveK.unifies_sat (hsatK.satMono σ hσ₄)
      exact .assumeDyn (.qEq ih₁ (hsolve.unifies_sat hσ₂')) (.qEq ih₂ hkey) List.mem_cons_self
  | _, _, _, _, _, .rcd hb, hΓ, hc, hq => fun σ hab hσ Γ' hr =>
      .qRcd (inferRecSound_of hvar hlet hb hΓ hc hq σ hab hσ Γ' hr)
  | _, _, _, _, _, .letE (S₁ := S₁) (ᾱ := ᾱ) (x := x) (τ₁ := τ₁) h₁ hA h₂,
      hΓ, hc, hq => by
      have hΓ' := letScheme_wf (x := x) (τ₁ := τ₁) hΓ hA.results
      have c₁ := Infer.clean h₁ hc
      have q₁ : ({ S₁ with parked := letG S₁ ᾱ } : SolverState B).Quiescent :=
        quiescent_sub (Infer.quiescent h₁ hq) (fun p hp => (mem_letG.mp hp).1)
      exact hlet h₁ hA h₂ hΓ hc hq
        (inferSound_of hvar hlet h₁ hΓ hc hq) (inferSound_of hvar hlet h₂ hΓ' c₁ q₁)

theorem inferRecSound_of {B C : Type} [DecidableEq B] {constTy : C → B}
    (hvar : VarCase B C constTy) (hlet : LetCase B C constTy) :
    {Γ : QCtx B} → {S S' : SolverState B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    InferRec constTy Γ S ξ ρ S' → Γ.SchemesWF → S.sol.Clean → S.Quiescent →
    SoundAtRec constTy Γ ξ ρ S'
  | _, _, _, _, _, .empty, _, _, _ => fun _ _ _ _ _ => .empty
  | _, _, _, _, _, .field h₁, hΓ, hc, hq => fun σ hab hσ Γ' hr =>
      .field (inferSound_of hvar hlet h₁ hΓ hc hq σ hab hσ Γ' hr)
  | _, _, _, _, _, .cat h₁ h₂, hΓ, hc, hq => fun σ hab hσ Γ' hr => by
      have k₂ := InferRec.keeps h₂
      have c₁ := InferRec.clean h₁ hc
      have hσ₁ := InferRec.sat_mono h₂ σ hσ
      exact .cat (k₂.liftBody hσ hr (inferRecSound_of hvar hlet h₁ hΓ hc hq σ
          (hab.back (InferRec.ext h₂) c₁) hσ₁ Γ' hr))
        (inferRecSound_of hvar hlet h₂ hΓ c₁ (InferRec.quiescent h₁ hq) σ hab hσ Γ' hr)

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
⟦S′⟧ — `Finalizes.holds`).
No transport along χ is left: `QTypedA.weaken` did the cashing-in. -/
theorem runSoundA_of {B C : Type} [DecidableEq B] {constTy : C → B}
    (hs : InferSound B C constTy)
    {e : Expr C} {τ : Ty B} {S₁ : SolverState B}
    (h : Infer constTy QCtx.empty ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁)
    {σ : TySubst B} (hab : Absorbs σ S₁) (hsat : Sol.Sat σ S₁.sol)
    (hfin : ∀ p ∈ S₁.parked, (p.stump.at σ).Holds) :
    QTyped constTy QCtx.empty e (τ.applySubst σ) :=
  (hs _ _ _ _ _ h .nil Sol.clean_nil (SolverState.Quiescent.nil rfl) σ hab hsat _
    (CtxRead.nil σ)).toQTyped (fun a ha => by
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    exact hfin p hp)

end MinimalCalculus
