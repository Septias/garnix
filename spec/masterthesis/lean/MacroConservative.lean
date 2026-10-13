-- MACRONIX IS CONSERVATIVE OVER MININIX.
--
-- A MiniNix term (no builtin, boolean, `if`, `assert`, list or `?`) means the
-- same under every signature with the same constants: its L2 typings, its
-- reductions and its inference run do not read `boolB`, `prim` or `delta`.
-- So the L2 theorems, stated for an arbitrary `Sig`, say about MiniNix
-- exactly what they said before MacroNix, at `Sig.plain`.

import InferFnTerm

namespace MinimalCalculus

variable {B C : Type}

mutual
/-- the MiniNix fragment: none of the MacroNix forms, anywhere -/
def Expr.IsMini : Expr C → Prop
  | .con _ | .var _ | .lab _ => True
  | .lam _ e | .sel e _ => e.IsMini
  | .app e₁ e₂ | .cat e₁ e₂ | .letE _ e₁ e₂ | .selDyn e₁ e₂ | .rcdDyn e₁ e₂ =>
      e₁.IsMini ∧ e₂.IsMini
  | .rcd b => b.IsMini
  | .prim _ _ | .bool _ | .ite _ _ _ | .asrt _ _ | .nil | .cons _ _ | .has _ _ => False

def RecBody.IsMini : RecBody (Expr C) → Prop
  | .empty => True
  | .field _ e => e.IsMini
  | .cat b₁ b₂ => b₁.IsMini ∧ b₂.IsMini
end

--------------------- TYPING -------------------------------------------------

mutual
/-- ⊢  **a MiniNix typing reads only the constants of the signature.** -/
theorem QTyped.sig_mini {sig sig' : Sig B C} (hcon : sig'.con = sig.con) :
    {Γ : QCtx B} → {e : Expr C} → {τ : Ty B} →
    QTyped sig Γ e τ → e.IsMini → QTyped sig' Γ e τ
  | Γ, _, _, .qCon (c := c), _ => by
      have h : QTyped sig' Γ (.con c) (.base (sig'.con c)) := .qCon
      rw [hcon] at h; exact h
  | _, _, _, .qVar hl hi, _ => .qVar hl hi
  | _, _, _, .qEq h he, hm => .qEq (QTyped.sig_mini hcon h hm) he
  | _, _, _, .qLam h, hm => .qLam (QTyped.sig_mini hcon h hm)
  | _, _, _, .qApp h₁ h₂, hm =>
      .qApp (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2)
  | _, _, _, .qLet hwf h₁ hne h₂, hm =>
      .qLet hwf (fun τ' hi => QTyped.sig_mini hcon (h₁ τ' hi) hm.1) hne
        (QTyped.sig_mini hcon h₂ hm.2)
  | _, _, _, .qCat h₁ h₂, hm =>
      .qCat (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2)
  | _, _, _, .qSel h hl, hm => .qSel (QTyped.sig_mini hcon h hm) hl
  | _, _, _, .qSelUnk h hl, hm => .qSelUnk (QTyped.sig_mini hcon h hm) hl
  | _, _, _, .qSelAbs h hl, hm => .qSelAbs (QTyped.sig_mini hcon h hm) hl
  | _, _, _, .qUnk h, hm => .qUnk (QTyped.sig_mini hcon h hm)
  | _, _, _, .qRcd h, hm => .qRcd (QTypedBody.sig_mini hcon h hm)
  | _, _, _, .qLab, _ => .qLab
  | _, _, _, .qSelDyn h₁ h₂ hl, hm =>
      .qSelDyn (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2) hl
  | _, _, _, .qSelDynUnk h₁ h₂ hl, hm =>
      .qSelDynUnk (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2) hl
  | _, _, _, .qSelDynAbs h₁ h₂ hl, hm =>
      .qSelDynAbs (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2) hl
  | _, _, _, .qRcdDyn h₁ h₂, hm =>
      .qRcdDyn (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2)
  | _, _, _, .qSelDynBase h₁ h₂, hm =>
      .qSelDynBase (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2)
  | _, _, _, .qRcdDynBase h₁ h₂, hm =>
      .qRcdDynBase (QTyped.sig_mini hcon h₁ hm.1) (QTyped.sig_mini hcon h₂ hm.2)
  | _, _, _, .qPrim _ _ _, hm => hm.elim
  | _, _, _, .qBool _, hm => hm.elim
  | _, _, _, .qIte _ _ _ _, hm => hm.elim
  | _, _, _, .qAsrt _ _ _, hm => hm.elim
  | _, _, _, .qNil, hm => hm.elim
  | _, _, _, .qCons _ _, hm => hm.elim
  | _, _, _, .qHas _ _, hm => hm.elim

theorem QTypedBody.sig_mini {sig sig' : Sig B C} (hcon : sig'.con = sig.con) :
    {Γ : QCtx B} → {b : RecBody (Expr C)} → {ρ : Row B} →
    QTypedBody sig Γ b ρ → b.IsMini → QTypedBody sig' Γ b ρ
  | _, _, _, .empty, _ => .empty
  | _, _, _, .field h, hm => .field (QTyped.sig_mini hcon h hm)
  | _, _, _, .cat h₁ h₂, hm =>
      .cat (QTypedBody.sig_mini hcon h₁ hm.1) (QTypedBody.sig_mini hcon h₂ hm.2)
end

/-- ⊢  **conservativity of typing**: on MiniNix, a signature types exactly what
the plain signature of its constants types. -/
theorem qtyped_mini_iff {sig : Sig B C} {Γ : QCtx B} {e : Expr C} {τ : Ty B}
    (hm : e.IsMini) : QTyped sig Γ e τ ↔ QTyped (Sig.plain sig.con) Γ e τ :=
  ⟨fun h => QTyped.sig_mini (sig := sig) (sig' := Sig.plain sig.con) rfl h hm,
   fun h => QTyped.sig_mini (sig := Sig.plain sig.con) (sig' := sig) rfl h hm⟩

--------------------- INFERENCE ----------------------------------------------

mutual
/-- ⊢  **the inference function reads only the constants on MiniNix.** -/
theorem inferF_sig_mini [DecidableEq B] {sig sig' : Sig B C} (hcon : sig'.con = sig.con)
    (n : Nat) : (e : Expr C) → e.IsMini →
    ∀ (Γ : QCtx B) (S : SolverState B), inferF sig' n Γ S e = inferF sig n Γ S e
  | .con _, _, _, _ => by simp only [inferF, hcon]
  | .var _, _, _, _ => by simp only [inferF]
  | .lab _, _, _, _ => by simp only [inferF]
  | .lam _ e, hm, _, _ => by simp only [inferF, inferF_sig_mini hcon n e hm]
  | .sel e _, hm, _, _ => by simp only [inferF, inferF_sig_mini hcon n e hm]
  | .app e₁ e₂, hm, _, _ => by
      simp only [inferF, inferF_sig_mini hcon n e₁ hm.1, inferF_sig_mini hcon n e₂ hm.2]
  | .cat e₁ e₂, hm, _, _ => by
      simp only [inferF, inferF_sig_mini hcon n e₁ hm.1, inferF_sig_mini hcon n e₂ hm.2]
  | .letE _ e₁ e₂, hm, _, _ => by
      simp only [inferF, inferF_sig_mini hcon n e₁ hm.1, inferF_sig_mini hcon n e₂ hm.2]
  | .selDyn e₁ e₂, hm, _, _ => by
      simp only [inferF, inferF_sig_mini hcon n e₁ hm.1, inferF_sig_mini hcon n e₂ hm.2]
  | .rcdDyn e₁ e₂, hm, _, _ => by
      simp only [inferF, inferF_sig_mini hcon n e₁ hm.1, inferF_sig_mini hcon n e₂ hm.2]
  | .rcd b, hm, _, _ => by simp only [inferF, inferRecF_sig_mini hcon n b hm]
  | .prim _ _, hm, _, _ => hm.elim
  | .bool _, hm, _, _ => hm.elim
  | .ite _ _ _, hm, _, _ => hm.elim
  | .asrt _ _, hm, _, _ => hm.elim
  | .nil, hm, _, _ => hm.elim
  | .cons _ _, hm, _, _ => hm.elim
  | .has _ _, hm, _, _ => hm.elim

theorem inferRecF_sig_mini [DecidableEq B] {sig sig' : Sig B C} (hcon : sig'.con = sig.con)
    (n : Nat) : (b : RecBody (Expr C)) → b.IsMini →
    ∀ (Γ : QCtx B) (S : SolverState B), inferRecF sig' n Γ S b = inferRecF sig n Γ S b
  | .empty, _, _, _ => by simp only [inferRecF]
  | .field _ e, hm, _, _ => by simp only [inferRecF, inferF_sig_mini hcon n e hm]
  | .cat b₁ b₂, hm, _, _ => by
      simp only [inferRecF, inferRecF_sig_mini hcon n b₁ hm.1, inferRecF_sig_mini hcon n b₂ hm.2]
end

/-- ⊢  **conservativity of runs**: on MiniNix, a run under any signature is the
run under the plain signature of its constants. -/
theorem runF_mini [DecidableEq B] {sig : Sig B C} (n : Nat) {e : Expr C} (hm : e.IsMini) :
    runF sig n e = runF (Sig.plain sig.con) n e := by
  simp only [runF, inferF_sig_mini (sig := Sig.plain sig.con) (sig' := sig) rfl n e hm]

--------------------- REDUCTION ----------------------------------------------

mutual
theorem Expr.IsMini.subst {x : Var} {v : Expr C} (hv : v.IsMini) :
    (e : Expr C) → e.IsMini → (MinimalCalculus.subst x v e).IsMini
  | .con _, _ => trivial
  | .var y, _ => by
      simp only [MinimalCalculus.subst]; split
      · exact hv
      · trivial
  | .lab _, _ => trivial
  | .lam y e, hm => by
      simp only [MinimalCalculus.subst]; split
      · exact hm
      · exact Expr.IsMini.subst hv e hm
  | .sel e _, hm => Expr.IsMini.subst hv e hm
  | .app e₁ e₂, hm => ⟨Expr.IsMini.subst hv e₁ hm.1, Expr.IsMini.subst hv e₂ hm.2⟩
  | .cat e₁ e₂, hm => ⟨Expr.IsMini.subst hv e₁ hm.1, Expr.IsMini.subst hv e₂ hm.2⟩
  | .letE y e₁ e₂, hm => by
      simp only [MinimalCalculus.subst]
      refine ⟨Expr.IsMini.subst hv e₁ hm.1, ?_⟩
      split
      · exact hm.2
      · exact Expr.IsMini.subst hv e₂ hm.2
  | .selDyn e₁ e₂, hm => ⟨Expr.IsMini.subst hv e₁ hm.1, Expr.IsMini.subst hv e₂ hm.2⟩
  | .rcdDyn e₁ e₂, hm => ⟨Expr.IsMini.subst hv e₁ hm.1, Expr.IsMini.subst hv e₂ hm.2⟩
  | .rcd b, hm => RecBody.IsMini.subst hv b hm
  | .prim _ _, hm => hm.elim
  | .bool _, hm => hm.elim
  | .ite _ _ _, hm => hm.elim
  | .asrt _ _, hm => hm.elim
  | .nil, hm => hm.elim
  | .cons _ _, hm => hm.elim
  | .has _ _, hm => hm.elim

theorem RecBody.IsMini.subst {x : Var} {v : Expr C} (hv : v.IsMini) :
    (b : RecBody (Expr C)) → b.IsMini → (substBody x v b).IsMini
  | .empty, _ => trivial
  | .field _ e, hm => Expr.IsMini.subst hv e hm
  | .cat b₁ b₂, hm => ⟨RecBody.IsMini.subst hv b₁ hm.1, RecBody.IsMini.subst hv b₂ hm.2⟩
end

theorem RecBody.IsMini.lookup {l : Label} :
    {b : RecBody (Expr C)} → b.IsMini → ∀ {e : Expr C}, RecBody.lookup l b = some e → e.IsMini
  | .empty, _, _, h => by simp [RecBody.lookup] at h
  | .field l' e', hm, _, h => by
      simp only [RecBody.lookup] at h
      split at h
      · cases h; exact hm
      · cases h
  | .cat b₁ b₂, hm, _, h => by
      simp only [RecBody.lookup] at h
      split at h
      · rename_i e' he; cases h; exact RecBody.IsMini.lookup hm.1 he
      · exact RecBody.IsMini.lookup hm.2 h

/-- ⊢  **conservativity of reduction**: a MiniNix term steps the same under every
δ, to a MiniNix term. -/
theorem Step.mini {δ δ' : Delta C} :
    {e e' : Expr C} → Step δ e e' → e.IsMini → Step δ' e e' ∧ e'.IsMini
  | _, _, .beta hv, hm => ⟨.beta hv, Expr.IsMini.subst hm.2 _ hm.1⟩
  | _, _, .appFun s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.1; ⟨.appFun s', h', hm.2⟩
  | _, _, .appArg v s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.2; ⟨.appArg v s', hm.1, h'⟩
  | _, _, .selStep s, hm => let ⟨s', h'⟩ := Step.mini s hm; ⟨.selStep s', h'⟩
  | _, _, .selVal hl, hm => ⟨.selVal hl, RecBody.IsMini.lookup hm hl⟩
  | _, _, .catLeft s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.1; ⟨.catLeft s', h', hm.2⟩
  | _, _, .catRight v s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.2; ⟨.catRight v s', hm.1, h'⟩
  | _, _, .catVal, hm => ⟨.catVal, hm.2, hm.1⟩
  | _, _, .letCong s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.1; ⟨.letCong s', h', hm.2⟩
  | _, _, .letBeta hv, hm => ⟨.letBeta hv, Expr.IsMini.subst hm.1 _ hm.2⟩
  | _, _, .selDynL s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.1; ⟨.selDynL s', h', hm.2⟩
  | _, _, .selDynR v s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.2; ⟨.selDynR v s', hm.1, h'⟩
  | _, _, .selDynVal hl, hm => ⟨.selDynVal hl, RecBody.IsMini.lookup hm.1 hl⟩
  | _, _, .rcdDynKey s, hm =>
      let ⟨s', h'⟩ := Step.mini s hm.1; ⟨.rcdDynKey s', h', hm.2⟩
  | _, _, .rcdDynVal, hm => ⟨.rcdDynVal, hm.2⟩
  | _, _, .iteC _, hm => hm.elim
  | _, _, .iteT, hm => hm.elim
  | _, _, .iteF, hm => hm.elim
  | _, _, .asrtC _, hm => hm.elim
  | _, _, .asrtT, hm => hm.elim
  | _, _, .hasStep _, hm => hm.elim
  | _, _, .hasVal, hm => hm.elim
  | _, _, .primArg _ _, hm => hm.elim
  | _, _, .primDelta _ _, hm => hm.elim

end MinimalCalculus
