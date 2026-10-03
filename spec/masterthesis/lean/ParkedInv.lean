-- EVERY PARKED STUMP IS KEPT.
--
-- Every rule that retires a parked stump filters Δ on the STUMP. Equal stumps
-- discharge alike, so retiring one retires only what the discharge covers. The
-- filters used to key on `stump.res`, which needed an invariant — each result
-- variable names one stump (`nameReuse_shared_res`, FreshNames.lean) — and broke
-- down once a result could be a TYPE: two instances of a spent promise can share
-- `𝓫 → 𝓫`.
--
-- What remains is `KeepsS`: a stump parked at S is, at every later state, still
-- parked — the same stump — or discharged under any σ that satisfies the later
-- state.

import InferSound

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

/-- the schemes of Γ are well-formed. -/
def QCtx.SchemesWF (Γ : QCtx B) : Prop :=
  ∀ x sc, Γ.lookup x = some sc → sc.WF

/-- a stump parked at S survives to S′ as the same stump, or discharges. -/
def SolverState.KeepsS (S S' : SolverState B) : Prop :=
  ∀ σ : TySubst B, Sol.Sat σ S'.sol → ∀ p ∈ S.parked,
    (∃ q ∈ S'.parked, q.stump = p.stump) ∨
    p.stump.Discharge σ

--------------------- ELEMENTARY STEPS -----------------------------------------

theorem SolverState.KeepsS.refl (S : SolverState B) : S.KeepsS S :=
  fun _ _ p hp => .inl ⟨p, hp, rfl⟩

theorem SolverState.KeepsS.of_sub {S S' : SolverState B}
    (h : ∀ p ∈ S.parked, p ∈ S'.parked) : S.KeepsS S' :=
  fun _ _ p hp => .inl ⟨p, h p hp, rfl⟩

theorem SolverState.KeepsS.trans {S₁ S₂ S₃ : SolverState B}
    (h₁ : S₁.KeepsS S₂) (h₂ : S₂.KeepsS S₃) (hm : S₂.SatMono S₃) : S₁.KeepsS S₃ := by
  intro σ hσ p hp
  rcases h₁ σ (hm σ hσ) p hp with ⟨q, hq, hqs⟩ | hd
  · rcases h₂ σ hσ q hq with ⟨q', hq', hq's⟩ | hd'
    · exact .inl ⟨q', hq', hq's.trans hqs⟩
    · exact .inr (hqs ▸ hd')
  · exact .inr hd

theorem SolveTy.keepsS {S S' : SolverState B} {τ τ' : Ty B} (hs : SolveTy S τ τ' S') :
    S.KeepsS S' := by
  obtain ⟨_, _, _, _, rfl⟩ := hs
  exact .of_sub (fun p hp => hp)

theorem draw_keeps {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) :
    S.KeepsS S₀ ∧ S.SatMono S₀ ∧ α = natName S.supply.next ∧
      S₀.supply.next = S.supply.next + 1 ∧ S₀.parked = S.parked := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  have h1 : α = (S.draw κ).1 := congrArg Prod.fst h
  subst h2; subst h1
  exact ⟨.of_sub (fun p hp => hp), .of_sol_eq rfl, rfl, rfl, rfl⟩

--------------------- WAKE-UP ------------------------------------------------

theorem Wake.keepsS {S S₁ : SolverState B} {p : Parked B} (hw : Wake S p S₁) :
    S.KeepsS S₁ := by
  intro σ hσ q hq
  by_cases hqs : q.stump = p.stump
  · rcases Wake.dischargeEquiv hw hσ with hd | ⟨q', hq', hq's⟩
    · exact .inr (hqs ▸ hd)
    · exact .inl ⟨q', hq', hq's.trans hqs.symm⟩
  · exact .inl ⟨q, Wake.parked_preserved hqs hw hq, rfl⟩

theorem Wakes.keeps {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → S.KeepsS S'
  | .nil => .refl _
  | .cons hw hws => hw.keepsS.trans (Wakes.keeps hws) hws.satMono
  | .park _ hws => (SolverState.KeepsS.of_sub (S' := SolverState.park _ _)
      (fun q hq => List.mem_cons_of_mem _ hq)).trans (Wakes.keeps hws) hws.satMono

theorem Saturate.keeps {S S' : SolverState B} : Saturate S S' → S.KeepsS S'
  | .done _ => .refl _
  | .step _ _ hw hsat => hw.keepsS.trans (Saturate.keeps hsat) hsat.satMono

theorem SolveTySat.keeps {S S' : SolverState B} {τ τ' : Ty B} :
    SolveTySat S τ τ' S' → S.KeepsS S'
  | ⟨_, hs, hsat⟩ => hs.keepsS.trans hsat.keeps hsat.satMono

--------------------- CONTEXTS -------------------------------------------------

theorem QCtx.SchemesWF.bindScheme {Γ : QCtx B} (h : Γ.SchemesWF) (x : Var)
    {sc : QScheme B} (hwf : sc.WF) :
    (Γ.bindScheme x sc).SchemesWF := by
  intro y sc' hy
  rw [QCtx.lookup_bindScheme] at hy
  by_cases hxy : (x == y) = true
  · rw [if_pos hxy] at hy; injection hy with hy; subst hy; exact hwf
  · rw [if_neg hxy] at hy; exact h y sc' hy

theorem QCtx.SchemesWF.bindTy {Γ : QCtx B} (h : Γ.SchemesWF) (x : Var) (τ : Ty B) :
    (Γ.bindTy x τ).SchemesWF :=
  h.bindScheme x (fun _ h => nomatch h)

theorem QCtx.SchemesWF.nil : (QCtx.empty : QCtx B).SchemesWF :=
  fun _ _ h => nomatch h

--------------------- WHAT HAPPENS TO A SUBMITTED CONSTRAINT -----------------

/-- ⊢  every constraint submitted to wake-up ends up parked (the same stump) or
discharged, under any σ satisfying the end state. Unlike `Wakes.dischargeEquiv`
this needs no distinctness of the submitted list. -/
theorem Wakes.fate {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → ∀ σ : TySubst B, Sol.Sat σ S'.sol →
    ∀ p ∈ ps, (∃ q ∈ S'.parked, q.stump = p.stump) ∨
      p.stump.Discharge σ
  | .nil, _, _, p, hp => absurd hp List.not_mem_nil
  | .cons hw hws, σ, hσ, p', hp' => by
      rcases List.mem_cons.mp hp' with rfl | hp'
      · rcases Wake.dischargeEquiv hw (hws.satMono σ hσ) with hd | ⟨q, hq, hqs⟩
        · exact .inr hd
        · rcases hws.keeps σ hσ q hq with ⟨q', hq', hq's⟩ | hd
          · exact .inl ⟨q', hq', hq's.trans hqs⟩
          · exact .inr (hqs ▸ hd)
      · exact Wakes.fate hws σ hσ p' hp'
  | .park (p := p) _ hws, σ, hσ, p', hp' => by
      rcases List.mem_cons.mp hp' with rfl | hp'
      · exact hws.keeps σ hσ p' List.mem_cons_self
      · exact Wakes.fate hws σ hσ p' hp'

--------------------- OVER A DERIVATION ---------------------------------------

mutual

/-- ⊢  **every parked stump is kept**: at the end of a derivation it is still
parked, the same stump, or it discharges under any σ that satisfies the final
state. -/
theorem Infer.keeps {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {e : Expr C} → {τ : Ty B} →
    Infer constTy Γ S e τ S' → S.KeepsS S'
  | _, _, _, _, _, .con => .refl _
  | _, _, _, _, _, .var _ _ _ _ _ _ _ ⟨_, hws, hsat⟩ =>
      ((SolverState.KeepsS.of_sub (fun p hp => hp)).trans hws.keeps hws.satMono).trans
        hsat.keeps hsat.satMono
  | _, _, _, _, _, .lam hd hb => (draw_keeps hd).1.trans (Infer.keeps hb) (Infer.sat_mono hb)
  | _, _, _, _, _, .app h₁ h₂ hd hs => by
      obtain ⟨k₃, m₃, -⟩ := draw_keeps hd
      exact (Infer.keeps h₁).trans ((Infer.keeps h₂).trans (k₃.trans hs.keeps hs.satMono)
        (m₃.trans hs.satMono)) ((Infer.sat_mono h₂).trans (m₃.trans hs.satMono))
  | _, _, _, _, _, .rcdDyn h₁ h₂ hd hs => by
      obtain ⟨k₃, m₃, -⟩ := draw_keeps hd
      exact (Infer.keeps h₁).trans ((Infer.keeps h₂).trans (k₃.trans hs.keeps hs.satMono)
        (m₃.trans hs.satMono)) ((Infer.sat_mono h₂).trans (m₃.trans hs.satMono))
  | _, _, _, _, _, .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd₁
      obtain ⟨kb, mb, -⟩ := draw_keeps hd₂
      have m34 := hs₁.satMono.trans hs₂.satMono
      exact (Infer.keeps h₁).trans ((Infer.keeps h₂).trans (ka.trans (kb.trans
        (hs₁.keeps.trans hs₂.keeps hs₂.satMono) m34) (mb.trans m34)) (ma.trans (mb.trans m34)))
        ((Infer.sat_mono h₂).trans (ma.trans (mb.trans m34)))
  | _, _, _, _, _, .sel h₁ hd hs _ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd
      exact (Infer.keeps h₁).trans (ka.trans hs.keeps hs.satMono) (ma.trans hs.satMono)
  | _, _, _, _, _, .selAbs h₁ hd hs _ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd
      exact (Infer.keeps h₁).trans (ka.trans hs.keeps hs.satMono) (ma.trans hs.satMono)
  | _, _, _, _, _, .selUnk (S₂' := S₂') (l := l) (r := r) (α := α) (δ := δ) h₁ hd hs _ hd₂ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd
      obtain ⟨kb, mb, -⟩ := draw_keeps hd₂
      have kp : SolverState.KeepsS S₂' (S₂'.park ⟨α, ⟨Row.var r, .lit l, .var δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₂'.park ⟨α, ⟨Row.var r, .lit l, .var δ⟩⟩) (S' := S₂') rfl
      exact (Infer.keeps h₁).trans (ka.trans (hs.keeps.trans (kb.trans kp mp) (mb.trans mp))
        (hs.satMono.trans (mb.trans mp))) (ma.trans (hs.satMono.trans (mb.trans mp)))
  | _, _, _, _, _, .lab => .refl _
  | _, _, _, _, _, .selDyn h₁ hd hs h₂ hdk hsk _ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd
      obtain ⟨kc, mc, -⟩ := draw_keeps hdk
      have mk := mc.trans hsk.satMono
      have m₂ := (Infer.sat_mono h₂).trans mk
      exact (Infer.keeps h₁).trans (ka.trans (hs.keeps.trans ((Infer.keeps h₂).trans
        (kc.trans hsk.keeps hsk.satMono) mk) m₂) (hs.satMono.trans m₂))
        (ma.trans (hs.satMono.trans m₂))
  | _, _, _, _, _, .selDynAbs h₁ hd hs h₂ hdk hsk _ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd
      obtain ⟨kc, mc, -⟩ := draw_keeps hdk
      have mk := mc.trans hsk.satMono
      have m₂ := (Infer.sat_mono h₂).trans mk
      have r := (Infer.keeps h₁).trans (ka.trans (hs.keeps.trans ((Infer.keeps h₂).trans
        (kc.trans hsk.keeps hsk.satMono) mk) m₂) (hs.satMono.trans m₂))
        (ma.trans (hs.satMono.trans m₂))
      exact r
  | _, _, _, _, _, .selDynUnk (S₄' := S₄') (r := r) (κ := κ) (α := α) (δ := δ)
      h₁ hd hs h₂ hdk hsk _ hd₂ => by
      obtain ⟨ka, ma, -⟩ := draw_keeps hd
      obtain ⟨kc, mc, -⟩ := draw_keeps hdk
      obtain ⟨kb, mb, -⟩ := draw_keeps hd₂
      have kp : SolverState.KeepsS S₄' (S₄'.park ⟨α, ⟨Row.var r, .var κ, .var δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₄'.park ⟨α, ⟨Row.var r, .var κ, .var δ⟩⟩) (S' := S₄') rfl
      have mt := hsk.satMono.trans (mb.trans mp)
      have mk := mc.trans mt
      have m₂ := (Infer.sat_mono h₂).trans mk
      exact (Infer.keeps h₁).trans (ka.trans (hs.keeps.trans ((Infer.keeps h₂).trans
        (kc.trans (hsk.keeps.trans (kb.trans kp mp) (mb.trans mp)) mt) mk) m₂)
        (hs.satMono.trans m₂)) (ma.trans (hs.satMono.trans m₂))
  | _, _, _, _, _, .rcd hb => InferRec.keeps hb
  | _, _, _, _, _, .letE (S₁ := S₁) (ᾱ := ᾱ) h₁ hA h₂ => by
      have k₁ := Infer.keeps h₁
      have k₂ := Infer.keeps h₂
      intro σ hσ p hp
      rcases k₁ σ (Infer.sat_mono h₂ σ hσ) p hp with ⟨q, hq, hqs⟩ | hd
      · replace hq := (letSplit S₁ ᾱ).mem_iff.mp hq
        rcases List.mem_append.mp hq with hq | hq
        · exact absurd hqs (hA.own q hq p hp)
        · rcases k₂ σ hσ q hq with ⟨q', hq', hq's⟩ | hd'
          · exact .inl ⟨q', hq', hq's.trans hqs⟩
          · exact .inr (hqs ▸ hd')
      · exact .inr hd

theorem InferRec.keeps {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    InferRec constTy Γ S ξ ρ S' → S.KeepsS S'
  | _, _, _, _, _, .empty => .refl _
  | _, _, _, _, _, .field h₁ => Infer.keeps h₁
  | _, _, _, _, _, .cat h₁ h₂ => (InferRec.keeps h₁).trans (InferRec.keeps h₂) (InferRec.sat_mono h₂)

end

end MinimalCalculus
