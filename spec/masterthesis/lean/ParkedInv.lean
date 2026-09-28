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
-- state. `PInv`/`PsOk` are kept as names and are now vacuous; they can be
-- dropped from the signatures that thread them.

import InferSound

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

/-- vacuous since the filters key on the stump (header). -/
def SolverState.PInv (_S : SolverState B) : Prop := True

/-- the schemes of Γ are well-formed and one constraint per result variable. -/
def QCtx.SchemesWF (Γ : QCtx B) : Prop :=
  ∀ x sc, Γ.lookup x = some sc →
    sc.WF ∧ ∀ a ∈ sc.constraints, ∀ b ∈ sc.constraints, a.res = b.res → a = b

/-- a stump parked at S survives to S′ as the same stump, or discharges. -/
def SolverState.KeepsS (S S' : SolverState B) : Prop :=
  ∀ σ : TySubst B, Sol.Sat σ S'.sol → ∀ p ∈ S.parked,
    (∃ q ∈ S'.parked, q.stump = p.stump) ∨
    p.stump.DischargeEquiv σ

/-- vacuous since the filters key on the stump (header). -/
def PsOk (_S : SolverState B) (_ps : List (Parked B)) : Prop := True

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

theorem SolverState.PInv.of_sub {S S' : SolverState B} (_h : S.PInv)
    (_hs : ∀ p ∈ S'.parked, p ∈ S.parked) (_hn : S.supply.next ≤ S'.supply.next) :
    S'.PInv := trivial

theorem SolveTy.pinv {S S' : SolverState B} {τ τ' : Ty B} (_hs : SolveTy S τ τ' S')
    (_h : S.PInv) : S'.PInv := trivial

theorem SolveTy.keepsS {S S' : SolverState B} {τ τ' : Ty B} (hs : SolveTy S τ τ' S') :
    S.KeepsS S' := by
  obtain ⟨_, _, _, _, rfl⟩ := hs
  exact .of_sub (fun p hp => hp)

theorem draw_pinv_keeps {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) (hi : S.PInv) :
    S₀.PInv ∧ S.KeepsS S₀ ∧ S.SatMono S₀ ∧ α = natName S.supply.next ∧
      S₀.supply.next = S.supply.next + 1 ∧ S₀.parked = S.parked := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  have h1 : α = (S.draw κ).1 := congrArg Prod.fst h
  subst h2; subst h1
  refine ⟨hi.of_sub (fun p hp => hp) (Nat.le_succ _), .of_sub (fun p hp => hp),
    .of_sol_eq rfl, rfl, rfl, rfl⟩

--------------------- WAKE-UP ------------------------------------------------

-- what a wake-up step can leave in Δ: old entries, or the woken stump reparked
private theorem Wake.parked_sub {S S₁ : SolverState B} {p : Parked B} :
    Wake S p S₁ → ∀ q ∈ S₁.parked, q ∈ S.parked ∨ q.stump = p.stump
  | .hit _ hs, q, hq => by
      obtain ⟨_, _, _, _, rfl⟩ := hs
      exact .inl (List.mem_filter.mp hq).1
  | .abs _ hs, q, hq => by
      obtain ⟨_, _, _, _, rfl⟩ := hs
      exact .inl (List.mem_filter.mp hq).1
  | .repark _, q, hq => by
      rcases List.mem_cons.mp hq with rfl | hq
      · exact .inr rfl
      · exact .inl (List.mem_filter.mp hq).1

theorem Wake.pinv {S S₁ : SolverState B} {p : Parked B} (_hw : Wake S p S₁)
    (_h : S.PInv) (_hp : PsOk S [p]) : S₁.PInv := trivial

theorem Wake.keepsS {S S₁ : SolverState B} {p : Parked B} (hw : Wake S p S₁)
    (_hp : PsOk S [p]) : S.KeepsS S₁ := by
  intro σ hσ q hq
  by_cases hqs : q.stump = p.stump
  · rcases Wake.dischargeEquiv hw hσ with hd | ⟨q', hq', hq's⟩
    · exact .inr (hqs ▸ hd)
    · exact .inl ⟨q', hq', hq's.trans hqs.symm⟩
  · exact .inl ⟨q, Wake.parked_preserved hqs hw hq, rfl⟩

-- ⊢  after one step, the rest of the submitted list is still compatible
private theorem Wake.psOk_tail {S S₁ : SolverState B} {p : Parked B}
    {ps : List (Parked B)} (_hw : Wake S p S₁) (_hp : PsOk S (p :: ps)) : PsOk S₁ ps :=
  trivial

private theorem psOk_park_tail {S : SolverState B} {p : Parked B} {ps : List (Parked B)}
    (_hp : PsOk S (p :: ps)) : PsOk (S.park p) ps := trivial

private theorem pinv_park {S : SolverState B} {p : Parked B} {ps : List (Parked B)}
    (_h : S.PInv) (_hp : PsOk S (p :: ps)) : (S.park p).PInv := trivial

private theorem psOk_head {S : SolverState B} {p : Parked B} {ps : List (Parked B)}
    (_hp : PsOk S (p :: ps)) : PsOk S [p] := trivial

theorem Wakes.pinv_keeps {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → S.PInv → PsOk S ps → S'.PInv ∧ S.KeepsS S'
  | .nil, h, _ => ⟨h, .refl _⟩
  | .cons hw hws, h, hp => by
      have hp1 := psOk_head hp
      obtain ⟨h', hk'⟩ := Wakes.pinv_keeps hws (hw.pinv h hp1) (hw.psOk_tail hp)
      exact ⟨h', (hw.keepsS hp1).trans hk' hws.satMono⟩
  | .park _ hws, h, hp => by
      obtain ⟨h', hk'⟩ := Wakes.pinv_keeps hws (pinv_park h hp) (psOk_park_tail hp)
      exact ⟨h', (SolverState.KeepsS.of_sub (S' := SolverState.park _ _)
        (fun q hq => List.mem_cons_of_mem _ hq)).trans hk' hws.satMono⟩

private theorem psOk_of_parked {S : SolverState B} {p : Parked B} (_h : S.PInv)
    (_hp : p ∈ S.parked) : PsOk S [p] := trivial

theorem Saturate.pinv_keeps {S S' : SolverState B} :
    Saturate S S' → S.PInv → S'.PInv ∧ S.KeepsS S'
  | .done _, h => ⟨h, .refl _⟩
  | .step hp _ hw hsat, h => by
      have hp1 := psOk_of_parked h hp
      obtain ⟨h', hk'⟩ := Saturate.pinv_keeps hsat (hw.pinv h hp1)
      exact ⟨h', (hw.keepsS hp1).trans hk' hsat.satMono⟩

theorem SolveTySat.pinv_keeps {S S' : SolverState B} {τ τ' : Ty B} :
    SolveTySat S τ τ' S' → S.PInv → S'.PInv ∧ S.KeepsS S'
  | ⟨_, hs, hsat⟩, h => by
      obtain ⟨h', hk'⟩ := hsat.pinv_keeps (hs.pinv h)
      exact ⟨h', hs.keepsS.trans hk' hsat.satMono⟩

--------------------- CONTEXTS -------------------------------------------------

theorem QCtx.SchemesWF.bindScheme {Γ : QCtx B} (h : Γ.SchemesWF) (x : Var)
    {sc : QScheme B} (hwf : sc.WF)
    (hf : ∀ a ∈ sc.constraints, ∀ b ∈ sc.constraints, a.res = b.res → a = b) :
    (Γ.bindScheme x sc).SchemesWF := by
  intro y sc' hy
  rw [QCtx.lookup_bindScheme] at hy
  by_cases hxy : (x == y) = true
  · rw [if_pos hxy] at hy; injection hy with hy; subst hy; exact ⟨hwf, hf⟩
  · rw [if_neg hxy] at hy; exact h y sc' hy

theorem QCtx.SchemesWF.bindTy {Γ : QCtx B} (h : Γ.SchemesWF) (x : Var) (τ : Ty B) :
    (Γ.bindTy x τ).SchemesWF :=
  h.bindScheme x (fun _ h => nomatch h) (fun _ h => nomatch h)

theorem QCtx.SchemesWF.nil : (QCtx.empty : QCtx B).SchemesWF :=
  fun _ _ h => nomatch h

--------------------- A-var: THE SUBMITTED LIST IS COMPATIBLE -----------------

/-- ⊢  a well-formed constraint's result, instantiated: the renamed binder. -/
theorem inst_res {θ : TySubst B} {vs : List TyVar} {f : TyVar → TyVar} {st : Stump B}
    (hren : IsRenaming θ vs f) (h : ∃ δ ∈ vs, st.res = .var δ) :
    ∃ δ ∈ vs, st.res = .var δ ∧ st.res.applySubst θ = .var (f δ) := by
  obtain ⟨δ, hδ, hre⟩ := h
  exact ⟨δ, hδ, hre, by rw [hre]; exact (hren.2 δ hδ).1⟩

--------------------- WHAT HAPPENS TO A SUBMITTED CONSTRAINT -----------------

/-- ⊢  every constraint submitted to wake-up ends up parked (the same stump) or
discharged, under any σ satisfying the end state. Unlike
`Wakes.dischargeEquiv` this needs no distinctness of the submitted list — only
that it is compatible with the state (`PsOk`). -/
theorem Wakes.fate {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → S.PInv → PsOk S ps → ∀ σ : TySubst B, Sol.Sat σ S'.sol →
    ∀ p ∈ ps, (∃ q ∈ S'.parked, q.stump = p.stump) ∨
      p.stump.DischargeEquiv σ
  | .nil, _, _, _, _, p, hp => absurd hp List.not_mem_nil
  | .cons hw hws, h, hp, σ, hσ, p', hp' => by
      have hp1 := psOk_head hp
      have h₁ := hw.pinv h hp1
      have ht := hw.psOk_tail hp
      rcases List.mem_cons.mp hp' with rfl | hp'
      · obtain ⟨-, hk⟩ := hws.pinv_keeps h₁ ht
        rcases Wake.dischargeEquiv hw (hws.satMono σ hσ)
          with hd | ⟨q, hq, hqs⟩
        · exact .inr hd
        · rcases hk σ hσ q hq with ⟨q', hq', hq's⟩ | hd
          · exact .inl ⟨q', hq', hq's.trans hqs⟩
          · exact .inr (hqs ▸ hd)
      · exact Wakes.fate hws h₁ ht σ hσ p' hp'
  | .park (p := p) _ hws, h, hp, σ, hσ, p', hp' => by
      have h₁ := pinv_park h hp
      have ht := psOk_park_tail hp
      rcases List.mem_cons.mp hp' with rfl | hp'
      · obtain ⟨-, hk⟩ := hws.pinv_keeps h₁ ht
        exact hk σ hσ p' List.mem_cons_self
      · exact Wakes.fate hws h₁ ht σ hσ p' hp'

--------------------- OVER A DERIVATION ---------------------------------------

theorem supply_up_pinv {S : SolverState B} {Sup : Supply} {K : KEnv} (h : S.PInv)
    (hle : S.supply.next ≤ Sup.next) :
    ({ S with supply := Sup, kinds := K } : SolverState B).PInv :=
  h.of_sub (fun p hp => hp) hle

mutual

/-- ⊢  **the invariant holds of every reachable state, and every parked stump is
kept**: at the end of a derivation it is still parked, the same stump, or it
discharges under any σ that satisfies the final state. -/
theorem Infer.pinv_keeps {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {e : Expr C} → {τ : Ty B} →
    Infer constTy Γ S e τ S' → S.PInv → Γ.SchemesWF → S'.PInv ∧ S.KeepsS S'
  | _, _, _, _, _, .con, h, _ => ⟨h, .refl _⟩
  | _, _, _, _, _, .var hl hren hfr hdr hle _ hps ⟨S₁, hws, hsat⟩, h, hΓ => by
      obtain ⟨hwf, hfun⟩ := hΓ _ _ hl
      obtain ⟨h₁, hk₁⟩ := hws.pinv_keeps (supply_up_pinv h hle) trivial
      obtain ⟨h₂, hk₂⟩ := hsat.pinv_keeps h₁
      exact ⟨h₂, ((SolverState.KeepsS.of_sub (fun p hp => hp)).trans hk₁
        (hws.satMono)).trans hk₂ hsat.satMono⟩
  | _, _, _, _, _, .lam hd hb, h, hΓ => by
      obtain ⟨h₀, hk₀, -, -, -, -⟩ := draw_pinv_keeps hd h
      obtain ⟨h', hk'⟩ := Infer.pinv_keeps hb h₀ (hΓ.bindTy _ _)
      exact ⟨h', hk₀.trans hk' (Infer.sat_mono hb)⟩
  | _, _, _, _, _, .app h₁ h₂ hd hs, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨i₂, k₂⟩ := Infer.pinv_keeps h₂ i₁ hΓ
      obtain ⟨i₃, k₃, m₃, -, -, -⟩ := draw_pinv_keeps hd i₂
      obtain ⟨i₄, k₄⟩ := hs.pinv_keeps i₃
      exact ⟨i₄, k₁.trans (k₂.trans (k₃.trans k₄ hs.satMono) (m₃.trans hs.satMono))
        ((Infer.sat_mono h₂).trans (m₃.trans hs.satMono))⟩
  | _, _, _, _, _, .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨i₂, k₂⟩ := Infer.pinv_keeps h₂ i₁ hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd₁ i₂
      obtain ⟨ib, kb, mb, -, -, -⟩ := draw_pinv_keeps hd₂ ia
      obtain ⟨i₃, k₃⟩ := hs₁.pinv_keeps ib
      obtain ⟨i₄, k₄⟩ := hs₂.pinv_keeps i₃
      have m34 := hs₁.satMono.trans hs₂.satMono
      exact ⟨i₄, k₁.trans (k₂.trans (ka.trans (kb.trans (k₃.trans k₄ hs₂.satMono) m34)
        (mb.trans m34)) (ma.trans (mb.trans m34)))
        ((Infer.sat_mono h₂).trans (ma.trans (mb.trans m34)))⟩
  | _, _, _, _, _, .sel h₁ hd hs _, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      exact ⟨i₂, k₁.trans (ka.trans k₂ hs.satMono) (ma.trans hs.satMono)⟩
  | _, _, _, _, _, .selAbs h₁ hd hs _, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      exact ⟨i₂, k₁.trans (ka.trans k₂ hs.satMono) (ma.trans hs.satMono)⟩
  | _, _, _, _, _, .selUnk (S₂ := S₂) (S₂' := S₂') (l := l) (r := r) (α := α) (δ := δ)
      h₁ hd hs _ hd₂, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      obtain ⟨ib, kb, mb, hδ, hsup, hpk⟩ := draw_pinv_keeps hd₂ i₂
      have hnew : PsOk S₂' [⟨α, ⟨Row.var r, .lab l, .var δ⟩⟩] := trivial
      refine ⟨pinv_park (ps := []) ib hnew, ?_⟩
      have kp : SolverState.KeepsS S₂' (S₂'.park ⟨α, ⟨Row.var r, .lab l, .var δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₂'.park ⟨α, ⟨Row.var r, .lab l, .var δ⟩⟩) (S' := S₂') rfl
      exact k₁.trans (ka.trans (k₂.trans (kb.trans kp mp) (mb.trans mp))
        (hs.satMono.trans (mb.trans mp))) (ma.trans (hs.satMono.trans (mb.trans mp)))
  | _, _, _, _, _, .lab, h, _ => ⟨h, .refl _⟩
  | _, _, _, _, _, .selDyn h₁ hd hs h₂ _, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      obtain ⟨i₃, k₃⟩ := Infer.pinv_keeps h₂ i₂ hΓ
      have mt := hs.satMono.trans (Infer.sat_mono h₂)
      exact ⟨i₃, k₁.trans (ka.trans (k₂.trans k₃ (Infer.sat_mono h₂)) mt) (ma.trans mt)⟩
  | _, _, _, _, _, .selDynAbs h₁ hd hs h₂ _, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      obtain ⟨i₃, k₃⟩ := Infer.pinv_keeps h₂ i₂ hΓ
      have mt := hs.satMono.trans (Infer.sat_mono h₂)
      have r := And.intro i₃ (k₁.trans (ka.trans (k₂.trans k₃ (Infer.sat_mono h₂)) mt)
        (ma.trans mt))
      exact r
  | _, _, _, _, _, .selDynUnk (S₃ := S₃) (S₃' := S₃') (τ₂ := τ₂) (r := r) (α := α) (δ := δ)
      h₁ hd hs h₂ _ hd₂, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      obtain ⟨ia, ka, ma, -, -, -⟩ := draw_pinv_keeps hd i₁
      obtain ⟨i₂, k₂⟩ := hs.pinv_keeps ia
      obtain ⟨i₃, k₃⟩ := Infer.pinv_keeps h₂ i₂ hΓ
      obtain ⟨ib, kb, mb, hδ, hsup, hpk⟩ := draw_pinv_keeps hd₂ i₃
      have hnew : PsOk S₃' [⟨α, ⟨Row.var r, τ₂, .var δ⟩⟩] := trivial
      refine ⟨pinv_park (ps := []) ib hnew, ?_⟩
      have kp : SolverState.KeepsS S₃' (S₃'.park ⟨α, ⟨Row.var r, τ₂, .var δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₃'.park ⟨α, ⟨Row.var r, τ₂, .var δ⟩⟩) (S' := S₃') rfl
      have m₃ := (Infer.sat_mono h₂).trans (mb.trans mp)
      exact k₁.trans (ka.trans (k₂.trans (k₃.trans (kb.trans kp mp) (mb.trans mp)) m₃)
        (hs.satMono.trans m₃)) (ma.trans (hs.satMono.trans m₃))
  | _, _, _, _, _, .rcd hb, h, hΓ => InferRec.pinv_keeps hb h hΓ
  | _, _, _, _, _, .letE (S₁ := S₁) (Δq := Δq) (Δγ := Δγ) (x := x) (τ₁ := τ₁) (ᾱ := ᾱ) h₁ _ hsplit _ _ _ hown hres _ _ _ h₂,
      h, hΓ => by
      obtain ⟨i₁, k₁⟩ := Infer.pinv_keeps h₁ h hΓ
      have hγ : ∀ p ∈ Δγ, p ∈ S₁.parked := fun p hp => by
        exact hsplit.mem_iff.mpr <| List.mem_append_right _ hp
      have i₁' : SolverState.PInv { S₁ with parked := Δγ } := i₁.of_sub hγ (Nat.le_refl _)
      have hΓ' := hΓ.bindScheme x (sc := letScheme S₁ ᾱ Δq τ₁)
        (fun st hst => by
          obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst
          exact ⟨_, (hres.1 p hp).2, rfl⟩)
        (fun a ha b hb he => by
          obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
          obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hb
          rw [hres.2 p hp q hq (Ty.var.inj he)])
      obtain ⟨i₂, k₂⟩ := Infer.pinv_keeps h₂ i₁' hΓ'
      refine ⟨i₂, fun σ hσ p hp => ?_⟩
      rcases k₁ σ (Infer.sat_mono h₂ σ hσ) p hp with ⟨q, hq, hqs⟩ | hd
      · replace hq := hsplit.mem_iff.mp hq
        rcases List.mem_append.mp hq with hq | hq
        · exact absurd hqs (hown q hq p hp)
        · rcases k₂ σ hσ q hq with ⟨q', hq', hq's⟩ | hd'
          · exact .inl ⟨q', hq', hq's.trans hqs⟩
          · exact .inr (hqs ▸ hd')
      · exact .inr hd

theorem InferRec.pinv_keeps {C : Type} {constTy : C → B} :
    {Γ : QCtx B} → {S S' : SolverState B} → {ξ : RecBody (Expr C)} → {ρ : Row B} →
    InferRec constTy Γ S ξ ρ S' → S.PInv → Γ.SchemesWF → S'.PInv ∧ S.KeepsS S'
  | _, _, _, _, _, .empty, h, _ => ⟨h, .refl _⟩
  | _, _, _, _, _, .field h₁, h, hΓ => Infer.pinv_keeps h₁ h hΓ
  | _, _, _, _, _, .cat h₁ h₂, h, hΓ => by
      obtain ⟨i₁, k₁⟩ := InferRec.pinv_keeps h₁ h hΓ
      obtain ⟨i₂, k₂⟩ := InferRec.pinv_keeps h₂ i₁ hΓ
      exact ⟨i₂, k₁.trans k₂ (InferRec.sat_mono h₂)⟩

end

theorem SolverState.PInv.init : (⟨Sol.nil, [], [], ⟨1⟩, []⟩ : SolverState B).PInv :=
  trivial

end MinimalCalculus
