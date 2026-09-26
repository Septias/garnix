-- ONE STUMP PER RESULT VARIABLE, AND WHAT THAT BUYS.
--
-- Every rule that retires a parked stump filters Δ on `stump.res`. That is only
-- right if a result variable names ONE stump: otherwise retiring one retires its
-- namesakes too, silently (`nameReuse_shared_res`, FreshNames.lean). With A-var
-- now drawing its names from the supply, the invariant is provable:
--
--   `PInv S` — every parked result variable is a name the supply has already
--              issued, and two parked entries with the same result variable
--              carry the same stump.
--
-- The first half is what makes a DRAWN name fresh for Δ; the second is what the
-- filters need. Together they give `KeepsS`: a stump parked at S is, at every
-- later state, still parked — the same stump — or discharged under any σ that
-- satisfies the later state. That is the bookkeeping `inferSound_of` used to
-- take as a hypothesis.
--
-- Contexts need their own condition, because A-var instantiates Γ's schemes:
-- each scheme's result variables are bound (`QScheme.WF`), and its constraints
-- are one per result variable. A-let's premises supply both for the schemes it
-- builds.

import InferSound

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

/-- every parked result variable has been issued by the supply. -/
def SolverState.ResBelow (S : SolverState B) : Prop :=
  ∀ p ∈ S.parked, ∃ k, k < S.supply.next ∧ p.stump.res = natName k

/-- a parked result variable names one stump. -/
def SolverState.ResFun (S : SolverState B) : Prop :=
  ∀ p ∈ S.parked, ∀ q ∈ S.parked, p.stump.res = q.stump.res → p.stump = q.stump

def SolverState.PInv (S : SolverState B) : Prop := S.ResBelow ∧ S.ResFun

/-- the schemes of Γ are well-formed and one constraint per result variable. -/
def QCtx.SchemesWF (Γ : QCtx B) : Prop :=
  ∀ x sc, Γ.lookup x = some sc →
    sc.WF ∧ ∀ a ∈ sc.constraints, ∀ b ∈ sc.constraints, a.res = b.res → a = b

/-- a stump parked at S survives to S′ as the same stump, or discharges. -/
def SolverState.KeepsS (S S' : SolverState B) : Prop :=
  ∀ σ : TySubst B, Sol.Sat σ S'.sol → ∀ p ∈ S.parked,
    (∃ q ∈ S'.parked, q.stump = p.stump) ∨
    p.stump.DischargeEquiv (⟨[], []⟩ : Ctx B) σ

/-- a list of constraints about to be submitted is compatible with S. -/
def PsOk (S : SolverState B) (ps : List (Parked B)) : Prop :=
  (∀ p ∈ ps, ∃ k, k < S.supply.next ∧ p.stump.res = natName k) ∧
  (∀ p ∈ ps, ∀ q ∈ S.parked, q.stump.res = p.stump.res → q.stump = p.stump) ∧
  (∀ p ∈ ps, ∀ p' ∈ ps, p.stump.res = p'.stump.res → p.stump = p'.stump)

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

theorem SolverState.PInv.of_sub {S S' : SolverState B} (h : S.PInv)
    (hs : ∀ p ∈ S'.parked, p ∈ S.parked) (hn : S.supply.next ≤ S'.supply.next) :
    S'.PInv := by
  refine ⟨fun p hp => ?_, fun p hp q hq he => h.2 p (hs p hp) q (hs q hq) he⟩
  obtain ⟨k, hk, he⟩ := h.1 p (hs p hp)
  exact ⟨k, Nat.lt_of_lt_of_le hk hn, he⟩

private theorem natName_lt_ne {j k : Nat} (h : j < k) : natName j ≠ natName k :=
  fun he => by have := natName_inj he; omega

theorem SolveTy.pinv {S S' : SolverState B} {τ τ' : Ty B} (hs : SolveTy S τ τ' S')
    (h : S.PInv) : S'.PInv := by
  have hn := hs.supply
  obtain ⟨_, _, _, _, rfl⟩ := hs
  exact h.of_sub (fun p hp => hp) hn

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

theorem Wake.pinv {S S₁ : SolverState B} {p : Parked B} (hw : Wake S p S₁)
    (h : S.PInv) (hp : PsOk S [p]) : S₁.PInv := by
  have hn := hw.supply
  obtain ⟨hpb, hpf, -⟩ := hp
  refine ⟨fun q hq => ?_, fun q hq q' hq' he => ?_⟩
  · rcases Wake.parked_sub hw q hq with hq | hqs
    · obtain ⟨k, hk, he⟩ := h.1 q hq
      exact ⟨k, Nat.lt_of_lt_of_le hk hn, he⟩
    · obtain ⟨k, hk, he⟩ := hpb p List.mem_cons_self
      exact ⟨k, Nat.lt_of_lt_of_le hk hn, hqs ▸ he⟩
  · -- the filters keep only entries whose res differs from p's
    have hfilt : ∀ r ∈ S₁.parked, r ∈ S.parked → r.stump.res ≠ p.stump.res ∨ r.stump = p.stump := by
      intro r hr _
      cases hw with
      | hit _ hs =>
          obtain ⟨_, _, _, _, rfl⟩ := hs
          have := (List.mem_filter.mp hr).2
          exact .inl (by simpa using this)
      | abs _ hs =>
          obtain ⟨_, _, _, _, rfl⟩ := hs
          have := (List.mem_filter.mp hr).2
          exact .inl (by simpa using this)
      | repark _ =>
          rcases List.mem_cons.mp hr with rfl | hr
          · exact .inr rfl
          · have := (List.mem_filter.mp hr).2
            exact .inl (by simpa using this)
    rcases Wake.parked_sub hw q hq with hq₀ | hqs <;>
      rcases Wake.parked_sub hw q' hq' with hq₀' | hq's
    · exact h.2 q hq₀ q' hq₀' he
    · -- q old, q' the reparked p
      rcases hfilt q hq hq₀ with hne | hs
      · exact absurd (he.trans (congrArg Stump.res hq's)) hne
      · exact hs.trans hq's.symm
    · rcases hfilt q' hq' hq₀' with hne | hs
      · exact absurd ((congrArg Stump.res hqs).symm.trans he).symm hne
      · exact hqs.trans hs.symm
    · exact hqs.trans hq's.symm

theorem Wake.keepsS {S S₁ : SolverState B} {p : Parked B} (hw : Wake S p S₁)
    (hp : PsOk S [p]) : S.KeepsS S₁ := by
  intro σ hσ q hq
  by_cases hr : q.stump.res = p.stump.res
  · have hqs : q.stump = p.stump := hp.2.1 p List.mem_cons_self q hq hr
    rcases Wake.dischargeEquiv (Γ' := (⟨[], []⟩ : QCtx B)) rfl hw hσ with hd | ⟨q', hq', hq's⟩
    · exact .inr (hqs ▸ hd)
    · exact .inl ⟨q', hq', hq's.trans hqs.symm⟩
  · exact .inl ⟨q, Wake.parked_preserved hr hw hq, rfl⟩

-- ⊢  after one step, the rest of the submitted list is still compatible
private theorem Wake.psOk_tail {S S₁ : SolverState B} {p : Parked B}
    {ps : List (Parked B)} (hw : Wake S p S₁) (hp : PsOk S (p :: ps)) : PsOk S₁ ps := by
  have hn := hw.supply
  obtain ⟨h1, h2, h3⟩ := hp
  refine ⟨fun p' hp' => ?_, fun p' hp' q hq he => ?_, fun a ha b hb he =>
    h3 a (List.mem_cons_of_mem _ ha) b (List.mem_cons_of_mem _ hb) he⟩
  · obtain ⟨k, hk, he⟩ := h1 p' (List.mem_cons_of_mem _ hp')
    exact ⟨k, Nat.lt_of_lt_of_le hk hn, he⟩
  · rcases Wake.parked_sub hw q hq with hq | hqs
    · exact h2 p' (List.mem_cons_of_mem _ hp') q hq he
    · have hpp := h3 p List.mem_cons_self p' (List.mem_cons_of_mem _ hp')
        ((congrArg Stump.res hqs).symm.trans he)
      exact hqs.trans hpp

private theorem psOk_park_tail {S : SolverState B} {p : Parked B} {ps : List (Parked B)}
    (hp : PsOk S (p :: ps)) : PsOk (S.park p) ps := by
  obtain ⟨h1, h2, h3⟩ := hp
  refine ⟨fun p' hp' => h1 p' (List.mem_cons_of_mem _ hp'), fun p' hp' q hq he => ?_,
    fun a ha b hb he => h3 a (List.mem_cons_of_mem _ ha) b (List.mem_cons_of_mem _ hb) he⟩
  rcases List.mem_cons.mp hq with rfl | hq
  · exact h3 q List.mem_cons_self p' (List.mem_cons_of_mem _ hp') he
  · exact h2 p' (List.mem_cons_of_mem _ hp') q hq he

private theorem pinv_park {S : SolverState B} {p : Parked B} {ps : List (Parked B)}
    (h : S.PInv) (hp : PsOk S (p :: ps)) : (S.park p).PInv := by
  obtain ⟨h1, h2, -⟩ := hp
  refine ⟨fun q hq => ?_, fun q hq q' hq' he => ?_⟩
  · rcases List.mem_cons.mp hq with rfl | hq
    · exact h1 q List.mem_cons_self
    · exact h.1 q hq
  · rcases List.mem_cons.mp hq with e1 | hq₁ <;> rcases List.mem_cons.mp hq' with e1' | hq₁'
    · rw [e1, e1']
    · rw [e1] at he ⊢
      exact (h2 p List.mem_cons_self q' hq₁' he.symm).symm
    · rw [e1'] at he ⊢
      exact h2 p List.mem_cons_self q hq₁ he
    · exact h.2 q hq₁ q' hq₁' he

private theorem psOk_head {S : SolverState B} {p : Parked B} {ps : List (Parked B)}
    (hp : PsOk S (p :: ps)) : PsOk S [p] :=
  ⟨fun q hq => by
      rw [List.mem_singleton] at hq; rw [hq]; exact hp.1 p List.mem_cons_self,
   fun q hq r hr he => by
      rw [List.mem_singleton] at hq; rw [hq] at he ⊢
      exact hp.2.1 p List.mem_cons_self r hr he,
   fun a ha b hb _ => by
      rw [List.mem_singleton] at ha hb; rw [ha, hb]⟩

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

private theorem psOk_of_parked {S : SolverState B} {p : Parked B} (h : S.PInv)
    (hp : p ∈ S.parked) : PsOk S [p] :=
  ⟨fun q hq => by rw [List.mem_singleton] at hq; rw [hq]; exact h.1 p hp,
   fun q hq r hr he => by
      rw [List.mem_singleton] at hq; rw [hq] at he ⊢; exact h.2 r hr p hp he,
   fun a ha b hb _ => by rw [List.mem_singleton] at ha hb; rw [ha, hb]⟩

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

theorem QCtx.SchemesWF.nil : (⟨[], []⟩ : QCtx B).SchemesWF :=
  fun _ _ h => nomatch h

--------------------- A-var: THE SUBMITTED LIST IS COMPATIBLE -----------------

theorem psOk_of_var {S : SolverState B} {sc : QScheme B} {θ : TySubst B}
    {f : TyVar → TyVar} {ps : List (Parked B)} {Sup : Supply} {K : KEnv} (h : S.PInv)
    (hwf : sc.WF) (hfun : ∀ a ∈ sc.constraints, ∀ b ∈ sc.constraints, a.res = b.res → a = b)
    (hinj : ∀ α ∈ sc.vars, ∀ β ∈ sc.vars, f α = f β → α = β)
    (hdr : ∀ α ∈ sc.vars, ∃ k, S.supply.next ≤ k ∧ k < Sup.next ∧ f α = natName k)
    (hps : InstStumps θ f sc.constraints ps) :
    PsOk { S with supply := Sup, kinds := K } ps := by
  -- every submitted stump is the image of one constraint
  have himg : ∀ p ∈ ps, ∃ st ∈ sc.constraints,
      p.stump = ⟨st.row.applySubst θ, st.label, f st.res⟩ := by
    intro p hp
    have hm : p.stump ∈ ps.map Parked.stump := List.mem_map_of_mem hp
    rw [hps] at hm
    obtain ⟨st, hst, he⟩ := List.mem_map.mp hm
    exact ⟨st, hst, he.symm⟩
  refine ⟨fun p hp => ?_, fun p hp q hq he => ?_, fun p hp p' hp' he => ?_⟩
  · obtain ⟨st, hst, he⟩ := himg p hp
    obtain ⟨k, -, hk, hfk⟩ := hdr st.res (hwf st hst)
    exact ⟨k, hk, by rw [he]; exact hfk⟩
  · obtain ⟨st, hst, hpe⟩ := himg p hp
    obtain ⟨k, hk₁, -, hfk⟩ := hdr st.res (hwf st hst)
    obtain ⟨j, hj, hqj⟩ := h.1 q hq
    have : natName j = natName k := by
      rw [← hqj, ← hfk, he, hpe]
    exact absurd this (natName_lt_ne (Nat.lt_of_lt_of_le hj hk₁))
  · obtain ⟨st, hst, hpe⟩ := himg p hp
    obtain ⟨st', hst', hpe'⟩ := himg p' hp'
    have hres : f st.res = f st'.res := by
      have := he; rw [hpe, hpe'] at this; exact this
    have := hfun st hst st' hst' (hinj _ (hwf st hst) _ (hwf st' hst') hres)
    rw [hpe, hpe', this]

--------------------- WHAT HAPPENS TO A SUBMITTED CONSTRAINT -----------------

/-- ⊢  every constraint submitted to wake-up ends up parked (the same stump) or
discharged, under any σ satisfying the end state. Unlike
`Wakes.dischargeEquiv` this needs no distinctness of the submitted list — only
that it is compatible with the state (`PsOk`). -/
theorem Wakes.fate {S S' : SolverState B} {ps : List (Parked B)} :
    Wakes S ps S' → S.PInv → PsOk S ps → ∀ σ : TySubst B, Sol.Sat σ S'.sol →
    ∀ p ∈ ps, (∃ q ∈ S'.parked, q.stump = p.stump) ∨
      p.stump.DischargeEquiv (⟨[], []⟩ : Ctx B) σ
  | .nil, _, _, _, _, p, hp => absurd hp List.not_mem_nil
  | .cons hw hws, h, hp, σ, hσ, p', hp' => by
      have hp1 := psOk_head hp
      have h₁ := hw.pinv h hp1
      have ht := hw.psOk_tail hp
      rcases List.mem_cons.mp hp' with rfl | hp'
      · obtain ⟨-, hk⟩ := hws.pinv_keeps h₁ ht
        rcases Wake.dischargeEquiv (Γ' := (⟨[], []⟩ : QCtx B)) rfl hw (hws.satMono σ hσ)
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
  | _, _, _, _, _, .var hl _ hfr hdr hle _ hps ⟨S₁, hws, hsat⟩, h, hΓ => by
      obtain ⟨hwf, hfun⟩ := hΓ _ _ hl
      obtain ⟨h₁, hk₁⟩ := hws.pinv_keeps (supply_up_pinv h hle)
        (psOk_of_var h hwf hfun hfr.1 hdr hps)
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
      -- the new stump's result is the name just drawn: issued, and fresh for Δ
      have hnew : PsOk S₂' [⟨α, ⟨Row.var r, l, δ⟩⟩] :=
        ⟨fun q hq => by
            rw [List.mem_singleton] at hq; rw [hq]
            exact ⟨_, by rw [hsup]; exact Nat.lt_succ_self _, hδ⟩,
         fun q hq r hr he => by
            rw [List.mem_singleton] at hq; rw [hq] at he
            rw [hpk] at hr
            obtain ⟨j, hj, hrj⟩ := i₂.1 r hr
            exact absurd (hrj.symm.trans (he.trans hδ)) (natName_lt_ne hj),
         fun a ha b hb _ => by rw [List.mem_singleton] at ha hb; rw [ha, hb]⟩
      refine ⟨pinv_park (ps := []) ib hnew, ?_⟩
      have kp : SolverState.KeepsS S₂' (S₂'.park ⟨α, ⟨Row.var r, l, δ⟩⟩) :=
        SolverState.KeepsS.of_sub (fun q hq => List.mem_cons_of_mem _ hq)
      have mp := SolverState.SatMono.of_sol_eq
        (S := S₂'.park ⟨α, ⟨Row.var r, l, δ⟩⟩) (S' := S₂') rfl
      exact k₁.trans (ka.trans (k₂.trans (kb.trans kp mp) (mb.trans mp))
        (hs.satMono.trans (mb.trans mp))) (ma.trans (hs.satMono.trans (mb.trans mp)))
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
          exact (hres.1 p hp).2)
        (fun a ha b hb he => by
          obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
          obtain ⟨q, hq, rfl⟩ := List.mem_map.mp hb
          rw [hres.2 p hp q hq he])
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
  ⟨fun _ h => (nomatch h), fun _ h => (nomatch h)⟩

end MinimalCalculus
