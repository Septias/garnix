-- A-var's NAMES ARE NOT RESERVED.
--
-- Every other rule invents a name by `draw`, which hands out the supply's next
-- name and advances it. A-var does not: `FreshRenaming` only asks the renaming f
-- to avoid what the state has ALREADY committed to — solved variables, parked
-- result variables, Γ's variables. Nothing stops f from choosing a name the
-- supply has not issued yet, and then a later `draw` issues it again.
--
-- The witness:
--
--     let g = λx. x.l in { a = g, b = λz. z.m }
--
-- `g : ∀ 1 2 3. ⟨2.l ↓ 3⟩ ⇒ {2} → 3`. A-var instantiates it with 3 ↦ `natName 6`,
-- which is fresh by every clause of `FreshRenaming` at that moment, and parks the
-- instantiated stump with result `natName 6`. Then `λz. z.m` draws 4, 5 and 6 —
-- and A-sel-? parks a SECOND stump with result `natName 6`, over a different row
-- and label. The run succeeds.
--
-- Two parked stumps sharing a result variable is exactly the configuration the
-- `Wakes.dischargeEquiv` note calls silent loss: every rule that retires a stump
-- filters the parked list on `stump.res`, so settling either one retires BOTH,
-- and the other's lookup is never performed.
--
-- FIXED: `Infer.var` now draws its renaming from the supply (every new name is
-- `natName k` for k in the block `[S.supply.next, Sup.next)`, and wake-up runs at
-- supply `Sup`). The witness is kept against the rule as it WAS, stated as a
-- closure property (`UnguardedVar`), the way `UnguardedLet` keeps A-let's.

import Infer

namespace MinimalCalculus

private abbrev n (k : Nat) : TyVar := natName k

-- 1 (x's type) is SOLVED in S₁, so it is not generalized
private def nrSc : QScheme Unit :=
  ⟨[n 2, n 3], [⟨.var (n 2), "l", n 3⟩], .fn (.rcd (.var (n 2))) (.var (n 3))⟩

private def nrS1 : SolverState Unit :=
  ⟨⟨[(n 1, .rcd (.var (n 2)))], []⟩, [⟨n 2, ⟨.var (n 2), "l", n 3⟩⟩], [], ⟨4⟩,
   [(n 3, .ty), (n 2, .row), (n 1, .ty)]⟩

private def nrB0 : SolverState Unit := { nrS1 with parked := [] }

private def nrΓg : QCtx Unit := (⟨[], []⟩ : QCtx Unit).bindScheme "g" nrSc

-- A-var's renaming: 2 ↦ 8, 3 ↦ 6. The second one is the reuse.
private def nrF (α : TyVar) : TyVar :=
  if α = n 2 then n 8 else n 6

private def nrθ : TySubst Unit :=
  ⟨fun α => if α ∈ [n 2, n 3] then .var (nrF α) else .var α,
   fun α => if α ∈ [n 2, n 3] then .var (nrF α) else .var α⟩

private def nrP : Parked Unit := ⟨n 8, ⟨.var (n 8), "l", n 6⟩⟩
private def nrSa : SolverState Unit := nrB0.park nrP
private def nrSb : SolverState Unit := (nrSa.draw .ty).2
private def nrSc' : SolverState Unit := (nrSb.draw .row).2
private def nrSol : Sol Unit := ⟨[(n 4, .rcd (.var (n 5)))], []⟩
private def nrSd : SolverState Unit := nrSc'.extend nrSol ⟨6⟩
private def nrSe : SolverState Unit := (nrSd.draw .ty).2
private def nrQ : Parked Unit := ⟨n 5, ⟨.var (n 5), "m", n 6⟩⟩

/-- the final state of the run. -/
def nrFinal : SolverState Unit := nrSe.park nrQ

private def nrId : TySubst Unit := ⟨fun x => .var x, fun x => .var x⟩

private def nrE : Expr Unit :=
  .letE "g" (selEx Unit)
    (.rcd (.cat (.field "a" (.var "g"))
                (.field "b" (.lam "z" (.sel (.var "z") "m")))))

/-- the inferred type: both selections answer at `natName 6`. -/
def nrTy : Ty Unit :=
  .rcd (.cat (.sing "a" (.fn (.rcd (.var (n 8))) (.var (n 6))))
             (.sing "b" (.fn (.var (n 4)) (.var (n 6)))))

private theorem nr_quiescent_a : nrSb.Quiescent := by
  intro p hp
  simp only [nrSb, nrSa, nrB0, SolverState.draw, SolverState.park, List.mem_cons,
    List.not_mem_nil, or_false] at hp
  subst hp; exact .varFree rfl

private theorem nr_quiescent_d : nrSd.Quiescent := by
  intro p hp
  simp only [nrSd, nrSc', nrSb, nrSa, nrB0, SolverState.draw, SolverState.park,
    SolverState.extend, List.mem_cons, List.not_mem_nil, or_false] at hp
  subst hp; exact .varFree rfl

/-- A-var as it stood: the renaming is fresh for the state but not drawn. -/
def UnguardedVar (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ {Γ : QCtx B} {S S' : SolverState B} {x : Var} {σ : QScheme B}
    {θ : TySubst B} {f : TyVar → TyVar} {ps : List (Parked B)},
    Γ.lookup x = some σ →
    IsRenaming θ σ.vars f → FreshRenaming f σ.vars Γ S →
    InstStumps θ f σ.constraints ps →
    WakesSat S ps S' →
    Infer constTy Γ S (.var x) (σ.body.applySubst θ) S'

private theorem nr_var_g (hu : UnguardedVar Unit Unit (fun _ => ())) :
    Infer (B := Unit) (C := Unit) (fun _ => ()) nrΓg nrB0 (.var "g")
      (.fn (.rcd (.var (n 8))) (.var (n 6))) nrSa := by
  have hren : IsRenaming nrθ nrSc.vars nrF := by
    refine ⟨⟨fun α h => ?_, fun α h => ?_⟩, fun α h => ?_⟩
    · have h' : α ∉ [n 2, n 3] := h
      simp only [nrθ, if_neg h']
    · have h' : α ∉ [n 2, n 3] := h
      simp only [nrθ, if_neg h']
    · have h' : α ∈ [n 2, n 3] := h
      simp only [nrθ, if_pos h', and_self]
  have hfr : FreshRenaming nrF nrSc.vars nrΓg nrB0 := by
    refine ⟨?_, ?_, ?_, ?_⟩
    · intro α hα β hβ he
      simp only [nrSc, List.mem_cons, List.not_mem_nil, or_false] at hα hβ
      rcases hα with rfl | rfl <;> rcases hβ with rfl | rfl <;>
        first | rfl | exact absurd he (by decide)
    · intro α hα
      simp only [nrSc, List.mem_cons, List.not_mem_nil, or_false] at hα
      rcases hα with rfl | rfl <;> decide
    · intro _ _ _ h; exact nomatch h
    · intro α hα
      simp only [nrSc, List.mem_cons, List.not_mem_nil, or_false] at hα
      rcases hα with rfl | rfl <;> decide
  have hinst : InstStumps nrθ nrF nrSc.constraints [nrP] := by
    show [nrP.stump] = [⟨(Row.var (n 2)).applySubst nrθ, "l", nrF (n 3)⟩]
    rfl
  have hbody : nrSc.body.applySubst nrθ = .fn (.rcd (.var (n 8))) (.var (n 6)) := by
    rfl
  rw [← hbody]
  exact hu (x := "g") (σ := nrSc) (θ := nrθ) (f := nrF) (ps := [nrP])
    (by rw [nrΓg, QCtx.lookup_bindScheme]; simp) hren hfr hinst
    ⟨nrSa, .park (.varFree rfl) .nil, .done (by
      intro p hp
      simp only [nrSa, nrB0, SolverState.park, List.mem_cons, List.not_mem_nil,
        or_false] at hp
      subst hp; exact .varFree rfl)⟩

private theorem nr_lam :
    Infer (B := Unit) (C := Unit) (fun _ => ()) nrΓg nrSa
      (.lam "z" (.sel (.var "z") "m")) (.fn (.var (n 4)) (.var (n 6)))
      nrFinal := by
  refine Infer.lam (S₀ := nrSb) rfl ?_
  refine Infer.selUnk (τ := .var (n 4)) (S₁ := nrSb) (S₁' := nrSc') (S₂ := nrSd)
    (r := n 5) (α := n 5) (δ := n 6) (S₂' := nrSe) ?_ rfl ?_ ?_ rfl
  · exact Infer.var_mono
      (by rw [QCtx.bindTy, QCtx.lookup_bindScheme]; simp)
      ⟨_, .nil, .done nr_quiescent_a⟩
  · exact ⟨nrSd, ⟨5, nrSol, ⟨6⟩, rfl, rfl⟩, .done nr_quiescent_d⟩
  · exact .varFree rfl

/-- ⊢  **the run goes through** against the unguarded A-var. -/
theorem nameReuse_infers_unguarded (hu : UnguardedVar Unit Unit (fun _ => ())) :
    Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩ ⟨Sol.nil, [], [], ⟨1⟩, []⟩
      nrE nrTy nrFinal := by
  refine Infer.letE (τ₁ := .fn (.var (n 1)) (.var (n 3))) (S₁ := nrS1)
    (Δq := nrS1.parked) (Δγ := []) (ᾱ := [n 2, n 3]) (κs := [.row, .ty])
    selEx_infers rfl (by simp [nrS1]) ?_ (fun _ h => nomatch h) ?_
    (fun _ _ _ h => nomatch h) ?_ (fun _ _ _ h => nomatch h) ?_ ?_ ?_
  · intro p hp
    simp only [nrS1, List.mem_cons, List.not_mem_nil, or_false] at hp
    subst hp; decide
  · intro α _ β hβ
    exact absurd hβ List.not_mem_nil
  · refine ⟨fun p hp => ?_, fun p hp q hq _ => ?_⟩
    · simp only [nrS1, List.mem_cons, List.not_mem_nil, or_false] at hp
      subst hp; exact ⟨rfl, by decide⟩
    · simp only [nrS1, List.mem_cons, List.not_mem_nil, or_false] at hp hq
      subst hp; subst hq; rfl
  · intro α hα
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hα
    rcases hα with rfl | rfl <;> decide
  · intro p hp q hq
    simp only [nrS1, List.mem_cons, List.not_mem_nil, or_false] at hp hq
    subst hp; subst hq; decide
  · have hsc : letScheme nrS1 [n 2, n 3] nrS1.parked (Ty.fn (.var (n 1)) (.var (n 3)))
        = nrSc := by
      rfl
    rw [hsc]
    exact Infer.rcd (.cat (.field (nr_var_g hu)) (.field nr_lam))

/-- ⊢  **…and ends with two parked stumps sharing a result variable**, over
different rows and labels. -/
-- (a property of the witness state alone, so it survives the fix unchanged)
theorem nameReuse_shared_res :
    nrFinal.parked.map (·.stump.res) = [n 6, n 6] ∧
      nrFinal.parked.map (·.stump.label) = ["m", "l"] := by
  decide

/-- ⊢  **settling either one retires both.** K-hit, K-⊥, K-repark and F-★ all
filter on `stump.res`. -/
theorem nameReuse_filter_drops_both :
    nrFinal.parked.filter (·.stump.res != n 6) = [] := by
  decide

end MinimalCalculus
