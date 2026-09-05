-- P4: the mutual driver unifyTyF / unifySpineMF, its entry points, and fuel monotonicity.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Reflection

namespace MinimalCalculus

--------------------- P4: THE MUTUAL ≐ / ≐ᵣ DRIVER ---------------------------
-- proof-plan.md §1.2 / §4-P4. The row pass no longer PARKS the type equations
-- it discovers: it solves them, on the spot, by calling the type pass, and
-- applies the solution to the residual before recursing. That is the whole of
-- P4, and it is what makes the stuck verdict mean something (§3): an equation
-- is discharged, or fatal, or itself stuck — never merely deferred.
--
-- Three deviations from §1.2, all recorded in §4-P4:
--  * a success carries its SUPPLY, because a type equation solved inside a
--    field may expand a row variable, and the invented tail travels into the
--    residual; without threading, the residual call would re-draw that name;
--  * `outOfFuel` is a separate verdict, so the fuel lemma is a structural
--    induction ("more fuel never changes a verdict that was reached") rather
--    than the termination measure §1.3 asked for — see the note on unifyBound;
--  * `[DecidableEq B]`: ≐ must decide 𝓫 = 𝓫′. The row pass never needed it.
--
-- This is now THE algorithm: the single-pass URes driver and its four legs were
-- deleted once P5/P6 had ported them (proof-plan.md §4-P5/P6).


theorem tyIsVar_eq {B : Type} : {τ : Ty B} → {α : TyVar} → tyIsVar τ = some α → τ = .var α
  | .var _, _, h => by simp only [tyIsVar, Option.some.injEq] at h; rw [h]
  | .base _, _, h => by simp [tyIsVar] at h
  | .unk, _, h => by simp [tyIsVar] at h
  | .fn _ _, _, h => by simp [tyIsVar] at h
  | .rcd _, _, h => by simp [tyIsVar] at h


-- ## Two worked verdicts, kernel-checked
-- The computational halves of the two incompleteness discussions above; they
-- live here rather than in Regressions.lean because the prose that reads them
-- is here.

-- The occurs guard is CONSERVATIVE: it rejects α ≐ᵣ (β | α | γ), which
-- occurs_allVar_unifiable (:935) shows is unifiable and occurs_allVar_hasMgu
-- (:956) shows has an MGU. Deliberate, and the price §1.3 accepts.
-- ⊢  unifyRowM α (β | α | γ)  =  occurs
theorem occurs_allVar_reported {B : Type} [DecidableEq B] :
    unifyRowM (B := B) 20 (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c")))
      = .occurs := rfl

-- U-EXPAND'S PAYOFF, computed. The verdict crossfield used to get was `.stuck`,
-- and it was WRONG (the prose at :1006 derives the mgu by hand). The driver now
-- finds exactly that mgu, with the equation 𝓫 ≐ δ SOLVED rather than parked.
-- ⊢  unifyRowM (l:𝓫 | α) (m:𝓫 | β)
--      =  success [δ ≔ 𝓫] [β ≔ (l:δ | β′), α ≔ (m:𝓫 | β′ | ε)]
theorem crossfield_success {B : Type} [DecidableEq B] (b : B) :
    unifyRowM (B := B) 20 (.cat (.sing "l" (.base b)) (.var "a"))
                          (.cat (.sing "m" (.base b)) (.var "b")) =
      .success ⟨[(natName 2, .base b)],
                [("b", .cat (.sing "l" (.var (natName 2))) (.var (natName 3))),
                 ("a", .cat (.sing "m" (.base b)) (.cat (.var (natName 3)) .empty))]⟩
               ⟨4⟩ := rfl

-- ## Fuel monotonicity
-- §1.3 asked for a closed-form BOUND realizing a lexicographic measure. That
-- is not what P4 delivers, and the reason is structural: solve-and-apply grows
-- the spine (a variable expands to a whole row), while the number of variables
-- can grow too, since a type equation solved inside a field may itself expand a
-- row variable and hand the invented tail to the residual. Neither component of
-- the §1.3 measure decreases, so no such bound is available yet — the missing
-- ingredient is a Rémy-style argument on the finitely many labels of the
-- problem. See §4-P4.
--
-- `outOfFuel` makes that a separable question. The lemma below says a verdict
-- that was REACHED never changes when the budget grows, which is all any leg
-- except the stuck one needs; only P6 has to know that a sufficient budget
-- exists at all.


-- ## Fuel monotonicity  (`UResM.Mono`, Defs.lean)
theorem UResM.Mono.rfl' {B : Type} (r : UResM B) : UResM.Mono r r := .inr rfl

-- ⊢  Mono is compatible with sequencing, ARM-WISE: no side condition survives
theorem UResM.Mono.seq {B : Type} {r r' : UResM B}
    {k k' : TySubst B → Supply → UResM B}
    (hr : UResM.Mono r r') (hk : ∀ θ S, UResM.Mono (k θ S) (k' θ S)) :
    UResM.Mono (r.seq k) (r'.seq k') := by
  rcases hr with h | h
  · subst h; exact .inl rfl
  · subst h
    cases r' with
    | success s S =>
        rcases hk s.toSubst S with hh | hh
        · exact .inl (by simp only [UResM.seq, hh])
        · exact .inr (by simp only [UResM.seq, hh])
    | clash     => exact .inr rfl
    | occurs    => exact .inr rfl
    | stuck     => exact .inr rfl
    | outOfFuel => exact .inl rfl

-- ⊢  … and with the U-expand wrapper
theorem UResM.Mono.expandRes {B : Type} (S : Supply) (β : TyVar) (l : Label) (τ : Ty B)
    {r r' : UResM B} (h : UResM.Mono r r') :
    UResM.Mono (expandResM S β l τ r) (expandResM S β l τ r') := by
  rcases h with h | h
  · subst h; exact .inl rfl
  · subst h; exact .inr rfl

-- THE FUEL LEMMA, for both sorts at once (the mutual induction is on the
-- budget, exactly as unifySpineF_fuel_irrel's is — but with no measure
-- hypothesis, because `outOfFuel` absorbs the shortfall).
-- The fuel-0 base, for both sorts: at zero budget everything except the arms
-- that need no recursion at all (a variable binding, ★, a base clash, an
-- exhausted side) is `outOfFuel`, and those arms do not look at the budget.
private theorem unifyM_fuel_mono_zero {B : Type} [DecidableEq B] (fuel' : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B),
        UResM.Mono (unifyTyF S 0 τ τ') (unifyTyF S fuel' τ τ')) ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)),
        UResM.Mono (unifySpineMF S 0 s₁ s₂) (unifySpineMF S fuel' s₁ s₂)) := by
  cases fuel' with
  | zero => exact ⟨fun _ _ _ => .inr rfl, fun _ _ _ => .inr rfl⟩
  | succ g =>
      refine ⟨fun S τ τ' => ?_, fun S s₁ s₂ => ?_⟩
      · cases τ <;> cases τ' <;> first | exact .inr rfl | exact .inl rfl
      · cases s₁ with
        | nil => exact .inr rfl
        | cons a s₁ =>
          cases s₂ with
          | nil => exact .inr rfl
          | cons b s₂ => exact .inl rfl

theorem unifyM_fuel_mono {B : Type} [DecidableEq B] (N : Nat) :
    ∀ fuel, fuel ≤ N → ∀ fuel', fuel ≤ fuel' →
      (∀ (S : Supply) (τ τ' : Ty B),
          UResM.Mono (unifyTyF S fuel τ τ') (unifyTyF S fuel' τ τ')) ∧
      (∀ (S : Supply) (s₁ s₂ : List (Atom B)),
          UResM.Mono (unifySpineMF S fuel s₁ s₂) (unifySpineMF S fuel' s₁ s₂)) := by
  induction N with
  | zero =>
      intro fuel hfN fuel' _
      have h0 : fuel = 0 := Nat.le_zero.mp hfN
      subst h0
      exact unifyM_fuel_mono_zero fuel'
  | succ N IH =>
      intro fuel hfN fuel' hff
      cases fuel with
      | zero => exact unifyM_fuel_mono_zero fuel'
      | succ f =>
          obtain ⟨f', rfl⟩ : ∃ g, fuel' = g + 1 := ⟨fuel' - 1, by omega⟩
          have IH' := IH f (by omega) f' (by omega)
          refine ⟨fun S τ τ' => ?_, fun S s₁ s₂ => ?_⟩
          · cases τ <;> cases τ' <;>
              first
                | exact .inr rfl
                | exact UResM.Mono.seq (IH'.1 S _ _) (fun θ S' => IH'.1 S' _ _)
                | exact IH'.2 S _ _
          · cases s₁ with
            | nil => exact .inr rfl
            | cons a s₁ =>
              cases s₂ with
              | nil => exact .inr rfl
              | cons b s₂ =>
                simp only [unifySpineMF]
                cases hsl : stripL (a :: s₁) (b :: s₂) with
                | some p => obtain ⟨t₁, t₂⟩ := p; exact IH'.2 S t₁ t₂
                | none =>
                cases hsr : stripR (a :: s₁) (b :: s₂) with
                | some p => obtain ⟨t₁, t₂⟩ := p; exact IH'.2 S t₁ t₂
                | none =>
                cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
                | some r => exact .inr rfl
                | none =>
                cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
                | some r => exact .inr rfl
                | none =>
                cases hml : matchL (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hml2 : matchL (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hmr : matchR (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hmr2 : matchR (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hg : groundMatch (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨τ0, τ0', t₁, t₂⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨τ0', τ0, t₂, t₁⟩ := p
                    exact UResM.Mono.seq (IH'.1 S τ0 τ0') (fun θ S' => IH'.2 S' _ _)
                | none =>
                cases he1 : expandL S (a :: s₁) (b :: s₂) with
                | some p =>
                    obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
                    exact UResM.Mono.expandRes S β0 l0 τ0
                      (IH'.2 S.fresh.2.fresh.2 t₁ t₂)
                | none =>
                cases he2 : expandL S (b :: s₂) (a :: s₁) with
                | some p =>
                    obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
                    exact UResM.Mono.expandRes S β0 l0 τ0
                      (IH'.2 S.fresh.2.fresh.2 t₁ t₂)
                | none => exact .inr rfl

-- ⊢  a REACHED row verdict is fuel-independent
theorem unifySpineMF_fuel_mono {B : Type} [DecidableEq B] {S : Supply}
    {fuel fuel' : Nat} {s₁ s₂ : List (Atom B)} (h : fuel ≤ fuel')
    (hne : unifySpineMF S fuel s₁ s₂ ≠ .outOfFuel) :
    unifySpineMF S fuel' s₁ s₂ = unifySpineMF S fuel s₁ s₂ :=
  ((unifyM_fuel_mono fuel fuel (Nat.le_refl _) fuel' h).2 S s₁ s₂).resolve_left hne

-- ⊢  … and a reached type verdict
theorem unifyTyF_fuel_mono {B : Type} [DecidableEq B] {S : Supply}
    {fuel fuel' : Nat} {τ τ' : Ty B} (h : fuel ≤ fuel')
    (hne : unifyTyF S fuel τ τ' ≠ .outOfFuel) :
    unifyTyF S fuel' τ τ' = unifyTyF S fuel τ τ' :=
  ((unifyM_fuel_mono fuel fuel (Nat.le_refl _) fuel' h).1 S τ τ').resolve_left hne

-- ⊢  … lifted to the entry points
theorem unifyRowM_fuel_mono {B : Type} [DecidableEq B] {fuel fuel' : Nat}
    {ρ₁ ρ₂ : Row B} (h : fuel ≤ fuel') (hne : unifyRowM fuel ρ₁ ρ₂ ≠ .outOfFuel) :
    unifyRowM fuel' ρ₁ ρ₂ = unifyRowM fuel ρ₁ ρ₂ :=
  unifySpineMF_fuel_mono h hne


end MinimalCalculus
