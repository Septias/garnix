-- P4: the mutual driver unifyTyF / unifySpineMF, its entry points, and fuel monotonicity.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Reflection

namespace MinimalCalculus

--------------------- P4: THE MUTUAL ≐ / ≐ᵣ DRIVER ---------------------------
-- The row pass solves the type equations it
-- discovers on the spot, by calling the type pass, and applies the solution to
-- the residual before recursing. That is what makes the stuck verdict mean
-- something: an equation is discharged, or fatal, or itself stuck — never
-- merely deferred.
--
-- Three things the row pass alone did not need:
--  * a success carries its SUPPLY, because a type equation solved inside a
--    field may expand a row variable, and the invented tail travels into the
--    residual; without threading, the residual call would re-draw that name;
--  * `outOfFuel` is a separate verdict, so the fuel lemma is a structural
--    induction rather than a termination measure;
--  * `[DecidableEq B]`: ≐ must decide 𝓫 = 𝓫′.


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

-- THE ε-COLLAPSE, COMPUTED. α ≐ᵣ (β | α | γ) was the standing incompleteness
-- witness of the occurs guard: `occurs_allVar_unifiable` showed it unifiable and
-- `occurs_allVar_hasMgu` showed the unifier FORCED, yet the algorithm answered
-- `.occurs`. It now returns exactly that mgu. α occurs ONCE on the spine, so α
-- itself stays free — ε | α | ε ≈ α whatever α is — and only β, γ collapse.
-- ⊢  unifyRowM α (β | α | γ)  =  success [β ≔ ε, γ ≔ ε]
theorem allVar_collapse_reported {B : Type} [DecidableEq B] :
    unifyRowM (B := B) 20 (.var "a") (.cat (.var "b") (.cat (.var "a") (.var "c")))
      = .success ⟨[], [("b", .empty), ("c", .empty)]⟩ ⟨2⟩ := rfl

-- …and at multiplicity TWO the counting closes on α as well, so every spine
-- variable collapses — α listed at each of its occurrences (`rowLookup` reads
-- the first, so the repetition is harmless).
-- ⊢  unifyRowM α (β | α | α | γ)  =  success [β ≔ ε, α ≔ ε, α ≔ ε, γ ≔ ε]
theorem allVar_collapse_reported_k2 {B : Type} [DecidableEq B] :
    unifyRowM (B := B) 20 (.var "a")
        (.cat (.var "b") (.cat (.var "a") (.cat (.var "a") (.var "c"))))
      = .success ⟨[], [("b", .empty), ("a", .empty), ("a", .empty), ("c", .empty)]⟩ ⟨2⟩ := rfl

-- WHAT DROPPING U-EXPAND COSTS, computed. This is the crossfield shape, and it
-- is the price of the removal made concrete: a unifier EXISTS — the prose at
-- :1006 derives it by hand, and U-expand used to find exactly it, binding
-- β ≔ (l:δ | β′), α ≔ (m:𝓫 | β′ | ε) with 𝓫 ≐ δ solved rather than parked — and
-- the driver no longer finds it.
--
-- `.stuck` is the CONSERVATIVE verdict, not a wrong one: it claims nothing, and
-- downstream it degrades to `★` with a W-flag. The soundness contract
-- ("success ⟹ the solution unifies") is untouched; only coverage shrinks. Cf.
-- `stuck_masks_mgu`, which has always been this shape.
--
-- This is the single most important regression on this branch: if it ever goes
-- back to `.success`, an expansion arm has been reintroduced somewhere.
-- ⊢  unifyRowM (l:𝓫 | α) (m:𝓫 | β)  =  stuck
theorem crossfield_stuck {B : Type} [DecidableEq B] (b : B) :
    unifyRowM (B := B) 20 (.cat (.sing "l" (.base b)) (.var "a"))
                          (.cat (.sing "m" (.base b)) (.var "b")) = .stuck := rfl

-- ## Fuel monotonicity
-- No closed-form bound yet: solve-and-apply grows the spine, and the variable
-- count can grow too (a type equation solved inside a field may expand a row
-- variable and hand the invented tail to the residual), so no lexicographic
-- measure decreases. The missing ingredient is a Rémy-style argument on the
-- problem's finitely many labels.
--
-- `outOfFuel` makes that separable: the lemma below says a verdict that was
-- REACHED never changes when the budget grows, which is all any leg except the
-- stuck one needs.


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



-- THE FUEL LEMMA, for both sorts at once (the mutual induction is on the
-- budget, with no measure hypothesis, because `outOfFuel` absorbs the
-- shortfall).
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
                -- Past the last recursive arm both remaining outcomes are fuel-
                -- independent constants. With U-expand gone there is no longer
                -- an expansion case carrying an induction hypothesis here.
                cases hpc : projClash (a :: s₁) (b :: s₂) with
                | true  => exact .inr rfl
                | false => exact .inr rfl

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
