-- P6: the mgu statement and the stuck leg, plus the NEXT roadmap.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Clash

namespace MinimalCalculus

-- ## The mgu statement, in one place
-- Soundness and completeness together: a success DESCRIBES the unifier set.
-- ⊢  unifyRowM fuel ρ₁ ρ₂ = success s _   ⟹
--      every θ meeting s unifies ρ₁, ρ₂,  and every unifier of ρ₁, ρ₂ extends
--      (without moving on the problem's own variables) to one meeting s
theorem unifyRowM_success_iff {B : Type} [DecidableEq B] {fuel : Nat} {ρ₁ ρ₂ : Row B}
    {s : Sol B} {S' : Supply} (h : unifyRowM fuel ρ₁ ρ₂ = .success s S') :
    (∀ θ : TySubst B, Sol.Sat θ s → Unifies θ ρ₁ ρ₂) ∧
    (∀ θ : TySubst B, Unifies θ ρ₁ ρ₂ → ∃ θ' : TySubst B,
        AgreeOn θ θ' (sFtv ρ₁.toSpine ++ sFtv ρ₂.toSpine) ∧ Sol.Sat θ' s) :=
  ⟨fun _ hsat => unifyRowM_success_sound h hsat,
   fun _ hu => unifyRowM_success_complete h hu⟩


------------------ P6: THE BASE-ARM DISPATCH, STEP 2 ------------------------
-- `hbase` — "a terminal stuck configuration has no mgu" — is the one hypothesis
-- the trichotomy still reduces to. Step 1 is stuck_leading_shape: with stripL
-- and both matchL directions dead, the two leading atoms take one of four
-- shapes. Step 2 is here — a terminal configuration also has both U-expand
-- directions dead, and uniqueHost refuses for exactly two reasons, so a leading
-- field facing the other side means either
--   * ≥ 2 candidate hosts — Wand's shape, killed by vars_vs_field_no_mgu, or
--   * the label already occurs there, necessarily BEHIND a variable (matchL is
--     dead) — the two-sided shape, killed by two_sided_no_mgu.
-- Step 3 remains: run those witnesses at the general shape.

-- ⊢  uniqueHost refuses for exactly two reasons
theorem uniqueHost_none {B : Type} {l : Label} {s : List (Atom B)}
    (h : uniqueHost l s = none) :
    (∀ β, sVarSeq s ≠ [β]) ∨ 0 < sFieldCount l s := by
  unfold uniqueHost at h
  cases hvs : sVarSeq s with
  | nil => exact .inl (fun _ hb => by cases hb)
  | cons γ t =>
      cases t with
      | cons _ _ => exact .inl (fun _ hb => by cases hb)
      | nil =>
          right
          rw [hvs] at h
          simp only at h
          split at h
          · cases h
          · next hc => exact Nat.pos_of_ne_zero hc

-- ⊢  … and so does U-expand, when the leading atom IS a field
theorem expandL_none_field {B : Type} {S : Supply} {l : Label} {τ : Ty B}
    {t₁ s₂ : List (Atom B)} (h : expandL S (.field l τ :: t₁) s₂ = none) :
    (∀ β, sVarSeq s₂ ≠ [β]) ∨ 0 < sFieldCount l s₂ := by
  simp only [expandL] at h
  cases hh : uniqueHost l s₂ with
  | none => exact uniqueHost_none hh
  | some γ => rw [hh] at h; cases h

-- ⊢  "no unique variable" plus "at least one variable" means TWO candidate hosts
theorem two_vars_of_not_singleton {vs : List TyVar}
    (h : ∀ γ, vs ≠ [γ]) (hne : vs ≠ []) : 2 ≤ vs.length := by
  cases vs with
  | nil => exact absurd rfl hne
  | cons x t =>
      cases t with
      | nil => exact absurd rfl (h x)
      | cons y u => simp only [List.length_cons]; omega

theorem sVarSeq_var_cons {B : Type} (β : TyVar) (s : List (Atom B)) :
    sVarSeq (Atom.var β :: s) = β :: sVarSeq s := rfl

-- STEP 2 OF THE DISPATCH. Every shape stuck_leading_shape allows, refined by
-- what U-expand's refusal adds. Shape (1) — two distinct leading variables — is
-- the only one U-expand says nothing about; it is the non-commutativity
-- territory (allvar_swap_no_mgu), where no leading field exists to host.
theorem stuck_leading_shape_expand {B : Type} {S : Supply} {a b : Atom B}
    {s₁ s₂ : List (Atom B)}
    (hsl : stripL (a :: s₁) (b :: s₂) = none)
    (hml : matchL (a :: s₁) (b :: s₂) = none)
    (hml2 : matchL (b :: s₂) (a :: s₁) = none)
    (he1 : expandL S (a :: s₁) (b :: s₂) = none)
    (he2 : expandL S (b :: s₂) (a :: s₁) = none) :
    (∃ α β, a = .var α ∧ b = .var β ∧ α ≠ β) ∨
    (∃ α l' τ', a = .var α ∧ b = .field l' τ' ∧
       ((∀ γ, sVarSeq (a :: s₁) ≠ [γ]) ∨ 0 < sFieldCount l' (a :: s₁))) ∨
    (∃ l τ β, a = .field l τ ∧ b = .var β ∧
       ((∀ γ, sVarSeq (b :: s₂) ≠ [γ]) ∨ 0 < sFieldCount l (b :: s₂))) ∨
    (∃ l τ l' τ', a = .field l τ ∧ b = .field l' τ' ∧ l ≠ l' ∧
      windowExtract l (b :: s₂) = none ∧ windowExtract l' (a :: s₁) = none ∧
      ((∀ γ, sVarSeq (b :: s₂) ≠ [γ]) ∨ 0 < sFieldCount l (b :: s₂)) ∧
      ((∀ γ, sVarSeq (a :: s₁) ≠ [γ]) ∨ 0 < sFieldCount l' (a :: s₁))) := by
  rcases stuck_leading_shape hsl hml hml2 with
    ⟨α, β, ha, hb, hne⟩ | ⟨α, l', τ', ha, hb⟩ | ⟨l, τ, β, ha, hb⟩
    | ⟨l, τ, l', τ', ha, hb, hlne, hw1, hw2⟩
  · exact .inl ⟨α, β, ha, hb, hne⟩
  · subst ha; subst hb
    exact .inr (.inl ⟨α, l', τ', rfl, rfl, expandL_none_field he2⟩)
  · subst ha; subst hb
    exact .inr (.inr (.inl ⟨l, τ, β, rfl, rfl, expandL_none_field he1⟩))
  · subst ha; subst hb
    exact .inr (.inr (.inr ⟨l, τ, l', τ', rfl, rfl, hlne, hw1, hw2,
      expandL_none_field he1, expandL_none_field he2⟩))

-- The reading of shape (3), spelled out: a leading field facing a leading
-- VARIABLE leaves exactly the two configurations the base techniques handle.
-- ⊢  (l:τ | s₁) ≐ᵣ (β | s₂) terminal  ⟹
--      the right side has ≥ 2 variables (count-shrink), or it already carries
--      an l-field — necessarily behind a variable (rigidity)
theorem stuck_field_vs_var {B : Type} {S : Supply} {l : Label} {τ : Ty B}
    {β : TyVar} {s₁ s₂ : List (Atom B)}
    (he1 : expandL S (.field l τ :: s₁) (.var β :: s₂) = none) :
    2 ≤ (sVarSeq (Atom.var β :: s₂)).length ∨ 0 < sFieldCount l (Atom.var β :: s₂) := by
  rcases expandL_none_field he1 with h | h
  · exact .inl (two_vars_of_not_singleton h (by rw [sVarSeq_var_cons]; exact fun hc => by cases hc))
  · exact .inr h



------------------ P6: THE FOURTH LEG ---------------------------------------
-- WHAT THIS LEG IS NOT. There used to be a `unifyM_stuck_no_mgu` here: an
-- induction on fuel reducing `.stuck ⟹ ¬HasMgu` to four hypotheses (hbase,
-- hexp, hsolve, hsolveTy), each threading an accumulated predicate `Q`. It is
-- deleted, because the theorem it was reducing is FALSE:
--
--   * `Refutations.stuck_masks_mgu` — the driver answers `.stuck` on
--     (k:{β|α} | β) ≐ᵣ (k:{l:𝓫} | l:𝓫), which has the unique mgu
--     β ≔ (l:𝓫), α ≔ ε. `UResM.seq` propagates the ambiguous sub-equation
--     {β|α} ≐ {l:𝓫} before the residual β ≐ᵣ (l:𝓫), which pins β, is looked
--     at. So `.stuck` is a CONSERVATIVE verdict, exactly like `.occurs`.
--   * `Refutations.hbase_shape_false` / `hbase_stableQ_false` — the four
--     hypotheses were also false in their own right, an unconstrained `Q`
--     conjunct being able to shrink a unifier set down to one with an mgu.
--     That is the shadow of the first point: the `Q`-threading assumed a stuck
--     conjunct makes the conjunction ambiguous.
--
-- NOR IS IT the retreat to TERMINAL configurations. `TerminalNoMgu` (Defs.lean)
-- is refuted too, by `Refutations.terminalNoMgu_false`: terminality says "no
-- move fires", a fact about the MOVES, and on (l:{w}) ≐ᵣ (w | v) one of the two
-- candidate placements is ruled out by an occurs violation the guards cannot
-- see, leaving a unique — hence most general — unifier.
--
-- WHAT THE LEG ACTUALLY IS, then: the SPECIFIC no-mgu theorems (NoMgu.lean —
-- vars_vs_field_no_mgu for Wand, two_sided_no_mgu, allvar_swap_no_mgu, each
-- also at the `On` level), together with the conservativity examples. There is
-- no general converse to prove. The dispatch below stays useful as the case
-- analysis a side-condition-guarded version would still go through.
--
-- The lemmas kept below (which arms can answer `.stuck` at all, and ≐'s
-- congruence iffs) are facts about the driver in their own right, and are what
-- a future algorithm-level statement would still be built from.

-- ## ≐'s congruence arms are pointwise iffs (so they need no hypothesis)
theorem tyUnifies_fn_iff {B : Type} (θ : TySubst B) (a₁ b₁ a₂ b₂ : Ty B) :
    TyUnifies θ (.fn a₁ b₁) (.fn a₂ b₂) ↔ (TyUnifies θ a₁ a₂ ∧ TyUnifies θ b₁ b₂) := by
  constructor
  · intro h
    obtain ⟨σ₁, σ₂, heq, hA, hB⟩ := TyEquiv.fn_inv
      (show TyEquiv (Ty.fn (a₁.applySubst θ) (b₁.applySubst θ)) _ from h)
    simp only [Ty.applySubst, Ty.fn.injEq] at heq
    obtain ⟨rfl, rfl⟩ := heq
    exact ⟨hA, hB⟩
  · exact fun ⟨hA, hB⟩ => TyEquiv.fn hA hB

theorem tyUnifies_rcd_iff {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) :
    TyUnifies θ (.rcd ρ₁) (.rcd ρ₂) ↔ Unifies θ ρ₁ ρ₂ := by
  constructor
  · intro h
    obtain ⟨ρ', heq, hR⟩ :=
      TyEquiv.rcd_inv (show TyEquiv (Ty.rcd (ρ₁.applySubst θ)) _ from h)
    simp only [Ty.applySubst, Ty.rcd.injEq] at heq
    obtain rfl := heq
    exact hR
  · exact fun h => TyEquiv.rcd h

theorem unifies_toSpine_iff {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) :
    Unifies θ (ofSpine ρ₁.toSpine) (ofSpine ρ₂.toSpine) ↔ Unifies θ ρ₁ ρ₂ := by
  have e₁ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₁)
  have e₂ := RowEquiv.applySubst θ (Row.toSpine_equiv ρ₂)
  exact ⟨fun h => e₁.trans (h.trans e₂.symm), fun h => e₁.symm.trans (h.trans e₂)⟩

-- ## Which arms can answer `stuck` at all
theorem bindTy_ne_stuck {B : Type} {S : Supply} {α : TyVar} {τ : Ty B} :
    bindTy S α τ ≠ .stuck := by
  intro h; unfold bindTy at h
  split at h
  · cases h
  · split at h <;> cases h

theorem solveVarM_ne_stuck {B : Type} {S : Supply} {s₁ s₂ : List (Atom B)} :
    solveVarM S s₁ s₂ ≠ some .stuck := by
  intro h
  cases s₁ with
  | nil => simp [solveVarM] at h
  | cons a r =>
    cases a with
    | field _ _ => simp [solveVarM] at h
    | var α =>
      cases r with
      | cons _ _ => simp [solveVarM] at h
      | nil => simp only [solveVarM] at h; split at h <;> simp at h

theorem expandResM_stuck {B : Type} {S : Supply} {β : TyVar} {l : Label} {τ : Ty B}
    {r : UResM B} (h : expandResM S β l τ r = .stuck) : r = .stuck := by
  cases r with
  | success _ _ => cases h
  | clash => cases h
  | occurs => cases h
  | stuck => rfl
  | outOfFuel => cases h

theorem UResM.seq_stuck {B : Type} {r : UResM B} {k : TySubst B → Supply → UResM B}
    (h : r.seq k = .stuck) :
    r = .stuck ∨ ∃ s S, r = .success s S ∧ k s.toSubst S = .stuck := by
  cases r with
  | success s S =>
      refine .inr ⟨s, S, rfl, ?_⟩
      simp only [UResM.seq] at h
      revert h; cases hk : k s.toSubst S with
      | success s' S' => intro h; cases h
      | clash => intro h; cases h
      | occurs => intro h; cases h
      | stuck => intro _; rfl
      | outOfFuel => intro h; cases h
  | clash => cases h
  | occurs => cases h
  | stuck => exact .inl rfl
  | outOfFuel => cases h

-- ⊢  the dispatch, packaged from a `Terminal` record — the entry point Phase B
-- proves `TerminalNoMgu` through.
theorem terminal_leading_shape {B : Type} {S : Supply} {a b : Atom B}
    {s₁ s₂ : List (Atom B)} (ht : Terminal S (a :: s₁) (b :: s₂)) :
    (∃ α β, a = .var α ∧ b = .var β ∧ α ≠ β) ∨
    (∃ α l' τ', a = .var α ∧ b = .field l' τ' ∧
       ((∀ γ, sVarSeq (a :: s₁) ≠ [γ]) ∨ 0 < sFieldCount l' (a :: s₁))) ∨
    (∃ l τ β, a = .field l τ ∧ b = .var β ∧
       ((∀ γ, sVarSeq (b :: s₂) ≠ [γ]) ∨ 0 < sFieldCount l (b :: s₂))) ∨
    (∃ l τ l' τ', a = .field l τ ∧ b = .field l' τ' ∧ l ≠ l' ∧
      windowExtract l (b :: s₂) = none ∧ windowExtract l' (a :: s₁) = none ∧
      ((∀ γ, sVarSeq (b :: s₂) ≠ [γ]) ∨ 0 < sFieldCount l (b :: s₂)) ∧
      ((∀ γ, sVarSeq (a :: s₁) ≠ [γ]) ∨ 0 < sFieldCount l' (a :: s₁))) :=
  stuck_leading_shape_expand ht.hstripL ht.hmatchL₁ ht.hmatchL₂ ht.hexpandL ht.hexpandR




------------------------------------ NEXT ------------------------------------
-- WHERE ≐ / ≐ᵣ STANDS (proof-plan.md is the live plan).
--
-- The algorithm is `unifyTyF` / `unifySpineMF` — one mutual block, structurally
-- recursive on fuel, so every worked example is a kernel-checked `rfl`
-- (Regressions.lean). Five verdicts: success / clash / occurs / stuck /
-- outOfFuel.
--
-- Three of the four legs are THEOREMS, at any fuel: unifyM_success_sound,
-- unifyM_success_complete (∃θ′/AgreeOn form; with soundness, the solution
-- DESCRIBES the unifier set — unifyRowM_success_iff), and
-- unifyM_clash_no_unifier. The fourth, unifyM_stuck_no_mgu, is a REDUCTION to
-- hbase, hexp and hsolve/hsolveTy. Supporting invariants: unifyM_fuel_mono and
-- unifyM_bounded.
--
-- OPEN, in the order the plan wants them:
--  * `HasMguOn V` / `InstanceOfOn V` — mgu RELATIVIZED to a variable set. The
--    strict `InstanceOf` over all variables is the wrong notion for an algorithm
--    that invents variables (the mismatch AgreeOn fixed for completeness, one
--    level up), and it is what blocks all four parked hypotheses above.
--  * hbase, step 3: run the three base-witness techniques (count-shrink,
--    rigidity, non-commutativity) at the GENERAL terminal shape. Steps 1 and 2
--    are done — stuck_leading_shape and stuck_leading_shape_expand, the latter
--    using U-expand's refusal to show a leading field faces either ≥ 2 candidate
--    hosts or a label already present behind a variable.
--  * TERMINATION: a fuel that provably suffices. The naive Rémy measure does not
--    close (renaming adds no fields, so the host side keeps count_l = 0 and the
--    same variable can be re-expanded at the same label); the bound has to come
--    from the other side's l-fields, which solve-and-apply can add.
--  * The occurs guard stays deliberately conservative (occurs_allVar_hasMgu).
--
-- MILESTONES ELSEWHERE THAT BUILD ON THIS FILE (algorithmic.typ, Open questions):
--  * Non-vacuity of qualified schemes: needs lookup_total (RowWF) plus a
--    freshness discipline for the result variables δ — P2/P5 now supply that
--    discipline (Supply/Avoids, SolBelow, AgreeOn, the substitution-ftv toolkit).
--  * STRICTNESS of the QTyped extension, and type safety for QTyped itself.
--  * The covering order ⊴ on qualified schemes (needed to STATE "the principal
--    type improves under reduction").
--  * Solver state S = (θ, Δ, W), stump wake-up, and the confluence argument that
--    the final state is independent of wake-up scheduling
--    (lookup_det + Discharge.mono_of_definite are the two pillars).


end MinimalCalculus
