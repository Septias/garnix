-- Move-reflection lemmas, the U-ground core, and agreement modulo fresh names.
--
-- Part of RowUnify; see RowUnify.lean for the overview.

import RowUnify.Solutions

namespace MinimalCalculus

-- ## ofSpine cons is definitionally a cat (kept as named rewrites).
-- ⊢  ofSpine (α :: s) = (α | ofSpine s)
theorem ofSpine_var_cons {B : Type} (α : TyVar) (s : List (Atom B)) :
    ofSpine (.var α :: s) = .cat (.var α) (ofSpine s) := rfl

-- ⊢  ofSpine ((l:τ) :: s) = (l:τ | ofSpine s)
theorem ofSpine_field_cons {B : Type} (l : Label) (τ : Ty B) (s : List (Atom B)) :
    ofSpine (.field l τ :: s) = .cat (.sing l τ) (ofSpine s) := rfl

-- ## Move-reflection lemmas
-- Each: "if θ unifies the residual (and satisfies the emitted binding/eq),
-- then θ unified the original pair." These are the per-rule soundness steps.

-- U-var-refl (left): a shared leading var contributes θα to both sides, so it
-- drops out of the equivalence — no constraint on θ.
-- Inversion: stripL succeeds only on a shared leading var.
-- ⊢  stripL s₁ s₂ = some (t₁,t₂)   ⟹   ∃ α. s₁ = α::t₁ ∧ s₂ = α::t₂
theorem stripL_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} :
    stripL s₁ s₂ = some (t₁, t₂) → ∃ α, s₁ = .var α :: t₁ ∧ s₂ = .var α :: t₂ := by
  cases s₁ with
  | nil => simp [stripL]
  | cons a₁ r₁ =>
    cases a₁ with
    | field _ _ => simp [stripL]
    | var α =>
      cases s₂ with
      | nil => simp [stripL]
      | cons a₂ r₂ =>
        cases a₂ with
        | field _ _ => simp [stripL]
        | var β =>
          intro h
          simp only [stripL] at h
          split at h
          · rename_i hαβ
            simp only [Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl⟩ := h
            exact ⟨α, rfl, by rw [hαβ]⟩
          · simp at h

-- Inversion: stripR succeeds only on a shared trailing var.
-- ⊢  stripR s₁ s₂ = some (t₁,t₂)   ⟹   ∃ α. s₁ = t₁++[α] ∧ s₂ = t₂++[α]
theorem stripR_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} :
    stripR s₁ s₂ = some (t₁, t₂) → ∃ α, s₁ = t₁ ++ [.var α] ∧ s₂ = t₂ ++ [.var α] := by
  intro h
  unfold stripR at h
  cases hsl : stripL s₁.reverse s₂.reverse with
  | none => rw [hsl] at h; simp at h
  | some p =>
    rw [hsl] at h
    obtain ⟨u₁, u₂⟩ := p
    simp only [Option.some.injEq, Prod.mk.injEq] at h
    obtain ⟨rfl, rfl⟩ := h
    obtain ⟨α, hr₁, hr₂⟩ := stripL_inv hsl
    refine ⟨α, ?_, ?_⟩
    · rw [← List.reverse_reverse s₁, hr₁]; simp
    · rw [← List.reverse_reverse s₂, hr₂]; simp

-- U-var-refl (left): a shared leading var contributes θα to both sides, so it
-- drops out of the equivalence — no constraint on θ.
-- ⊢  stripL s₁ s₂ = some (t₁,t₂),  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem stripL_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv hstrip
  simp only [ofSpine_var_cons, Row.applySubst]
  exact RowEquiv.cat (.refl _) hrec

-- U-var-refl (right): the same at the trailing end, via ofSpine over append.
-- ⊢  stripR s₁ s₂ = some (t₁,t₂),  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem stripR_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv hstrip
  have e₁ := RowEquiv.applySubst θ (ofSpine_append t₁ [Atom.var α])
  have e₂ := RowEquiv.applySubst θ (ofSpine_append t₂ [Atom.var α])
  simp only [ofSpine, Row.applySubst] at e₁ e₂
  exact e₁.trans ((RowEquiv.cat hrec (.refl _)).trans e₂.symm)

-- U-field (left): a leading field matched against the other side's window.
-- windowExtract bubbles the matched field to the front (distinct-label comm).
-- ⊢  windowExtract l s = some (τ,s')   ⟹   ofSpine s ≈ᵣ (l:τ | ofSpine s')
theorem windowExtract_equiv {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {s' : List (Atom B)} →
    windowExtract l s = some (τ, s') →
    RowEquiv (ofSpine s) (.cat (.sing l τ) (ofSpine s'))
  | [], _, _, h => by simp [windowExtract] at h
  | .var β :: s, _, _, h => by simp [windowExtract] at h
  | .field l' τ₀ :: s, τ, s', h => by
      simp only [windowExtract] at h
      split at h
      · rename_i hl
        subst hl
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact RowEquiv.refl _
      · rename_i hl
        split at h
        · rename_i τ'' s'' hwe
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have ih := windowExtract_equiv l s hwe
          simp only [ofSpine_field_cons]
          exact (RowEquiv.cat (.refl _) ih).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (.comm hl) (.refl _)).trans RowEquiv.assoc))
        · simp at h

-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem matchL_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  cases s₁ with
  | nil => simp [matchL] at hmatch
  | cons a₁ r₁ =>
    cases a₁ with
    | var α => simp [matchL] at hmatch
    | field l τ₀ =>
      simp only [matchL] at hmatch
      split at hmatch
      · rename_i τ'' s₂' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
        obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
        have hw := RowEquiv.applySubst θ (windowExtract_equiv l s₂ hwe)
        simp only [ofSpine_field_cons, Row.applySubst]
        refine RowEquiv.trans ?_ hw.symm
        simp only [Row.applySubst]
        exact RowEquiv.cat (.sing heq) hrec
      · simp at hmatch

-- Base case: an all-vars remainder, each solved to ε, unifies with the empty
-- side (allVarsEmpty forces every var to ε).
-- ⊢  allVarsEmpty s = some σ,  θ ⊨ σ   ⟹   θ(ofSpine s) ≈ᵣ ε
theorem allVarsEmpty_sound {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → SolSat θ σ →
    RowEquiv ((ofSpine s).applySubst θ) .empty
  | [], _, hσ, _ => by
      simp only [allVarsEmpty, Option.some.injEq] at hσ; subst hσ; exact .refl _
  | .field _ _ :: _, _, hσ, _ => by simp [allVarsEmpty] at hσ
  | .var α :: s, σ, hσ, hsol => by
      simp only [allVarsEmpty] at hσ
      cases hs : allVarsEmpty s with
      | none => rw [hs] at hσ; simp at hσ
      | some σ' =>
        rw [hs] at hσ
        have hσ' : σ = (α, Row.empty) :: σ' := (Option.some.inj hσ).symm
        subst hσ'
        have hhead := hsol (α, Row.empty) (by simp)
        have htail : SolSat θ σ' := fun p hp => hsol p (by simp [hp])
        have ih := allVarsEmpty_sound s hs htail
        simp only [ofSpine_var_cons, Row.applySubst]
        exact (RowEquiv.cat hhead ih).trans RowEquiv.unitL

-- Completeness counterpart: if θ makes ofSpine s collapse to ε (the exhausted-
-- side unifier), then θ satisfies every ε-binding allVarsEmpty emitted.
-- ⊢  allVarsEmpty s = some σ,  θ(ofSpine s) ≈ᵣ ε   ⟹   SolSat θ σ
theorem allVarsEmpty_complete {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) → {σ : List (TyVar × Row B)} →
    allVarsEmpty s = some σ → RowEquiv ((ofSpine s).applySubst θ) .empty →
    SolSat θ σ
  | [], _, hσ, _ => by
      simp only [allVarsEmpty, Option.some.injEq] at hσ; subst hσ
      intro p hp; simp at hp
  | .field _ _ :: _, _, hσ, _ => by simp [allVarsEmpty] at hσ
  | .var α :: s, σ, hσ, hu => by
      simp only [allVarsEmpty] at hσ
      cases hs : allVarsEmpty s with
      | none => rw [hs] at hσ; simp at hσ
      | some σ' =>
        rw [hs] at hσ
        have hσ' : σ = (α, Row.empty) :: σ' := (Option.some.inj hσ).symm
        subst hσ'
        simp only [ofSpine_var_cons, Row.applySubst] at hu
        obtain ⟨hα, hrest⟩ := hu.cat_empty_split
        have ih := allVarsEmpty_complete s hs hrest
        intro p hp
        rcases List.mem_cons.mp hp with rfl | hp'
        · simp only [Row.applySubst]; exact hα
        · exact ih p hp'

-- Row reversal (reverses cat order): the algebraic image of List.reverse on
-- spines. Used only to transport windowExtract_equiv from s.reverse to s — no
-- List.reverseRecOn is available (no Batteries dep), so we go through revRow.
def revRow {B : Type} : Row B → Row B
  | .empty     => .empty
  | .var α     => .var α
  | .sing l τ  => .sing l τ
  | .cat ρ₁ ρ₂ => .cat (revRow ρ₂) (revRow ρ₁)

-- ⊢  revRow (revRow ρ) = ρ
theorem revRow_involutive {B : Type} : (ρ : Row B) → revRow (revRow ρ) = ρ
  | .empty     => rfl
  | .var _     => rfl
  | .sing _ _  => rfl
  | .cat a b   => by simp only [revRow, revRow_involutive a, revRow_involutive b]

-- ⊢  a ≈ᵣ b   ⟹   revRow a ≈ᵣ revRow b
theorem RowEquiv.revRow {B : Type} :
    {a b : Row B} → RowEquiv a b → RowEquiv (revRow a) (revRow b)
  | _, _, .refl _      => .refl _
  | _, _, .symm h      => (RowEquiv.revRow h).symm
  | _, _, .trans h₁ h₂ => (RowEquiv.revRow h₁).trans (RowEquiv.revRow h₂)
  | _, _, .sing hty    => .sing hty
  | _, _, .cat h₁ h₂   => .cat (RowEquiv.revRow h₂) (RowEquiv.revRow h₁)
  | _, _, .assoc       => RowEquiv.assoc.symm
  | _, _, .unitL       => RowEquiv.unitR
  | _, _, .unitR       => RowEquiv.unitL
  | _, _, .comm hne    => RowEquiv.comm (fun h => hne h.symm)

-- ofSpine of a reversed spine is the row-reversal of ofSpine (mod ≈).
-- ⊢  ofSpine (reverse as) ≈ᵣ revRow (ofSpine as)
theorem ofSpine_reverse_equiv {B : Type} : (as : List (Atom B)) →
    RowEquiv (ofSpine as.reverse) (revRow (ofSpine as))
  | [] => .refl _
  | .field l τ :: t => by
      rw [List.reverse_cons]
      refine (ofSpine_append t.reverse [Atom.field l τ]).trans ?_
      simp only [ofSpine, revRow]
      exact RowEquiv.cat (ofSpine_reverse_equiv t) RowEquiv.unitR
  | .var α :: t => by
      rw [List.reverse_cons]
      refine (ofSpine_append t.reverse [Atom.var α]).trans ?_
      simp only [ofSpine, revRow]
      exact RowEquiv.cat (ofSpine_reverse_equiv t) RowEquiv.unitR

-- U-field (right): the trailing-end mirror of matchL. matchR runs windowExtract
-- on the REVERSED spines, so the matched field sits in the trailing var-free
-- window and bubbles to the RIGHT end past distinct-label fields only. We get
-- this by transporting windowExtract_equiv through revRow.
-- ⊢  windowExtract l (reverse s) = some (τ,q)
--        ⟹   ofSpine s ≈ᵣ (ofSpine (reverse q) | l:τ)
theorem windowExtract_reverse_equiv {B : Type} (l : Label) (s : List (Atom B))
    {τ : Ty B} {q : List (Atom B)}
    (h : windowExtract l s.reverse = some (τ, q)) :
    RowEquiv (ofSpine s) (.cat (ofSpine q.reverse) (.sing l τ)) := by
  have hwe := windowExtract_equiv l s.reverse h
  have h1 : RowEquiv (revRow (ofSpine s)) (.cat (.sing l τ) (ofSpine q)) :=
    (ofSpine_reverse_equiv s).symm.trans hwe
  have h2 := RowEquiv.revRow h1
  rw [revRow_involutive (ofSpine s)] at h2
  simp only [revRow] at h2
  exact h2.trans (RowEquiv.cat (ofSpine_reverse_equiv q).symm (.refl _))

-- Inversion of a leading field-match.
-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂)
--        ⟹  ∃ l. s₁ = (l:τ)::t₁ ∧ windowExtract l s₂ = some (τ',t₂)
theorem matchL_inv {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B} :
    matchL s₁ s₂ = some (τ, τ', t₁, t₂) →
    ∃ l, s₁ = .field l τ :: t₁ ∧ windowExtract l s₂ = some (τ', t₂) := by
  cases s₁ with
  | nil => simp [matchL]
  | cons a₁ r₁ =>
    cases a₁ with
    | var α => simp [matchL]
    | field l τ₀ =>
      intro h
      simp only [matchL] at h
      split at h
      · rename_i τ'' s₂' hwe
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl, rfl, rfl⟩ := h
        exact ⟨l, rfl, hwe⟩
      · simp at h

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)      (trailing-field mirror)
theorem matchR_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  -- matchR = matchL on the reverses; recover the leading-field inversion there.
  unfold matchR at hmatch
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at hmatch; simp at hmatch
  | some p =>
    rw [hml] at hmatch
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    -- matchR returns (τa, τb, u₁.reverse, u₂.reverse); pin the theorem vars.
    simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
    obtain ⟨l, hrev, hwe⟩ := matchL_inv hml
    -- hrev : s₁.reverse = field l τa :: u₁ ; so s₁ = u₁.reverse ++ [field l τa]
    have hs₁ : s₁ = u₁.reverse ++ [Atom.field l τa] := by
      rw [← List.reverse_reverse s₁, hrev]; simp
    -- windowExtract l s₂.reverse = some (τb, u₂); right-bubble on s₂.
    have hs₂equiv := windowExtract_reverse_equiv l s₂ hwe
    have hs₁equiv : RowEquiv (ofSpine s₁) (.cat (ofSpine u₁.reverse) (.sing l τa)) := by
      rw [hs₁]
      exact (ofSpine_append u₁.reverse [Atom.field l τa]).trans
        (RowEquiv.cat (.refl _) RowEquiv.unitR)
    have e₁ := RowEquiv.applySubst θ hs₁equiv
    have e₂ := RowEquiv.applySubst θ hs₂equiv
    simp only [Row.applySubst] at e₁ e₂
    refine e₁.trans (RowEquiv.trans ?_ e₂.symm)
    exact RowEquiv.cat hrec (.sing heq)

-- ## FORWARD reflection: a unifier of the ORIGINAL unifies the RESIDUAL (+ eqs)
-- The converse of the *_reflect lemmas — the COMPLETENESS direction each move
-- needs. For strip this is just cancellativity (shared θα prefix/suffix). For
-- match it is leading/trailing-field cancellation (field_cancel_left/right),
-- which additionally EXTRACTS the emitted type equation θτ ≈ θτ'. Together these
-- say every move preserves the unifier set, so no-unifier propagates backwards.

theorem RowEquiv.field_cancel_right {B : Type} {l : Label} {τ₁ τ₂ : Ty B}
    {R₁ R₂ : Row B}
    (h : RowEquiv (.cat R₁ (.sing l τ₁)) (.cat R₂ (.sing l τ₂))) :
    TyEquiv τ₁ τ₂ ∧ RowEquiv R₁ R₂ := by
  obtain ⟨hty, hR⟩ := (h.revRow).field_cancel_left
  refine ⟨hty, ?_⟩
  have hRR := hR.revRow
  rwa [revRow_involutive, revRow_involutive] at hRR

-- ⊢  stripL s₁ s₂ = some (t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem stripL_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripL_inv hstrip
  simp only [ofSpine, Row.applySubst] at hu
  exact hu.cancel_cat_left

-- ⊢  stripR s₁ s₂ = some (t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem stripR_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨α, rfl, rfl⟩ := stripR_inv hstrip
  have e₁ := RowEquiv.applySubst θ (ofSpine_append t₁ [Atom.var α])
  have e₂ := RowEquiv.applySubst θ (ofSpine_append t₂ [Atom.var α])
  simp only [ofSpine, Row.applySubst] at e₁ e₂
  exact (e₁.symm.trans (hu.trans e₂)).cancel_cat_right

-- ## Strip moves reflect no-mgu (the unifier-set-preserving case of the lift)
-- stripL/stripR cancel a shared end-var, and reflect + reflect_fwd together show
-- they preserve the unifier set EXACTLY. So mgu-status transfers both ways: a
-- config that gets stuck purely by end-stripping inherits the no-mgu verdict of
-- its stripped core. (matchL/matchR/groundMatch emit a type equation, so they do
-- NOT preserve the unifier set — those need the harder augmented-witness lift.)
-- ⊢  stripL s₁ s₂ = some (t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--                                       HasMgu (ofSpine t₁)(ofSpine t₂))
theorem stripL_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripL s₁ s₂ = some (t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔ HasMgu (ofSpine t₁) (ofSpine t₂) :=
  hasMgu_congr (fun _ => ⟨fun hu => stripL_reflect_fwd hstrip hu,
                          fun hu => stripL_reflect hstrip hu⟩)

-- ⊢  stripR s₁ s₂ = some (t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--                                       HasMgu (ofSpine t₁)(ofSpine t₂))
theorem stripR_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)}
    (hstrip : stripR s₁ s₂ = some (t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔ HasMgu (ofSpine t₁) (ofSpine t₂) :=
  hasMgu_congr (fun _ => ⟨fun hu => stripR_reflect_fwd hstrip hu,
                          fun hu => stripR_reflect hstrip hu⟩)

-- End-to-end demonstration that the pieces compose: the Wand core wrapped in a
-- shared leading var. stripL peels γ, the stripped core is Wand's no-mgu config,
-- so the whole thing has no mgu — obtained mechanically from stripL_hasMgu_iff +
-- hasMgu_rowEquiv + wand_no_mgu_count. (This is exactly how the eventual lift
-- discharges its strip arms.)
-- ⊢  ¬ HasMgu (γ | β | α) (γ | l:𝓫)
theorem wand_under_strip_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ HasMgu (ofSpine [Atom.var "γ", .var "β", .var "α"])
             (ofSpine [Atom.var "γ", .field l (.base b)]) := by
  have hs : stripL ([Atom.var "γ", .var "β", .var "α"] : List (Atom B))
                   [Atom.var "γ", .field l (.base b)]
          = some ([.var "β", .var "α"], [.field l (.base b)]) := rfl
  rw [stripL_hasMgu_iff hs]
  simp only [ofSpine]
  rw [hasMgu_rowEquiv (ρ₁' := .cat (.var "β") (.var "α")) (ρ₂' := .sing l (.base b))
        (RowEquiv.cat (RowEquiv.refl _) RowEquiv.unitR) RowEquiv.unitR]
  exact wand_no_mgu_count b l

-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem matchL_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨l, rfl, hwe⟩ := matchL_inv hmatch
  have hs₂ := RowEquiv.applySubst θ (windowExtract_equiv l s₂ hwe)
  simp only [ofSpine, Row.applySubst] at hu hs₂
  exact (hu.trans hs₂).field_cancel_left

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem matchR_reflect_fwd {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  unfold matchR at hmatch
  cases hml : matchL s₁.reverse s₂.reverse with
  | none => rw [hml] at hmatch; simp at hmatch
  | some p =>
    rw [hml] at hmatch
    obtain ⟨τa, τb, u₁, u₂⟩ := p
    simp only [Option.some.injEq, Prod.mk.injEq] at hmatch
    obtain ⟨rfl, rfl, rfl, rfl⟩ := hmatch
    obtain ⟨l, hrev, hwe⟩ := matchL_inv hml
    have hs₁ : s₁ = u₁.reverse ++ [Atom.field l τa] := by
      rw [← List.reverse_reverse s₁, hrev]; simp
    have hs₂equiv := windowExtract_reverse_equiv l s₂ hwe
    have hs₁equiv : RowEquiv (ofSpine s₁) (.cat (ofSpine u₁.reverse) (.sing l τa)) := by
      rw [hs₁]
      exact (ofSpine_append u₁.reverse [Atom.field l τa]).trans
        (RowEquiv.cat (.refl _) RowEquiv.unitR)
    have e₁ := RowEquiv.applySubst θ hs₁equiv
    have e₂ := RowEquiv.applySubst θ hs₂equiv
    simp only [Row.applySubst] at e₁ e₂
    exact (e₁.symm.trans (hu.trans e₂)).field_cancel_right

-- ## U-ground: the reusable algebraic core
-- A field ≈-commutes past a row that is BOTH var-free and l-free. (Past a var
-- it would NOT commute — shadowing — so both hypotheses are essential; this is
-- exactly why groundMatch's soundness is conditional on the skipped vars being
-- l-free under θ, which the counting forces.)
-- ⊢  R var-free,  count_l(spine R) = 0   ⟹   (l:τ | R) ≈ᵣ (R | l:τ)
theorem field_comm_lfree {B : Type} (l : Label) (τ : Ty B) :
    (R : Row B) → R.SpineVarFree → sFieldCount l R.toSpine = 0 →
    RowEquiv (.cat (.sing l τ) R) (.cat R (.sing l τ))
  | .empty, _, _ => RowEquiv.unitR.trans RowEquiv.unitL.symm
  | .var _, hv, _ => nomatch hv
  | .sing l' τ', _, hc => by
      have hne : l' ≠ l := by
        intro h; subst h; simp [Row.toSpine, sFieldCount] at hc
      exact RowEquiv.comm (fun h => hne h.symm)
  | .cat R₁ R₂, .cat hv₁ hv₂, hc => by
      rw [Row.toSpine, sFieldCount_append] at hc
      have hc₁ : sFieldCount l R₁.toSpine = 0 := by omega
      have hc₂ : sFieldCount l R₂.toSpine = 0 := by omega
      have IH₁ := field_comm_lfree l τ R₁ hv₁ hc₁
      have IH₂ := field_comm_lfree l τ R₂ hv₂ hc₂
      exact RowEquiv.assoc.symm.trans
        ((RowEquiv.cat IH₁ (.refl _)).trans
          (RowEquiv.assoc.trans
            ((RowEquiv.cat (.refl _) IH₂).trans RowEquiv.assoc.symm)))

-- removeField pulls the matched l-field to the front — UNDER θ, provided every
-- variable of the spine is var-free and l-free under θ (vacuous when the spine
-- is var-free, e.g. the ground side). The var case is the only one that needs
-- field_comm_lfree; the field case only crosses distinct labels (comm).
-- ⊢  removeField l s = some (τ,t),
--       (∀ β ∈ vars(s). θβ var-free ∧ count_l(spine θβ) = 0)
--        ⟹   θ(ofSpine s) ≈ᵣ (l:θτ | θ(ofSpine t))
theorem removeField_equiv_of {B : Type} {θ : TySubst B} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) →
    (∀ β ∈ sVarSeq s, (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0) →
    RowEquiv ((ofSpine s).applySubst θ)
             (.cat (.sing l (τ.applySubst θ)) ((ofSpine t).applySubst θ))
  | [], _, _, h, _ => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h, hvars => by
      simp only [removeField] at h
      split at h
      · rename_i hl'
        subst hl'
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [ofSpine_field_cons, Row.applySubst]
        exact .refl _
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have hvars' : ∀ β ∈ sVarSeq s,
              (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0 :=
            fun β hβ => hvars β (by simp only [sVarSeq]; exact hβ)
          have IH := removeField_equiv_of l s hrem hvars'
          simp only [ofSpine_field_cons, Row.applySubst]
          exact (RowEquiv.cat (.refl _) IH).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (RowEquiv.comm hl') (.refl _)).trans RowEquiv.assoc))
        · simp at h
  | .var β :: s, τ, t, h, hvars => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        have hβ : (θ.row β).SpineVarFree ∧ sFieldCount l (θ.row β).toSpine = 0 :=
          hvars β (by simp [sVarSeq])
        have hvars' : ∀ γ ∈ sVarSeq s,
            (θ.row γ).SpineVarFree ∧ sFieldCount l (θ.row γ).toSpine = 0 :=
          fun γ hγ => hvars γ (by simp only [sVarSeq]; exact List.mem_cons_of_mem β hγ)
        have IH := removeField_equiv_of l s hrem hvars'
        simp only [ofSpine_var_cons, Row.applySubst]
        exact (RowEquiv.cat (.refl _) IH).trans
          (RowEquiv.assoc.symm.trans
            ((RowEquiv.cat
              (field_comm_lfree l (τ''.applySubst θ) (θ.row β) hβ.1 hβ.2).symm (.refl _)).trans
              RowEquiv.assoc))
      · simp at h

-- ## U-ground: the counting (measurement lemmas)
-- removeField keeps the variable sequence (it only deletes a concrete field).
-- ⊢  removeField l s = some (τ,t)   ⟹   vars(t) = vars(s)
theorem removeField_sVarSeq {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) → sVarSeq t = sVarSeq s
  | [], _, _, h => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h; rfl
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          simp only [sVarSeq]; exact removeField_sVarSeq l s hrem
        · simp at h
  | .var β :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sVarSeq]; rw [removeField_sVarSeq l s hrem]
      · simp at h

-- removeField deletes exactly one l-field.
-- ⊢  removeField l s = some (τ,t)   ⟹   count_l(s) = count_l(t) + 1
theorem removeField_sFieldCount {B : Type} (l : Label) :
    (s : List (Atom B)) → {τ : Ty B} → {t : List (Atom B)} →
    removeField l s = some (τ, t) → sFieldCount l s = sFieldCount l t + 1
  | [], _, _, h => by simp [removeField] at h
  | .field l' τ₀ :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i hl'
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sFieldCount, if_pos hl']; omega
      · rename_i hl'
        split at h
        · rename_i τ'' s'' hrem
          simp only [Option.some.injEq, Prod.mk.injEq] at h
          obtain ⟨rfl, rfl⟩ := h
          have hcnt := removeField_sFieldCount l s hrem
          simp only [sFieldCount, if_neg hl']
          omega
        · simp at h
  | .var β :: s, τ, t, h => by
      simp only [removeField] at h
      split at h
      · rename_i τ'' s'' hrem
        simp only [Option.some.injEq, Prod.mk.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        simp only [sFieldCount]
        rw [removeField_sFieldCount l s hrem]
      · simp at h

-- A var-free spine stays var-free under θ (θ introduces vars only via row-vars).
-- ⊢  vars(s) = []   ⟹   vars(spine (θ(ofSpine s))) = []
theorem varSeq_applySubst_nil {B : Type} (θ : TySubst B) :
    (s : List (Atom B)) → sVarSeq s = [] →
    sVarSeq ((ofSpine s).applySubst θ).toSpine = []
  | [], _ => rfl
  | .field l τ :: s, hs => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sVarSeq_append,
        sVarSeq, List.nil_append]
      exact varSeq_applySubst_nil θ s (by simpa [sVarSeq] using hs)
  | .var β :: s, hs => by simp [sVarSeq] at hs

-- If the θ-image of ofSpine s carries NO spine variable, every variable of s is
-- var-free under θ.
-- ⊢  vars(spine (θ(ofSpine s))) = []   ⟹   ∀ β ∈ vars(s). θβ var-free
theorem allVars_varFree_of {B : Type} {θ : TySubst B} :
    (s : List (Atom B)) →
    sVarSeq ((ofSpine s).applySubst θ).toSpine = [] →
    ∀ β ∈ sVarSeq s, (θ.row β).SpineVarFree
  | [], _, β, hβ => by simp [sVarSeq] at hβ
  | .field l τ :: s, h, β, hβ => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sVarSeq_append,
        sVarSeq, List.nil_append] at h
      exact allVars_varFree_of s h β (by simpa [sVarSeq] using hβ)
  | .var γ :: s, h, β, hβ => by
      simp only [ofSpine_var_cons, Row.applySubst, Row.toSpine, sVarSeq_append] at h
      obtain ⟨hγ, hs⟩ := List.append_eq_nil_iff.mp h
      simp only [sVarSeq] at hβ
      rcases List.mem_cons.mp hβ with rfl | hβ'
      · exact (spineVarFree_iff_varSeq_nil _).mpr hγ
      · exact allVars_varFree_of s hs β hβ'

-- If θ does not INCREASE the l-count of ofSpine s (it never decreases it), then
-- every variable of s is l-free under θ — the U-ground counting fact.
-- ⊢  count_l(spine (θ(ofSpine s))) = count_l(s)
--        ⟹   ∀ β ∈ vars(s). count_l(spine θβ) = 0
theorem allVars_lfree_of {B : Type} {θ : TySubst B} (l : Label) :
    (s : List (Atom B)) →
    sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s →
    ∀ β ∈ sVarSeq s, sFieldCount l (θ.row β).toSpine = 0
  | [], _, β, hβ => by simp [sVarSeq] at hβ
  | .field l' τ :: s, h, β, hβ => by
      simp only [ofSpine_field_cons, Row.applySubst, Row.toSpine, sFieldCount_append,
        sFieldCount] at h
      have h' : sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s := by omega
      exact allVars_lfree_of l s h' β (by simpa [sVarSeq] using hβ)
  | .var γ :: s, h, β, hβ => by
      simp only [ofSpine_var_cons, Row.applySubst, Row.toSpine, sFieldCount_append,
        sFieldCount] at h
      have hmono : sFieldCount l s ≤ sFieldCount l ((ofSpine s).applySubst θ).toSpine := by
        have hle := sFieldCount_applySubst_le θ l (ofSpine s); rwa [ofSpine_toSpine] at hle
      have hs : sFieldCount l ((ofSpine s).applySubst θ).toSpine = sFieldCount l s := by omega
      simp only [sVarSeq] at hβ
      rcases List.mem_cons.mp hβ with rfl | hβ'
      · omega
      · exact allVars_lfree_of l s hs β hβ'

-- ## U-ground: inversion + the reflection lemma
-- ⊢  groundMatchAux s₁ s₂ ls = some (τ,τ',t₁,t₂)   ⟹   ∃ l.
--       count_l(s₁) = count_l(s₂) ∧ 0 < count_l(s₁)
--       ∧ removeField l s₁ = some (τ,t₁) ∧ removeField l s₂ = some (τ',t₂)
theorem groundMatchAux_inv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} :
    (ls : List Label) → groundMatchAux s₁ s₂ ls = some (τ, τ', t₁, t₂) →
    ∃ l, sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ ∧
         removeField l s₁ = some (τ, t₁) ∧ removeField l s₂ = some (τ', t₂)
  | [], h => by simp [groundMatchAux] at h
  | l :: ls, h => by
      simp only [groundMatchAux] at h
      split at h
      · rename_i hcond
        cases hr₁ : removeField l s₁ with
        | none => simp only [hr₁] at h; exact groundMatchAux_inv ls h
        | some p₁ =>
          cases hr₂ : removeField l s₂ with
          | none => simp only [hr₁, hr₂] at h; exact groundMatchAux_inv ls h
          | some p₂ =>
            obtain ⟨τa, ta⟩ := p₁
            obtain ⟨τb, tb⟩ := p₂
            simp only [hr₁, hr₂, Option.some.injEq, Prod.mk.injEq] at h
            obtain ⟨rfl, rfl, rfl, rfl⟩ := h
            exact ⟨l, hcond.1, hcond.2, hr₁, hr₂⟩
      · exact groundMatchAux_inv ls h

-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   vars(s₂) = [] ∧ ∃ l.
--       count_l(s₁) = count_l(s₂) ∧ 0 < count_l(s₁)
--       ∧ removeField l s₁ = some (τ,t₁) ∧ removeField l s₂ = some (τ',t₂)
theorem groundMatch_inv {B : Type} {s₁ s₂ : List (Atom B)} {τ τ' : Ty B}
    {t₁ t₂ : List (Atom B)} :
    groundMatch s₁ s₂ = some (τ, τ', t₁, t₂) →
    sVarSeq s₂ = [] ∧ ∃ l, sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁ ∧
      removeField l s₁ = some (τ, t₁) ∧ removeField l s₂ = some (τ', t₂) := by
  intro h
  unfold groundMatch at h
  split at h
  · simp at h
  · rename_i hnv
    exact ⟨(sHasVar_false_iff s₂).mp (by simpa using hnv), groundMatchAux_inv (sLabels s₁) h⟩

-- U-ground reflection: the paired l-field bubbles out of BOTH sides. The ground
-- side (s₂ var-free) is a direct removeField_equiv_of. The other side needs its
-- variables to be l-free under θ — which the counting forces: hrec pins the
-- l-count of (ofSpine t₁)θ to that of the var-free (ofSpine t₂)θ, and
-- sFieldCount l s₁ = sFieldCount l s₂ then makes θ introduce zero l-fields
-- across s₁'s variables (allVars_lfree_of); likewise they stay var-free
-- (allVars_varFree_of). This is the one move whose soundness is genuinely
-- non-local — it reads the residual solution back through the counting.
-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂),  θτ ≈ θτ',  θ(ofSpine t₁) ≈ᵣ θ(ofSpine t₂)
--        ⟹   θ(ofSpine s₁) ≈ᵣ θ(ofSpine s₂)
theorem groundMatch_reflect {B : Type} {θ : TySubst B} {s₁ s₂ t₁ t₂ : List (Atom B)}
    {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂))
    (heq : TyEquiv (τ.applySubst θ) (τ'.applySubst θ))
    (hrec : RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ)) :
    RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ) := by
  obtain ⟨hs₂vars, l, hcount, _, hr₁, hr₂⟩ := groundMatch_inv hg
  have hs₂equiv : RowEquiv ((ofSpine s₂).applySubst θ)
      (.cat (.sing l (τ'.applySubst θ)) ((ofSpine t₂).applySubst θ)) :=
    removeField_equiv_of l s₂ hr₂ (fun β hβ => by simp [hs₂vars] at hβ)
  have ht₂vars : sVarSeq t₂ = [] := by rw [removeField_sVarSeq l s₂ hr₂]; exact hs₂vars
  have ht₂vf : (ofSpine t₂).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact ht₂vars)
  have hv₁nil : sVarSeq ((ofSpine t₁).applySubst θ).toSpine = [] := by
    rw [hrec.char.1]; exact varSeq_applySubst_nil θ t₂ ht₂vars
  have hfc : sFieldCount l ((ofSpine t₁).applySubst θ).toSpine = sFieldCount l t₁ := by
    have e1 := rowEquiv_fieldCount_eq l hrec
    have e2 : sFieldCount l ((ofSpine t₂).applySubst θ).toSpine = sFieldCount l t₂ := by
      rw [sFieldCount_applySubst_varFree θ l ht₂vf, ofSpine_toSpine]
    have c1 := removeField_sFieldCount l s₁ hr₁
    have c2 := removeField_sFieldCount l s₂ hr₂
    omega
  have hvarfree := allVars_varFree_of t₁ hv₁nil
  have hlfree := allVars_lfree_of l t₁ hfc
  have hvars₁ : sVarSeq t₁ = sVarSeq s₁ := removeField_sVarSeq l s₁ hr₁
  have hs₁equiv : RowEquiv ((ofSpine s₁).applySubst θ)
      (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) :=
    removeField_equiv_of l s₁ hr₁
      (fun β hβ => ⟨hvarfree β (by rw [hvars₁]; exact hβ),
                    hlfree β (by rw [hvars₁]; exact hβ)⟩)
  exact hs₁equiv.trans ((RowEquiv.cat (.sing heq) hrec).trans hs₂equiv.symm)

-- U-ground FORWARD: a unifier of the original ground-match pins the field types
-- and unifies the residual. The var-free + l-free side conditions on s₁ (which
-- the backward lemma reads off the residual) are here DERIVED from hu itself:
-- the ground side s₂ fixes count_l and the var sequence under θ, hu transports
-- both to s₁, and hcount forces θ to add no l-fields across s₁'s vars.
-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂),  θ ⊨ ofSpine s₁ ≐ᵣ ofSpine s₂
--        ⟹   θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂
theorem groundMatch_reflect_fwd {B : Type} {θ : TySubst B}
    {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂))
    (hu : RowEquiv ((ofSpine s₁).applySubst θ) ((ofSpine s₂).applySubst θ)) :
    TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
    RowEquiv ((ofSpine t₁).applySubst θ) ((ofSpine t₂).applySubst θ) := by
  obtain ⟨hs₂vars, l, hcount, _, hr₁, hr₂⟩ := groundMatch_inv hg
  have hs₂equiv : RowEquiv ((ofSpine s₂).applySubst θ)
      (.cat (.sing l (τ'.applySubst θ)) ((ofSpine t₂).applySubst θ)) :=
    removeField_equiv_of l s₂ hr₂ (fun β hβ => by simp [hs₂vars] at hβ)
  have hs₂vf : (ofSpine s₂).SpineVarFree :=
    (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact hs₂vars)
  have hv₁nil : sVarSeq ((ofSpine s₁).applySubst θ).toSpine = [] := by
    rw [hu.char.1]; exact varSeq_applySubst_nil θ s₂ hs₂vars
  have hfc : sFieldCount l ((ofSpine s₁).applySubst θ).toSpine = sFieldCount l s₁ := by
    have e1 := rowEquiv_fieldCount_eq l hu
    have e2 : sFieldCount l ((ofSpine s₂).applySubst θ).toSpine = sFieldCount l s₂ := by
      rw [sFieldCount_applySubst_varFree θ l hs₂vf, ofSpine_toSpine]
    omega
  have hvarfree := allVars_varFree_of s₁ hv₁nil
  have hlfree := allVars_lfree_of l s₁ hfc
  have hs₁equiv : RowEquiv ((ofSpine s₁).applySubst θ)
      (.cat (.sing l (τ.applySubst θ)) ((ofSpine t₁).applySubst θ)) :=
    removeField_equiv_of l s₁ hr₁ (fun β hβ => ⟨hvarfree β hβ, hlfree β hβ⟩)
  exact (hs₁equiv.symm.trans (hu.trans hs₂equiv)).field_cancel_left

-- ## Eq-emitting moves reflect no-mgu (the augmented-witness case of the lift)
-- Each of matchL/matchR/groundMatch replaces the row problem by "residual row
-- problem PLUS the emitted type equation τ ≐ τ'". reflect_fwd (forward) and
-- reflect (backward) together say a θ unifies the original IFF it unifies the
-- residual AND satisfies τ ≐ τ'. That is a pointwise predicate equivalence, so
-- `hasMguP_congr` transports mgu-status across the move. Thus a stuck residual
-- (whose eq-constrained problem has no mgu) forces the original to have no mgu —
-- the eq-emitting analogue of stripL/stripR_hasMgu_iff.
-- ⊢  matchL s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--        HasMguP (λθ. θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂))
theorem matchL_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hmatch : matchL s₁ s₂ = some (τ, τ', t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔
    HasMguP (fun θ => TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
                      Unifies θ (ofSpine t₁) (ofSpine t₂)) :=
  hasMguP_congr (fun _ =>
    ⟨fun hu => matchL_reflect_fwd hmatch hu,
     fun ⟨heq, hrec⟩ => matchL_reflect hmatch heq hrec⟩)

-- ⊢  matchR s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--        HasMguP (λθ. θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂))
theorem matchR_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hmatch : matchR s₁ s₂ = some (τ, τ', t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔
    HasMguP (fun θ => TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
                      Unifies θ (ofSpine t₁) (ofSpine t₂)) :=
  hasMguP_congr (fun _ =>
    ⟨fun hu => matchR_reflect_fwd hmatch hu,
     fun ⟨heq, hrec⟩ => matchR_reflect hmatch heq hrec⟩)

-- ⊢  groundMatch s₁ s₂ = some (τ,τ',t₁,t₂)   ⟹   (HasMgu (ofSpine s₁)(ofSpine s₂) ↔
--        HasMguP (λθ. θτ ≈ₜ θτ'  ∧  θ ⊨ ofSpine t₁ ≐ᵣ ofSpine t₂))
theorem groundMatch_hasMgu_iff {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {τ τ' : Ty B}
    (hg : groundMatch s₁ s₂ = some (τ, τ', t₁, t₂)) :
    HasMgu (ofSpine s₁) (ofSpine s₂) ↔
    HasMguP (fun θ => TyEquiv (τ.applySubst θ) (τ'.applySubst θ) ∧
                      Unifies θ (ofSpine t₁) (ofSpine t₂)) :=
  hasMguP_congr (fun _ =>
    ⟨fun hu => groundMatch_reflect_fwd hg hu,
     fun ⟨heq, hrec⟩ => groundMatch_reflect hg heq hrec⟩)

-- End-to-end demo that the eq-emitting arm composes (the match analogue of
-- wand_under_strip_no_mgu): prepend a shared ground field l:𝓪 to both sides of
-- the Wand core (β|α)≐ᵣ(l:𝓫). matchL fires on the leading l-fields, emitting the
-- (trivially satisfiable) eq 𝓪≐𝓪, and the residual is exactly Wand. matchL_hasMgu_iff
-- transports mgu-status to HasMguP of that eq-constrained residual; the eq being
-- 𝓪≐𝓪 the constraint is vacuous, so it collapses to HasMgu Wand — with no mgu.
-- (Here the accumulated eq is trivial; the genuine augmented-witness case is when
-- it constrains a shared field variable — same skeleton, witnesses extended to it.)
-- ⊢  ¬ HasMgu (l:𝓪 | β | α) (l:𝓪 | l:𝓫)
theorem wand_under_match_no_mgu {B : Type} (a b : B) :
    ¬ HasMgu (ofSpine [Atom.field "l" (.base a), .var "β", .var "α"])
             (ofSpine [Atom.field "l" (.base a), .field "l" (.base b)]) := by
  intro hmgu
  have hmatch : matchL ([Atom.field "l" (.base a), .var "β", .var "α"] : List (Atom B))
                       [Atom.field "l" (.base a), .field "l" (.base b)]
      = some (.base a, .base a, [.var "β", .var "α"], [.field "l" (.base b)]) := rfl
  rw [matchL_hasMgu_iff hmatch] at hmgu
  -- drop the vacuous eq conjunct: HasMguP (𝓪≐𝓪 ∧ ·) ⟹ HasMgu of the residual
  have hw : HasMgu (ofSpine [Atom.var "β", .var "α"]) (ofSpine [Atom.field "l" (.base b)]) := by
    obtain ⟨θ, ⟨_, hu⟩, hmax⟩ := hmgu
    exact ⟨θ, hu, fun θ' hu' => hmax θ' ⟨TyEquiv.refl _, hu'⟩⟩
  refine absurd hw ?_
  simp only [ofSpine]
  rw [hasMgu_rowEquiv (ρ₁' := .cat (.var "β") (.var "α")) (ρ₂' := .sing "l" (.base b))
        (RowEquiv.cat (RowEquiv.refl _) RowEquiv.unitR) RowEquiv.unitR]
  exact wand_no_mgu_count b "l"

------------------------- AGREEMENT: mgu MODULO FRESH NAMES -----------------
-- The vocabulary ≐ᵣ completeness needs once the algorithm can INVENT variables
--; consumed by P5's completeness and clash legs and by
-- the boundedness invariant they rest on.


theorem AgreeOn.refl {B : Type} (θ : TySubst B) (V : List TyVar) : AgreeOn θ θ V :=
  fun _ _ => ⟨rfl, rfl⟩

theorem AgreeOn.trans' {B : Type} {θ₁ θ₂ θ₃ : TySubst B} {V W : List TyVar}
    (h₁ : AgreeOn θ₁ θ₂ V) (h₂ : AgreeOn θ₂ θ₃ W) (hVW : V ⊆ W) : AgreeOn θ₁ θ₃ V :=
  fun α hα => ⟨(h₁ α hα).1.trans (h₂ α (hVW hα)).1,
               (h₁ α hα).2.trans (h₂ α (hVW hα)).2⟩

theorem AgreeOn.tyEq {B : Type} {θ θ' : TySubst B} {V : List TyVar}
    (h : AgreeOn θ θ' V) {τ : Ty B} (hsub : τ.ftv ⊆ V) :
    τ.applySubst θ = τ.applySubst θ' :=
  Ty.applySubst_congr τ (fun α hα => h α (hsub hα))

-- The problem's variables sit inside V; every residual's do too.
theorem sFtv_sub_left {B : Type} {s₁ s₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) : sFtv s₁ ⊆ V :=
  fun _ hx => hV (List.mem_append_left _ hx)

theorem sFtv_sub_right {B : Type} {s₁ s₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) : sFtv s₂ ⊆ V :=
  fun _ hx => hV (List.mem_append_right _ hx)

theorem sFtv_sub_residual {B : Type} {s₁ s₂ t₁ t₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) (h₁ : sFtv t₁ ⊆ sFtv s₁) (h₂ : sFtv t₂ ⊆ sFtv s₂) :
    (sFtv t₁ ++ sFtv t₂) ⊆ V := fun _ hx => by
  rcases List.mem_append.mp hx with hh | hh
  · exact sFtv_sub_left hV (h₁ hh)
  · exact sFtv_sub_right hV (h₂ hh)

theorem sFtv_sub_swap {B : Type} {s₁ s₂ : List (Atom B)} {V : List TyVar}
    (hV : (sFtv s₁ ++ sFtv s₂) ⊆ V) : (sFtv s₂ ++ sFtv s₁) ⊆ V := fun _ hx => by
  rcases List.mem_append.mp hx with hh | hh
  · exact sFtv_sub_right hV hh
  · exact sFtv_sub_left hV hh

------------------- ≐ᵣ TERMINAL STUCK-SHAPE STRUCTURE -----------------------
-- Structural facts about the terminal stuck config (every move dead, no
-- projClash). These pin down which shapes the base arm must handle: chiefly,
-- BOTH sides cannot be var-free — a genuinely stuck config always has a live
-- row-var somewhere, so it sits in the setting of the three base-witness
-- techniques (count-shrink / rigidity / non-commutativity). This is the
-- de-risking characterization the base arm dispatches on.

-- removeField finds an l-field whenever the l-count is positive.
theorem removeField_isSome_of_pos {B : Type} (l : Label) :
    (s : List (Atom B)) → 0 < sFieldCount l s → (removeField l s).isSome = true
  | [], h => by simp [sFieldCount] at h
  | .var β :: s, h => by
      simp only [sFieldCount] at h
      simp only [removeField]
      have ih := removeField_isSome_of_pos l s h
      cases hr : removeField l s with
      | none => rw [hr] at ih; simp at ih
      | some p => simp
  | .field l' τ :: s, h => by
      simp only [removeField]
      by_cases hl : l' = l
      · subst hl; simp
      · rw [if_neg hl]
        simp only [sFieldCount, if_neg hl, Nat.zero_add] at h
        have ih := removeField_isSome_of_pos l s h
        cases hr : removeField l s with
        | none => rw [hr] at ih; simp at ih
        | some p => simp

-- groundMatchAux none ⟹ no scanned label has equal positive counts on both sides
-- (equal positive counts would let removeField fire on both, yielding `some`).
theorem groundMatchAux_none_of_mem {B : Type} {s₁ s₂ : List (Atom B)} :
    (ls : List Label) → groundMatchAux s₁ s₂ ls = none →
    ∀ l ∈ ls, ¬ (sFieldCount l s₁ = sFieldCount l s₂ ∧ 0 < sFieldCount l s₁)
  | [], _, l, hmem, _ => by simp at hmem
  | c :: ls, hnone, l, hmem, hcond' => by
      simp only [groundMatchAux] at hnone
      by_cases hcond : sFieldCount c s₁ = sFieldCount c s₂ ∧ 0 < sFieldCount c s₁
      · rw [if_pos hcond] at hnone
        have h1 := removeField_isSome_of_pos c s₁ hcond.2
        have h2 := removeField_isSome_of_pos c s₂ (hcond.1 ▸ hcond.2)
        cases hr1 : removeField c s₁ with
        | none => rw [hr1] at h1; simp at h1
        | some p1 =>
          cases hr2 : removeField c s₂ with
          | none => rw [hr2] at h2; simp at h2
          | some p2 =>
            obtain ⟨t1, r1⟩ := p1; obtain ⟨t2, r2⟩ := p2
            rw [hr1, hr2] at hnone; simp at hnone
      · rw [if_neg hcond] at hnone
        rcases List.mem_cons.mp hmem with rfl | htail
        · exact hcond hcond'
        · exact groundMatchAux_none_of_mem ls hnone l htail hcond'

-- projClash false, both sides var-free ⟹ every scanned label has EQUAL counts.
theorem projClash_false_count_eq {B : Type} {s₁ s₂ : List (Atom B)}
    (hpc : projClash s₁ s₂ = false) (hv₁ : sHasVar s₁ = false) (hv₂ : sHasVar s₂ = false)
    (l : Label) (hmem : l ∈ sLabels s₁ ++ sLabels s₂) :
    sFieldCount l s₁ = sFieldCount l s₂ := by
  have hpc' : (sLabels s₁ ++ sLabels s₂).any (fun l =>
      (decide (sFieldCount l s₂ < sFieldCount l s₁) && !sHasVar s₂) ||
      (decide (sFieldCount l s₁ < sFieldCount l s₂) && !sHasVar s₁)) = false := hpc
  have hkey := List.any_eq_false.mp hpc' l hmem
  rw [hv₁, hv₂] at hkey
  simp only [Bool.not_false, Bool.and_true, Bool.or_eq_true, decide_eq_true_eq, not_or] at hkey
  omega

-- The de-risking fact: a terminal stuck config is NEVER ground on both sides.
-- (If it were, projClash-false pins all counts equal, and the leading field's
-- label then satisfies groundMatch's fire condition — contradicting groundMatch
-- = none.) So the base arm always has a live var to run a witness technique on.
-- ⊢  groundMatch (a::s₁) s₂ = none,  projClash (a::s₁) s₂ = false,
--      sHasVar (a::s₁) = false,  sHasVar s₂ = false   ⟹   False
theorem stuck_not_both_ground {B : Type} {a : Atom B} {s₁ s₂ : List (Atom B)}
    (hg : groundMatch (a :: s₁) s₂ = none)
    (hpc : projClash (a :: s₁) s₂ = false)
    (hv₁ : sHasVar (a :: s₁) = false) (hv₂ : sHasVar s₂ = false) : False := by
  cases a with
  | var α => simp [sHasVar] at hv₁
  | field l₀ τ =>
    have hpos : 0 < sFieldCount l₀ (Atom.field l₀ τ :: s₁) := by simp [sFieldCount]; omega
    have hmem₁ : l₀ ∈ sLabels (Atom.field l₀ τ :: s₁) := by simp [sLabels]
    have hcount : sFieldCount l₀ (Atom.field l₀ τ :: s₁) = sFieldCount l₀ s₂ :=
      projClash_false_count_eq hpc hv₁ hv₂ l₀ (List.mem_append_left _ hmem₁)
    have hgaux : groundMatchAux (Atom.field l₀ τ :: s₁) s₂ (sLabels (Atom.field l₀ τ :: s₁)) = none := by
      rw [groundMatch, hv₂] at hg; simpa using hg
    exact groundMatchAux_none_of_mem (sLabels (Atom.field l₀ τ :: s₁)) hgaux l₀ hmem₁ ⟨hcount, hpos⟩

-- Leading-atom characterization of a terminal stuck config. With stripL and
-- both matchL directions dead, the two leading atoms can only take one of four
-- shapes — none of which a forced move can act on. This is step 1 of the
-- base-arm DISPATCH the trichotomy still owes: each shape routes to a
-- base-witness technique (distinct leading vars → non-commutativity /
-- allvar_swap; a leading field facing a var, either way → count-shrink /
-- rigidity; two distinct leading fields each absent from the other's window →
-- count-shrink on the mismatched label). The last shape carries the two
-- windowExtract-failures the dispatch needs to locate the offending field.
-- ⊢  stripL/matchL(both) dead   ⟹   the leading atoms are one of the four shapes
theorem stuck_leading_shape {B : Type} {a b : Atom B} {s₁ s₂ : List (Atom B)}
    (hsl : stripL (a :: s₁) (b :: s₂) = none)
    (hml : matchL (a :: s₁) (b :: s₂) = none)
    (hml2 : matchL (b :: s₂) (a :: s₁) = none) :
    (∃ α β, a = .var α ∧ b = .var β ∧ α ≠ β) ∨
    (∃ α l' τ', a = .var α ∧ b = .field l' τ') ∨
    (∃ l τ β, a = .field l τ ∧ b = .var β) ∨
    (∃ l τ l' τ', a = .field l τ ∧ b = .field l' τ' ∧ l ≠ l' ∧
      windowExtract l (b :: s₂) = none ∧ windowExtract l' (a :: s₁) = none) := by
  cases a with
  | var α =>
    cases b with
    | var β =>
      -- stripL would fire on equal leading vars, so α ≠ β.
      refine Or.inl ⟨α, β, rfl, rfl, ?_⟩
      intro h; subst h; simp [stripL] at hsl
    | field l' τ' => exact Or.inr (Or.inl ⟨α, l', τ', rfl, rfl⟩)
  | field l τ =>
    cases b with
    | var β => exact Or.inr (Or.inr (Or.inl ⟨l, τ, β, rfl, rfl⟩))
    | field l' τ' =>
      refine Or.inr (Or.inr (Or.inr ⟨l, τ, l', τ', rfl, rfl, ?_, ?_, ?_⟩))
      -- matchL of a leading field fires exactly when the label is in the
      -- other side's window, so both windowExtracts must be none.
      · -- l ≠ l' : equal labels would let windowExtract fire at the head.
        intro h; subst h
        simp [matchL, windowExtract] at hml
      · simp only [matchL] at hml
        cases hwe : windowExtract l (Atom.field l' τ' :: s₂) with
        | none => rfl
        | some p => obtain ⟨t, r⟩ := p; rw [hwe] at hml; simp at hml
      · simp only [matchL] at hml2
        cases hwe : windowExtract l' (Atom.field l τ :: s₁) with
        | none => rfl
        | some p => obtain ⟨t, r⟩ := p; rw [hwe] at hml2; simp at hml2


end MinimalCalculus
