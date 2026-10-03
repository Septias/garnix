-- THE A-let CASE, AND WITH IT `InferSound`.
--
-- qLet asks for three things, and each has its own argument:
--
--   * the BODY — e₂'s induction hypothesis at the same σ, in Γ′ extended by the
--     let scheme READ under σ (binders renamed fresh, `SchemeRead`);
--   * the INSTANCES — for every instance χ of the read scheme, e₁ at the body
--     at χ, assuming the constraints at χ. This is generalization: e₁'s
--     induction hypothesis at σ₁ = (χ ∘ readSub) ∘ ⟦S₁⟧, which is the instance on
--     ᾱ and agrees with σ everywhere Γ and Δ_Γ can see — exactly, because σ
--     ABSORBS ⟦S₁⟧ and the binder names are fresh for σ's reach;
--   * INHABITATION — one instance whose constraints discharge exactly: every
--     generalized stump is blocked on a generalized row variable, so with the
--     binders left in place each lookup is `?`, and sending every result
--     variable to ★ is D-? for all of them at once.

import InferSoundA

namespace MinimalCalculus

variable {B : Type} [DecidableEq B]

--------------------- BLOCKED LOOKUPS SURVIVE A SUBSTITUTION --------------------
-- A definite absence survives any substitution (`lookup_applySubst`), and a
-- lookup blocked on β stays `?` as long as β is sent to a variable. The blocker
-- of a row-blocked lookup is a row variable ON THE SPINE of the row looked up,
-- which is what callers use to show it is sent to a variable.
--
-- (This section used to carry `NoChase Γ ρ` — "no row variable of ρ has a
-- solution in Γ" — on every lemma, to rule out the L-α chase. There is no chase
-- any more, so there is nothing to rule out.)

-- A row blocker must be sent to a row variable. A KEY blocker (a label variable)
-- must be sent to a label variable that no other label variable of the lookup is
-- also sent to: only an undecided comparison stays undecided.

/-- β is sent to a label variable β′ that no other label variable of `xs` reaches. -/
def KeyFresh (θ : TySubst B) (xs : List (Srt × TyVar)) (β : TyVar) : Prop :=
  ∃ β', θ.lab β = .var β' ∧ ∀ γ, (.lab, γ) ∈ xs → θ.lab γ = .var β' → γ = β

theorem KeyFresh.mono {θ : TySubst B} {xs ys : List (Srt × TyVar)} {β : TyVar}
    (h : KeyFresh θ xs β) (hs : ∀ x ∈ ys, x ∈ xs) : KeyFresh θ ys β :=
  let ⟨β', hβ, hf⟩ := h
  ⟨β', hβ, fun γ hγ he => hf γ (hs _ hγ) he⟩

private theorem mem_q_cat_r {x : Srt × TyVar} {a b c : List (Srt × TyVar)}
    (h : x ∈ a ++ c) : x ∈ a ++ (b ++ c) := by
  rcases List.mem_append.mp h with h | h
  · exact List.mem_append_left _ h
  · exact List.mem_append_right _ (List.mem_append_right _ h)

private theorem mem_q_cat_l {x : Srt × TyVar} {a b c : List (Srt × TyVar)}
    (h : x ∈ a ++ b) : x ∈ a ++ (b ++ c) := by
  rcases List.mem_append.mp h with h | h
  · exact List.mem_append_left _ h
  · exact List.mem_append_right _ (List.mem_append_left _ h)

-- ⊢  a blocked lookup stays `?` under a substitution that leaves the blocker a
--    variable (and, at a key, a label variable of its own)
theorem lookupQ_blocked_subst {ρ : Row B} {q : Key} {β : TyVar}
    (h : LookupBlockedQ ρ q β) (θ : TySubst B)
    (hrow : (.row, β) ∈ ρ.sortedFtv → ∃ β', θ.row β = .var β')
    (hkey : (.lab, β) ∈ Key.sortedFtv q ++ ρ.sortedFtv →
      KeyFresh θ (Key.sortedFtv q ++ ρ.sortedFtv) β) :
    LookupQ (ρ.applySubst θ) (q.applySubst θ) .unknown := by
  revert hrow hkey
  induction h with
  | varFree =>
      intro hrow _
      obtain ⟨β', hβ'⟩ := hrow (by simp [Row.sortedFtv])
      show LookupQ (θ.row _) _ _
      rw [hβ']; exact .varFree
  | @sunk α l τ =>
      intro _ hkey
      obtain ⟨β', hβ', -⟩ := hkey (by simp [Key.sortedFtv])
      show LookupQ (.sing l _) (θ.lab α) _
      rw [hβ']; exact .sunk rfl
  | @dunkQ α q' τ hcmp =>
      intro _ hkey
      obtain ⟨β', hβ', hf⟩ := hkey (by simp [Key.sortedFtv])
      show LookupQ (.dsing _ _) (θ.lab α) _
      rw [hβ']
      refine .dunk (Key.cmp_var_ne ?_)
      cases q' with
      | var γ =>
          intro he
          have hγ := hf γ (by simp [Key.sortedFtv, Row.sortedFtv]) he
          subst hγ; simp at hcmp
      | lit _ => intro he; cases he
  | @dunkF l τ γ =>
      intro _ hkey
      obtain ⟨β', hβ', -⟩ := hkey (by simp [Key.sortedFtv, Row.sortedFtv])
      show LookupQ (.dsing (θ.lab γ) _) _ _
      rw [hβ']
      exact .dunk rfl
  | catSkip ha _ ih =>
      intro hrow hkey
      have ha' := LookupQ.applySubst θ ha (by intro h; cases h)
      exact .catSkip ha' (ih (fun hm => hrow (List.mem_append_right _ hm))
        (fun hm => (hkey (mem_q_cat_r hm)).mono (fun _ hx => mem_q_cat_r hx)))
  | catUnk _ ih =>
      intro hrow hkey
      exact .catUnk (ih (fun hm => hrow (List.mem_append_left _ hm))
        (fun hm => (hkey (mem_q_cat_l hm)).mono (fun _ hx => mem_q_cat_l hx)))

-- ⊢  a clean state leaves every spine variable of a row it has substituted alone
theorem SolverState.row_var_of_clean {S : SolverState B} (hc : S.sol.Clean) {ρ : Row B}
    {α : TyVar} (hα : (.row, α) ∈ (ρ.applySubst S.subst).sortedFtv) :
    S.subst.row α = .var α := by
  apply rowLookup_not_mem
  intro hm
  obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hm
  exact hc.clears_row hα (Sol.mem_domS_row (List.mem_map_of_mem hp))

--------------------- RENAMING A RESULT ---------------------------------------
-- A-let reads each generalized result under `readSub`, which renames the
-- binders. A renaming keeps a result's shape: its variables are renamed, and it
-- stays a pattern, and a variable or not, exactly as it was.

theorem Key.ftv_rename {θ : TySubst B} {g : TyVar → TyVar} : (k : Key) →
    (∀ α ∈ k.ftv, θ.lab α = .var (g α)) → (k.applySubst θ).ftv = k.ftv.map g
  | .lit _, _ => rfl
  | .var α, h => by simp [(h α (by simp [Key.ftv])), Key.ftv]

mutual
  theorem Ty.ftv_rename {θ : TySubst B} {g : TyVar → TyVar} : (τ : Ty B) →
      (∀ α ∈ τ.ftv, θ.ty α = .var (g α) ∧ θ.row α = .var (g α) ∧ θ.lab α = .var (g α)) →
      (τ.applySubst θ).ftv = τ.ftv.map g
    | .var α, h => by simp [Ty.applySubst, (h α (by simp [Ty.ftv])).1, Ty.ftv]
    | .base _, _ => rfl
    | .lab k, h => by
        simp only [Ty.applySubst, Ty.ftv]
        exact Key.ftv_rename k (fun α hα => (h α (by simpa [Ty.ftv] using hα)).2.2)
    | .unk, _ => rfl
    | .fn a b, h => by
        simp only [Ty.applySubst, Ty.ftv, List.map_append]
        rw [Ty.ftv_rename a (fun α hα => h α (List.mem_append_left _ hα)),
          Ty.ftv_rename b (fun α hα => h α (List.mem_append_right _ hα))]
    | .rcd ρ, h => by
        simp only [Ty.applySubst, Ty.ftv]; exact Row.ftv_rename ρ h
  theorem Row.ftv_rename {θ : TySubst B} {g : TyVar → TyVar} : (ρ : Row B) →
      (∀ α ∈ ρ.ftv, θ.ty α = .var (g α) ∧ θ.row α = .var (g α) ∧ θ.lab α = .var (g α)) →
      (ρ.applySubst θ).ftv = ρ.ftv.map g
    | .empty, _ => rfl
    | .var α, h => by simp [Row.applySubst, (h α (by simp [Row.ftv])).2.1, Row.ftv, Ty.ftv]
    | .sing _ τ, h => by
        simp only [Row.applySubst, Row.ftv]; exact Ty.ftv_rename τ h
    | .cat a b, h => by
        simp only [Row.applySubst, Row.ftv, List.map_append]
        rw [Row.ftv_rename a (fun α hα => h α (List.mem_append_left _ hα)),
          Row.ftv_rename b (fun α hα => h α (List.mem_append_right _ hα))]
    | .dsing q τ, h => by
        simp only [Row.applySubst, Row.ftv, List.map_append]
        rw [Key.ftv_rename q (fun α hα => (h α (List.mem_append_left _ hα)).2.2),
          Ty.ftv_rename τ (fun α hα => h α (List.mem_append_right _ hα))]
end

theorem Ty.isVar_rename {θ : TySubst B} {g : TyVar → TyVar} (τ : Ty B)
    (h : ∀ α ∈ τ.ftv, θ.ty α = .var (g α) ∧ θ.row α = .var (g α) ∧ θ.lab α = .var (g α)) :
    (τ.applySubst θ).isVar = τ.isVar := by
  cases τ with
  | var α => simp [Ty.applySubst, (h α (by simp [Ty.ftv])).1, Ty.isVar]
  | _ => rfl

--------------------- FILLING A BLOCKER ----------------------------------------
-- The witness that a let scheme has an instance meets each SPENT constraint by
-- extending its blocker with the field — materialization, inside a proof.

/-- the fields `fs`, in order, then the variable γ. -/
def fillRow (fs : List (Label × Ty B)) (γ : TyVar) : Row B :=
  fs.foldr (fun lt ρ => .cat (.sing lt.1 lt.2) ρ) (.var γ)

/-- what a lookup of l finds in `fillRow fs γ`. -/
def fillRes (fs : List (Label × Ty B)) (l : Label) : LookupRes B :=
  match fs.lookup l with
  | some τ => .found τ
  | none   => .unknown

theorem lookup_fillRow (γ : TyVar) (l : Label) :
    ∀ fs : List (Label × Ty B), Lookup (fillRow fs γ) l (fillRes fs l)
  | [] => .varFree
  | (l', τ) :: fs => by
      show Lookup (.cat (.sing l' τ) (fillRow fs γ)) l _
      by_cases he : l = l'
      · subst he
        simp only [fillRes, List.lookup, beq_self_eq_true]
        exact .catHit .hit
      · have hb : (l == l') = false := by simp [he]
        have hr : fillRes ((l', τ) :: fs) l = fillRes fs l := by
          simp only [fillRes, List.lookup, hb]
        rw [hr]
        exact .catSkip (.miss (Ne.symm he)) (lookup_fillRow γ l fs)

/-- ⊢  a lookup blocked on β, with β filled: it finds what the fill has for l,
and is blocked again otherwise. -/
theorem fillRes_ne_absent (fs : List (Label × Ty B)) (l : Label) :
    fillRes fs l ≠ .absent := by
  unfold fillRes; split <;> simp

theorem lookup_cat_left {ρ₁ ρ₂ : Row B} {l : Label} {r : LookupRes B}
    (h : Lookup ρ₁ l r) (hr : r ≠ .absent) : Lookup (.cat ρ₁ ρ₂) l r := by
  cases r with
  | found τ => exact .catHit h
  | unknown => exact .catUnk h
  | absent => exact absurd rfl hr

theorem lookup_blocked_fill {ρ : Row B} {l : Label} {β : TyVar}
    (h : LookupBlocked ρ l β) (θ : TySubst B) {fs : List (Label × Ty B)} {γ : TyVar}
    (hβ : θ.row β = fillRow fs γ) (hk : (.lab, β) ∉ ρ.sortedFtv) :
    Lookup (ρ.applySubst θ) l (fillRes fs l) := by
  induction h with
  | varFree =>
      show Lookup (θ.row _) _ _
      rw [hβ]; exact lookup_fillRow γ _ fs
  | catSkip ha _ ih =>
      exact .catSkip (lookup_applySubst (r := .absent) θ ha (by intro h; cases h))
        (ih hβ (fun hm => hk (List.mem_append_right _ hm)))
  | catUnk _ ih =>
      exact lookup_cat_left (ih hβ (fun hm => hk (List.mem_append_left _ hm)))
        (fillRes_ne_absent _ _)
  -- a key blocker is a label variable of the row: excluded
  | dunk => exact absurd (by simp [Row.sortedFtv, Key.sortedFtv]) hk

theorem lookup_some_mem {l : Label} {τ : Ty B} :
    ∀ {fs : List (Label × Ty B)}, fs.lookup l = some τ → (l, τ) ∈ fs
  | [], h => by simp [List.lookup] at h
  | (l', τ') :: fs, h => by
      by_cases he : l = l'
      · subst he; simp only [List.lookup, beq_self_eq_true, Option.some.injEq] at h
        subst h; exact List.mem_cons_self
      · have hb : (l == l') = false := by simp [he]
        simp only [List.lookup, hb] at h
        exact List.mem_cons_of_mem _ (lookup_some_mem h)

theorem lookup_none_of_mem {l : Label} {τ : Ty B} :
    ∀ {fs : List (Label × Ty B)}, (l, τ) ∈ fs → fs.lookup l ≠ none
  | [], h => absurd h List.not_mem_nil
  | (l', τ') :: fs, h => by
      by_cases he : l = l'
      · subst he; simp [List.lookup]
      · have hb : (l == l') = false := by simp [he]
        simp only [List.lookup, hb]
        rcases List.mem_cons.mp h with h | h
        · exact absurd (congrArg Prod.fst h) he
        · exact lookup_none_of_mem h

theorem Ty.exists_of_isLab {τ : Ty B} (h : τ.isLab = true) : ∃ k, τ = .lab k := by
  cases τ <;> simp_all [Ty.isLab]

theorem Ty.exists_of_isVar {τ : Ty B} (h : τ.isVar = true) : ∃ δ, τ = .var δ := by
  cases τ <;> simp_all [Ty.isVar]

--------------------- THE CASE -------------------------------------------------

/-- ⊢  **the A-let case.** -/
theorem letCase {C : Type} {constTy : C → B} : LetCase B C constTy := by
  intro Γ S S₁ S₂ x e₁ e₂ τ₁ τ₂ ᾱ h₁ hA h₂ hΓ hc hq IH₁ IH₂ σ hab hσ Γ' hr
  -- Δ_q, Δ_Γ and the conditions on ᾱ, in the form the proof reads them
  generalize hQ : letQ S₁ ᾱ = Δq at h₂ IH₂
  generalize hG : letG S₁ ᾱ = Δγ at h₂ IH₂
  have hsplit : S₁.parked.Perm (Δq ++ Δγ) := hQ ▸ hG ▸ letSplit S₁ ᾱ
  have hbq : ∀ p ∈ Δq, p.blocker ∈ ᾱ := fun p hp => (mem_letQ.mp (hQ ▸ hp)).2
  have hfresh := hA.gfresh
  have hres : LetResults S₁ ᾱ Δq := hQ ▸ hA.results
  have hdis : ∀ α ∈ ᾱ, ∀ p ∈ Δγ, _ := fun α hα p hp => hA.dis α hα p (hG ▸ hp)
  -- the state e₁ ends in
  have c₁ := Infer.clean h₁ hc
  have q₁ := Infer.quiescent h₁ hq
  have k₂ := Infer.keeps h₂
  have x₁₂ : S₁.Ext S₂ := (SolverState.Ext.of_sol_eq (S := S₁)
    (S' := { S₁ with parked := Δγ }) rfl).trans (Infer.ext h₂)
  have hab₁ : Absorbs σ S₁ := hab.back x₁₂ c₁
  have hΔq : ∀ p ∈ Δq, p ∈ S₁.parked := fun p hp => by
    exact hsplit.mem_iff.mpr <| List.mem_append_left _ hp
  have hΔγ : ∀ p ∈ Δγ, p ∈ S₁.parked := fun p hp => by
    exact hsplit.mem_iff.mpr <| List.mem_append_right _ hp
  -- fresh binder names, avoiding everything σ can reach from what stays outside
  let sc := letScheme S₁ ᾱ Δq τ₁
  let LΓ : List TyVar := Γ.ftv.flatMap (fun β =>
    (S₁.subst.ty β).ftv ++ (S₁.subst.row β).ftv ++ (S₁.subst.lab β).ftv)
  let LΔ : List TyVar := Δγ.flatMap (fun p =>
    (p.stump.row.applySubst S₁.subst).ftv ++ (p.stump.res.applySubst S₁.subst).ftv ++
      (p.stump.label.applySubst S₁.subst).ftv)
  obtain ⟨f, hinj, -, havL⟩ := fresh_renaming_exists σ ᾱ (sc.freeFtv ++ LΓ ++ LΔ)
  let sc' := sc.readAt σ f
  have hread : SchemeRead σ sc sc' :=
    ⟨f, hinj, fun α hα _ => havL α (List.mem_append_left _ (List.mem_append_left _ hα)), rfl⟩
  -- a generalized result is read by renaming its variables
  have hrd : ∀ p ∈ Δq, ∀ α ∈ (p.stump.res.applySubst S₁.subst).ftv,
      (readSub σ ᾱ f).ty α = .var (f α) ∧ (readSub σ ᾱ f).row α = .var (f α) ∧
        (readSub σ ᾱ f).lab α = .var (f α) :=
    fun p hp α hα => by simp [readSub, hres.1 p hp α hα]
  have hrdftv : ∀ p ∈ Δq, ((p.stump.res.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).ftv
      = (p.stump.res.applySubst S₁.subst).ftv.map f :=
    fun p hp => Ty.ftv_rename _ (hrd p hp)
  refine .qLet (σ := sc') ?_ ?_ ?_ (IH₂ σ hab hσ _ (hr.bindScheme x hread))
  · ------------------------------------------------ the read scheme is well-formed
    intro st hst δ hδ
    obtain ⟨st₀, hst₀, rfl⟩ := List.mem_map.mp hst
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst₀
    change δ ∈ ((p.stump.res.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).ftv at hδ
    rw [hrdftv p hp] at hδ
    obtain ⟨α, hα, rfl⟩ := List.mem_map.mp hδ
    exact List.mem_map_of_mem (hres.1 p hp α hα)
  · ------------------------------------------------ the instances
    intro χ hχ
    let ρ : TySubst B := χ.comp (readSub σ ᾱ f)
    let σ₁ : TySubst B := ρ.comp S₁.subst
    -- ρ is σ wherever the outside can see
    have hρ : ∀ β ∈ sc.freeFtv ++ LΓ ++ LΔ, β ∉ ᾱ →
        ρ.ty β = σ.ty β ∧ ρ.row β = σ.row β ∧ ρ.lab β = σ.lab β := by
      intro β hβ hn
      obtain ⟨hvt, hvr, hvl⟩ := havL β hβ
      have fx : ∀ γ, γ ∉ ᾱ.map f → χ.ty γ = .var γ ∧ χ.row γ = .var γ ∧ χ.lab γ = .var γ :=
        fun γ h => ⟨hχ.1 γ h, hχ.2.1 γ h, hχ.2.2 γ h⟩
      refine ⟨?_, ?_, ?_⟩
      · show ((readSub σ ᾱ f).ty β).applySubst χ = _
        simp only [readSub, if_neg hn]
        exact Ty.applySubst_fixed_ftv _ (fun γ hγ => fx γ (hvt γ hγ))
      · show ((readSub σ ᾱ f).row β).applySubst χ = _
        simp only [readSub, if_neg hn]
        exact Row.applySubst_fixed_ftv _ (fun γ hγ => fx γ (hvr γ hγ))
      · show ((readSub σ ᾱ f).lab β).applySubst χ = _
        simp only [readSub, if_neg hn]
        cases hk : σ.lab β with
        | lit _ => rfl
        | var γ => exact (fx γ (hvl γ (by simp [hk, Key.ftv]))).2.2
    -- so σ₁ is σ on Γ's variables …
    have hΓag : ∀ β ∈ Γ.ftv, σ₁.ty β = σ.ty β ∧ σ₁.row β = σ.row β ∧ σ₁.lab β = σ.lab β := by
      intro β hβ
      refine ⟨?_, ?_, ?_⟩
      · show (S₁.subst.ty β).applySubst ρ = _
        rw [← (hab₁ β).1]
        exact Ty.applySubst_congr _ (fun γ hγ => hρ γ
          (List.mem_append_left _ (List.mem_append_right _
            (List.mem_flatMap.mpr ⟨β, hβ, List.mem_append_left _ (List.mem_append_left _ hγ)⟩)))
          (fun hm => (hfresh γ hm β hβ).1 hγ))
      · show (S₁.subst.row β).applySubst ρ = _
        rw [← (hab₁ β).2.1]
        exact Row.applySubst_congr _ (fun γ hγ => hρ γ
          (List.mem_append_left _ (List.mem_append_right _
            (List.mem_flatMap.mpr ⟨β, hβ, List.mem_append_left _ (List.mem_append_right _ hγ)⟩)))
          (fun hm => (hfresh γ hm β hβ).2.1 hγ))
      · show (S₁.subst.lab β).applySubst ρ = _
        rw [← (hab₁ β).2.2]
        exact Key.applySubst_congr _ (fun γ hγ => (hρ γ
          (List.mem_append_left _ (List.mem_append_right _
            (List.mem_flatMap.mpr ⟨β, hβ, List.mem_append_right _ hγ⟩)))
          (fun hm => (hfresh γ hm β hβ).2.2 hγ)).2.2)
    -- … and σ₁ absorbs S₁
    have hab₁' : Absorbs σ₁ S₁ := by
      intro α
      obtain ⟨hi₁, hi₂, hi₃⟩ := SolverState.idem c₁ α
      refine ⟨?_, ?_, ?_⟩
      · show (S₁.subst.ty α).applySubst (ρ.comp S₁.subst) = (S₁.subst.ty α).applySubst ρ
        rw [← Ty.applySubst_applySubst, hi₁]
      · show (S₁.subst.row α).applySubst (ρ.comp S₁.subst) = (S₁.subst.row α).applySubst ρ
        rw [← Row.applySubst_applySubst, hi₂]
      · show (S₁.subst.lab α).applySubst (ρ.comp S₁.subst) = (S₁.subst.lab α).applySubst ρ
        rw [← Key.applySubst_applySubst, hi₃]
    have hr₁ : CtxRead σ₁ Γ Γ' := ⟨fun y sc₀ hy => by
      obtain ⟨sc₀', hl', hs⟩ := hr.schem y sc₀ hy
      refine ⟨sc₀', hl', hs.congr (hΓ y sc₀ hy) (fun α hα _ => hΓag α ?_)⟩
      refine QCtx.lookup_ftv_subset hy α ?_
      unfold QScheme.freeFtv at hα; unfold QScheme.ftv
      rcases List.mem_append.mp hα with hα | hα
      · obtain ⟨st, hst, hα⟩ := List.mem_flatMap.mp hα
        exact List.mem_append_left _ (List.mem_append_right _
          (List.mem_flatMap.mpr ⟨st, hst, List.mem_append_right _ hα⟩))
      · exact List.mem_append_right _ hα⟩
    have ih := IH₁ σ₁ hab₁' (hab₁'.sat c₁) Γ' hr₁
    -- the type e₁ gets is the read body at χ
    have hty : τ₁.applySubst σ₁ = sc'.body.applySubst χ := by
      show τ₁.applySubst (ρ.comp S₁.subst)
        = ((τ₁.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
      rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]
    rw [hty] at ih
    refine ih.weaken (fun a ha => ?_)
    obtain ⟨p, hp, rfl⟩ := List.mem_map.mp ha
    replace hp := hsplit.mem_iff.mp hp
    rcases List.mem_append.mp hp with hp | hp
    · -- a generalized stump: it IS the read constraint at χ
      refine .inl (List.mem_append_left _ (List.mem_map.mpr ⟨_, List.mem_map.mpr
        ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, rfl⟩, ?_⟩))
      simp only [Stump.at]
      congr 1
      · show ((p.stump.row.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
          = p.stump.row.applySubst (ρ.comp S₁.subst)
        rw [Row.applySubst_applySubst, Row.applySubst_applySubst]
      · show ((p.stump.label.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
          = p.stump.label.applySubst (ρ.comp S₁.subst)
        rw [Key.applySubst_applySubst, Key.applySubst_applySubst]
      · show ((p.stump.res.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ
          = p.stump.res.applySubst (ρ.comp S₁.subst)
        rw [Ty.applySubst_applySubst, Ty.applySubst_applySubst]
    · -- a stump that stays: its σ₁-reading is its σ-reading, and it is kept
      have hsame : p.stump.at σ₁ = p.stump.at σ := by
        have hLΔ : ∀ γ, γ ∈ (p.stump.row.applySubst S₁.subst).ftv ++
            (p.stump.res.applySubst S₁.subst).ftv ++ (p.stump.label.applySubst S₁.subst).ftv →
            γ ∈ sc.freeFtv ++ LΓ ++ LΔ :=
          fun γ hγ => List.mem_append_right _ (List.mem_flatMap.mpr ⟨p, hp, hγ⟩)
        simp only [Stump.at]
        congr 1
        · show p.stump.row.applySubst (ρ.comp S₁.subst) = _
          rw [← Row.applySubst_applySubst, ← hab₁.row]
          exact Row.applySubst_congr _ (fun γ hγ =>
            hρ γ (hLΔ γ (List.mem_append_left _ (List.mem_append_left _ hγ)))
            (fun hm => (hdis γ hm p hp).1 hγ))
        · show p.stump.label.applySubst (ρ.comp S₁.subst) = _
          rw [← Key.applySubst_applySubst, ← hab₁.key]
          exact Key.applySubst_congr _ (fun γ hγ =>
            (hρ γ (hLΔ γ (List.mem_append_right _ hγ))
            (fun hm => (hdis γ hm p hp).2.2 hγ)).2.2)
        · show p.stump.res.applySubst (ρ.comp S₁.subst) = _
          rw [← Ty.applySubst_applySubst, ← hab₁.ty]
          exact Ty.applySubst_congr _ (fun γ hγ =>
            hρ γ (hLΔ γ (List.mem_append_left _ (List.mem_append_right _ hγ)))
            (fun hm => (hdis γ hm p hp).2.1 hγ))
      rw [hsame]
      rcases k₂ σ hσ p hp with ⟨q, hq', hqs⟩ | hd
      · exact .inl (List.mem_append_right _ (List.mem_map.mpr ⟨q, hq', by rw [hqs]⟩))
      · exact .inr (Stump.dischargeEquiv_iff_holds.mp hd)
  · ------------------------------------------------ inhabitation
    -- an unspent result is sent to ★ (its lookup stays blocked); a spent one is
    -- met by filling its blocker with the field, as `Materialize` does. The
    -- field holds the result with the unspent ones already at ★, so a variable
    -- shared between a spent and an unspent result is no obstacle
    let rd : Parked B → Ty B := fun p =>
      (p.stump.res.applySubst S₁.subst).applySubst (readSub σ ᾱ f)
    let resF : List TyVar :=
      (Δq.filter (fun p => (p.stump.res.applySubst S₁.subst).isVar)).flatMap (fun p => (rd p).ftv)
    let χR : TySubst B :=
      ⟨fun β => if β ∈ resF then .unk else .var β, fun γ => .var γ, fun x => .var x⟩
    let fills : TyVar → List (Label × Ty B) := fun γ =>
      (Δq.filter (fun p => !(p.stump.res.applySubst S₁.subst).isVar && decide (f p.blocker = γ))).map
        (fun p => ((p.stump.label.applySubst S₁.subst).keyName, (rd p).applySubst χR))
    let χ₀ : TySubst B :=
      ⟨χR.ty, fun γ => fillRow (fills γ) γ, fun x => .var x⟩
    have mem_resF : ∀ {β}, β ∈ resF → ∃ q ∈ Δq,
        (q.stump.res.applySubst S₁.subst).isVar = true ∧ β ∈ (rd q).ftv := by
      intro β h
      obtain ⟨q, hq, hm⟩ := List.mem_flatMap.mp h
      obtain ⟨hq, hv⟩ := List.mem_filter.mp hq
      exact ⟨q, hq, hv, hm⟩
    have mem_fills : ∀ {γ lt}, lt ∈ fills γ → ∃ q ∈ Δq,
        (q.stump.res.applySubst S₁.subst).isVar = false ∧ f q.blocker = γ ∧
        lt = ((q.stump.label.applySubst S₁.subst).keyName, (rd q).applySubst χR) := by
      intro γ lt h
      obtain ⟨q, hq, rfl⟩ := List.mem_map.mp h
      obtain ⟨hq, hc⟩ := List.mem_filter.mp hq
      simp only [Bool.and_eq_true, Bool.not_eq_true', decide_eq_true_eq] at hc
      exact ⟨q, hq, hc.1, hc.2, rfl⟩
    -- a filled blocker is some spent stump's, and names a key it has
    have fill_key : ∀ p ∈ Δq, ∀ {l τ}, (fills (f p.blocker)).lookup l = some τ → ∃ q ∈ Δq,
        (q.stump.res.applySubst S₁.subst).isVar = false ∧ q.blocker = p.blocker ∧
        q.stump.label.applySubst S₁.subst = .lit l ∧ τ = (rd q).applySubst χR := by
      intro p hp l τ h
      obtain ⟨q, hq, hqs, hqb, he⟩ := mem_fills (lookup_some_mem h)
      have hqb' : q.blocker = p.blocker := hinj _ (hbq q hq) _ (hbq p hp) hqb
      obtain ⟨l', hl'⟩ := Key.exists_of_isLit (Parked.fillable_iff.mp (hres.2 q hq hqs).1).1
      simp only [Prod.mk.injEq, hl', Key.keyName] at he
      exact ⟨q, hq, hqs, hqb', by rw [hl', he.1], he.2⟩
    refine ⟨_, χ₀, ⟨fun β hβ => ?_, fun β hβ => ?_, fun _ _ => rfl⟩, fun st hst => ?_, rfl⟩
    · have : β ∉ resF := fun hm => by
        obtain ⟨q, hq, -, hm⟩ := mem_resF hm
        rw [hrdftv q hq] at hm
        obtain ⟨δ, hδ, rfl⟩ := List.mem_map.mp hm
        exact hβ (List.mem_map_of_mem (hres.1 q hq δ hδ))
      simp only [χ₀, χR, if_neg this]
    · have : fills β = [] := by
        rcases h : fills β with _ | ⟨lt, _⟩
        · rfl
        · obtain ⟨q, hq, -, hqb, -⟩ := mem_fills (h ▸ List.mem_cons_self : lt ∈ fills β)
          exact absurd (hqb ▸ List.mem_map_of_mem (hbq q hq)) hβ
      show fillRow (fills β) β = _
      rw [this]; rfl
    · obtain ⟨st₀, hst₀, rfl⟩ := List.mem_map.mp hst
      obtain ⟨p, hp, rfl⟩ := List.mem_map.mp hst₀
      -- the read constraint under χ₀ is the S₁-reading under χ₀ ∘ readSub
      have hlk : ∀ r, LookupQ ((p.stump.row.applySubst S₁.subst).applySubst (χ₀.comp (readSub σ ᾱ f)))
          ((p.stump.label.applySubst S₁.subst).applySubst (χ₀.comp (readSub σ ᾱ f))) r →
          LookupQ (((p.stump.row.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀)
            (((p.stump.label.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀) r := by
        intro r h
        rw [Row.applySubst_applySubst, Key.applySubst_applySubst]; exact h
      have hblkrow : (χ₀.comp (readSub σ ᾱ f)).row p.blocker
          = fillRow (fills (f p.blocker)) (f p.blocker) := by
        show ((readSub σ ᾱ f).row p.blocker).applySubst χ₀ = _
        simp only [readSub, if_pos (hbq p hp), Row.applySubst]; rfl
      have hq₁ := q₁ p (hΔq p hp)
      cases hsp : (p.stump.res.applySubst S₁.subst).isVar
      · ---------------- SPENT: the filled blocker makes the lookup hit
        obtain ⟨hfil, hsame, hbl⟩ := hres.2 p hp hsp
        obtain ⟨hlab, hnk⟩ := Parked.fillable_iff.mp hfil
        obtain ⟨l, hl⟩ := Key.exists_of_isLit hlab
        rw [hl] at hq₁
        have hlit : LookupBlocked (p.stump.row.applySubst S₁.subst) l p.blocker :=
          hq₁.toLit
        have hfind := lookup_blocked_fill hlit (χ₀.comp (readSub σ ᾱ f)) hblkrow hnk
        have hmem : (l, (rd p).applySubst χR) ∈ fills (f p.blocker) :=
          List.mem_map.mpr ⟨p, List.mem_filter.mpr ⟨hp, by simp [hsp]⟩, by simp [hl, Key.keyName]⟩
        have hfound : fillRes (fills (f p.blocker)) l = .found ((rd p).applySubst χR) := by
          unfold fillRes
          cases hlk' : (fills (f p.blocker)).lookup l with
          | none => exact absurd hlk' (lookup_none_of_mem hmem)
          | some τ =>
              obtain ⟨q, hq, -, hqb, hql, rfl⟩ := fill_key p hp hlk'
              have hqp := (hsame q hq hqb).2 (hql.trans hl.symm)
              simp only [rd, hqp]
        rw [hfound] at hfind
        refine .hit (τ := (rd p).applySubst χR) (hlk _ ?_) (.of_eq ?_)
        · rw [hl]; exact LookupQ.lab_iff.mpr hfind
        · -- χ₀ is χR on the result: no spent blocker in it
          apply Ty.applySubst_congr
          intro α hα
          change α ∈ (rd p).ftv at hα
          rw [hrdftv p hp] at hα
          obtain ⟨δ, hδ, rfl⟩ := List.mem_map.mp hα
          have hδα := hres.1 p hp δ hδ
          refine ⟨rfl, ?_, rfl⟩
          have : fills (f δ) = [] := by
            rcases h : fills (f δ) with _ | ⟨lt, _⟩
            · rfl
            · obtain ⟨q, hq, hqs, hqb, -⟩ := mem_fills (h ▸ List.mem_cons_self : lt ∈ fills (f δ))
              have hqδ := hinj _ (hbq q hq) _ hδα hqb
              exact absurd (hqδ ▸ hδ) (hbl q hq hqs)
          show fillRow (fills (f δ)) (f δ) = _
          rw [this]; rfl
      · ---------------- UNSPENT: the result goes to ★, the lookup stays blocked
        obtain ⟨δ, hδe⟩ := Ty.exists_of_isVar hsp
        have hδα : δ ∈ ᾱ := hres.1 p hp δ (by rw [hδe]; simp [Ty.ftv])
        have hin : f δ ∈ resF := List.mem_flatMap.mpr ⟨p, List.mem_filter.mpr ⟨hp, hsp⟩, by
          show f δ ∈ ((p.stump.res.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).ftv
          rw [hδe]; simp [Ty.applySubst, readSub, hδα, Ty.ftv]⟩
        have hδres : (rd p).applySubst χ₀ = .unk := by
          show ((p.stump.res.applySubst S₁.subst).applySubst (readSub σ ᾱ f)).applySubst χ₀ = _
          rw [hδe]
          simp only [Ty.applySubst, readSub, if_pos hδα, χ₀, χR, if_pos hin]
        by_cases hfe : fills (f p.blocker) = []
        · -- the blocker is untouched: blocked as before
          -- a variable of p's row or key is one the fresh names avoid, or generalized
          have hL : ∀ γ, γ ∈ (p.stump.label.applySubst S₁.subst).ftv ∨
              γ ∈ (p.stump.row.applySubst S₁.subst).ftv → γ ∈ sc.freeFtv ++ LΓ ++ LΔ := by
            intro γ h
            rcases h with h | h
            · exact List.mem_append_left _ (List.mem_append_left _ (List.mem_append_left _
                (List.mem_flatMap.mpr ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, List.mem_append_right _ h⟩)))
            · exact List.mem_append_left _ (List.mem_append_left _ (List.mem_append_left _
                (List.mem_flatMap.mpr ⟨_, List.mem_map.mpr ⟨p, hp, rfl⟩, List.mem_append_left _ h⟩)))
          have hftv : ∀ {b γ}, (b, γ) ∈ (p.stump.label.applySubst S₁.subst).sortedFtv ++
              (p.stump.row.applySubst S₁.subst).sortedFtv →
              γ ∈ (p.stump.label.applySubst S₁.subst).ftv ∨
              γ ∈ (p.stump.row.applySubst S₁.subst).ftv := by
            intro b γ h
            rcases List.mem_append.mp h with h | h
            · exact .inl (Key.mem_ftv_of_mem_sortedFtv _ h)
            · exact .inr (Row.mem_ftv_of_mem_sortedFtv _ h)
          have hu := lookupQ_blocked_subst hq₁ (χ₀.comp (readSub σ ᾱ f)) ?_ ?_
          · exact .unk (hlk _ hu) hδres
          · intro _
            exact ⟨f p.blocker, by rw [hblkrow, hfe]; rfl⟩
          · -- the blocker keeps its fresh name (χ₀ is the identity on labels) …
            intro _
            refine ⟨f p.blocker, ?_, fun γ hγ he => ?_⟩
            · show ((readSub σ ᾱ f).lab p.blocker).applySubst χ₀ = _
              simp only [readSub, if_pos (hbq p hp), Key.applySubst_var]
              rfl
            · -- … and nothing else in key position is sent to it
              change ((readSub σ ᾱ f).lab γ).applySubst χ₀ = _ at he
              by_cases hγα : γ ∈ ᾱ
              · simp only [readSub, if_pos hγα, Key.applySubst_var] at he
                injection he with he
                exact hinj _ hγα _ (hbq p hp) he
              · simp only [readSub, if_neg hγα] at he
                have hm : f p.blocker ∈ (σ.lab γ).ftv := by
                  cases hs : σ.lab γ with
                  | var x =>
                      rw [hs] at he
                      simp only [Key.applySubst_var] at he
                      injection he with he; subst he; simp [Key.ftv]
                  | lit _ => rw [hs] at he; cases he
                exact absurd (List.mem_map_of_mem (hbq p hp))
                  ((havL γ (hL γ (hftv hγ))).2.2 _ hm)
        · -- the blocker is filled by a spent stump: p's key is literal, and names
          -- none of the fields (a field for it would be p itself, spent)
          obtain ⟨lt, hlt⟩ := List.exists_mem_of_ne_nil _ hfe
          obtain ⟨q, hq, hqs, hqb, -⟩ := mem_fills hlt
          have hqb' : q.blocker = p.blocker := hinj _ (hbq q hq) _ (hbq p hp) hqb
          obtain ⟨l, hl⟩ := Key.exists_of_isLit (Parked.fillable_iff.mp ((hres.2 q hq hqs).2.1 p hp hqb'.symm).1).1
          rw [hl] at hq₁
          have hlit : LookupBlocked (p.stump.row.applySubst S₁.subst) l p.blocker :=
            hq₁.toLit
          have hfind := lookup_blocked_fill hlit (χ₀.comp (readSub σ ᾱ f)) hblkrow
            (Parked.fillable_iff.mp ((hres.2 q hq hqs).2.1 p hp hqb'.symm).1).2
          have hnone : fillRes (fills (f p.blocker)) l = .unknown := by
            unfold fillRes
            cases hlk' : (fills (f p.blocker)).lookup l with
            | none => rfl
            | some τ =>
                exfalso
                obtain ⟨q', hq', hq's, hq'b, hq'l, -⟩ := fill_key p hp hlk'
                have hpq := ((hres.2 q' hq' hq's).2.1 p hp hq'b.symm).2 (hl.trans hq'l.symm)
                rw [hpq] at hsp; rw [hsp] at hq's; exact Bool.noConfusion hq's
          rw [hnone] at hfind
          refine .unk (hlk _ ?_) hδres
          rw [hl]; exact LookupQ.lab_iff.mpr hfind

--------------------- …AND THE STATEMENT ---------------------------------------

/-- ⊢  **Inference soundness, assumption form — PROVED.** Every derivation from a
clean, quiescent state, in a context of
well-formed schemes, is a typing under the assumptions it leaves parked, at every
σ that absorbs and satisfies its final state, in Γ read under σ. -/
theorem inferSound {C : Type} {constTy : C → B} : InferSound B C constTy :=
  inferSound_of_let letCase

/-- ⊢  a run from nothing, at any σ that absorbs and satisfies the state
inference ends in, is a plain L2 typing — given that the stumps still parked
hold at σ, which is finalization's job (`Finalizes.holds`). -/
theorem runSoundA {C : Type} {constTy : C → B}
    {e : Expr C} {τ : Ty B} {S₁ : SolverState B}
    (h : Infer constTy QCtx.empty ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁)
    {σ : TySubst B} (hab : Absorbs σ S₁) (hsat : Sol.Sat σ S₁.sol)
    (hfin : ∀ p ∈ S₁.parked, (p.stump.at σ).Holds) :
    QTyped constTy QCtx.empty e (τ.applySubst σ) :=
  runSoundA_of inferSound h hab hsat hfin

end MinimalCalculus
