-- U-EXPAND AT THE RIGHT END.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- ## Why this module exists
-- `expandL` pairs a LEADING field against the LEADING host of the other side,
-- because the invented field is emitted at the FRONT of `β ≔ (l:δ | β′)` and
-- has to commute out leftwards. Every other move in the driver is two-ended
-- (`stripL`/`stripR`, `matchL`/`matchR`); U-expand was not, so the algorithm
-- succeeded on `(l:𝓫 | α) ≐ᵣ (m:𝓫 | β)` and went STUCK on its mirror
-- `(α | l:𝓫) ≐ᵣ (β | m:𝓫)` — a verdict about the driver, not about the problem.
--
-- ## Why it is not a transport
-- `stripR` and `matchR` are defined by reversal and their metatheory is
-- transported through `revRow`. That works because those lemmas relate
-- SUBSTITUTION-FREE rows, and reversal is a congruence for `≈`. It does NOT
-- work here: `revRow (ρ.applySubst θ)` and `(revRow ρ).applySubst θ` differ —
-- substitution can put a multi-atom row at a variable, and reversing the
-- outside does not reverse what was put inside. So the two load-bearing
-- lemmas, `expand_shift_R` and `host_forced_R`, are genuine mirror PROOFS.
--
-- Only the syntactic side transports, and that part is `A1` below.

import RowUnify.Reflection

namespace MinimalCalculus

--------------------- THE SYNTACTIC REVERSAL BRIDGE ---------------------------
-- `expandR` is `expandL` on the reverses, so its inversion lemma has to move
-- `sVarSeq` / `sFieldCount` / `renameVar` across `List.reverse`. All three are
-- structural; `mem_sFtv_reverse` (Solutions.lean) is the model.

theorem renameVar_append {B : Type} (β β' : TyVar) : (s₁ s₂ : List (Atom B)) →
    renameVar β β' (s₁ ++ s₂) = renameVar β β' s₁ ++ renameVar β β' s₂
  | [], _ => rfl
  | .field _ _ :: s₁, s₂ => congrArg (_ :: ·) (renameVar_append β β' s₁ s₂)
  | .var _ :: s₁, s₂ => congrArg (_ :: ·) (renameVar_append β β' s₁ s₂)

-- ⊢  vars(reverse s) = reverse (vars s)
theorem sVarSeq_reverse {B : Type} : (s : List (Atom B)) →
    sVarSeq s.reverse = (sVarSeq s).reverse
  | [] => rfl
  | .field _ _ :: s => by
      rw [List.reverse_cons, sVarSeq_append, sVarSeq_reverse s]; simp [sVarSeq]
  | .var α :: s => by
      rw [List.reverse_cons, sVarSeq_append, sVarSeq_reverse s]; simp [sVarSeq]

-- ⊢  count_l is reversal-invariant
theorem sFieldCount_reverse {B : Type} (l : Label) : (s : List (Atom B)) →
    sFieldCount l s.reverse = sFieldCount l s
  | [] => rfl
  | .field l' τ :: s => by
      rw [List.reverse_cons, sFieldCount_append, sFieldCount_reverse l s]
      simp only [sFieldCount]; omega
  | .var α :: s => by
      rw [List.reverse_cons, sFieldCount_append, sFieldCount_reverse l s]
      simp only [sFieldCount]; omega

-- ⊢  renaming commutes with reversal
theorem renameVar_reverse {B : Type} (β β' : TyVar) : (s : List (Atom B)) →
    renameVar β β' s.reverse = (renameVar β β' s).reverse
  | [] => rfl
  | .field l τ :: s => by
      rw [List.reverse_cons, renameVar_append, renameVar_reverse β β' s]
      simp [renameVar]
  | .var γ :: s => by
      rw [List.reverse_cons, renameVar_append, renameVar_reverse β β' s]
      simp [renameVar]

--------------------- THE RIGHT-END HOST SHAPE --------------------------------

/-- `HostShape` read at the right end: the host is the TRAILING variable, and
every variable BEFORE it is excused by the self-reference filter. This is what
`uniqueHost` asserts about `s.reverse`, said about `s`. -/
def HostShapeR {B : Type} (l : Label) (τ : Ty B) (s : List (Atom B)) (β : TyVar) : Prop :=
  (∃ rest, sVarSeq s = rest ++ [β] ∧ ∀ γ ∈ rest, γ ∈ Ty.allRowVars τ) ∧
  sFieldCount l s = 0 ∧ β ∉ Ty.allRowVars τ

theorem hostShapeR_of_hostShape {B : Type} {l : Label} {τ : Ty B}
    {s : List (Atom B)} {β : TyVar} (h : HostShape l τ s.reverse β) :
    HostShapeR l τ s β := by
  obtain ⟨⟨rest, hvs, hrest⟩, hc, hnot⟩ := h
  rw [sVarSeq_reverse] at hvs
  refine ⟨⟨rest.reverse, ?_, fun γ hγ => hrest γ (List.mem_reverse.mp hγ)⟩, ?_, hnot⟩
  · have := congrArg List.reverse hvs
    simpa using this
  · rw [← sFieldCount_reverse l s]; exact hc

-- ⊢  the detector's side conditions are exactly the mirror lemmas' hypotheses
theorem expandR_spec {B : Type} {Θ : DepGraph} {S : Supply} {s₁ s₂ : List (Atom B)}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (h : expandR Θ S s₁ s₂ = some (β, l, τ, t₁, t₂)) :
    s₁ = t₁ ++ [.field l τ] ∧ HostShapeR l τ s₂ β ∧
    t₂ = renameVar β S.fresh.2.fresh.1 s₂ := by
  unfold expandR at h
  revert h
  cases hl : expandL Θ S s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨γ, m, σ, u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := h
      obtain ⟨hs1, hshape, hren⟩ := expandL_spec hl
      refine ⟨?_, hostShapeR_of_hostShape hshape, ?_⟩
      · have := congrArg List.reverse hs1
        simpa using this
      · rw [hren, renameVar_reverse]; simp

-- ⊢  the fused move still costs exactly one atom (expandL_len, reversed)
theorem expandR_len {B : Type} {Θ : DepGraph} {S : Supply} {s₁ s₂ : List (Atom B)}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (h : expandR Θ S s₁ s₂ = some (β, l, τ, t₁, t₂)) :
    t₁.length + t₂.length + 1 = s₁.length + s₂.length := by
  unfold expandR at h
  revert h
  cases hl : expandL Θ S s₁.reverse s₂.reverse with
  | none => intro h; cases h
  | some p =>
      intro h
      obtain ⟨γ, m, σ, u₁, u₂⟩ := p
      simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl, rfl, rfl, rfl⟩ := h
      have := expandL_len hl
      simpa using this

--------------------- PROJECTION ALGEBRA AT THE RIGHT END ---------------------
-- `host_forced` reads the l-field off segment index 0 — "no variable precedes
-- it". The mirror reads it off segment index |vars| — "no variable FOLLOWS
-- it". Everything below is that one change of index, carried through the same
-- four lemmas.

-- ⊢  proj_l s = []  ⟹  count_l s = 0   (converse of sProj_nil_of_count_zero)
theorem count_zero_of_sProj_nil {B : Type} (l : Label) :
    (s : List (Atom B)) → sProj l s = [] → sFieldCount l s = 0
  | [], _ => rfl
  | .field l' τ :: s, h => by
      simp only [sProj] at h
      by_cases hl : l' = l
      · rw [if_pos hl] at h; cases h
      · rw [if_neg hl] at h
        simp only [sFieldCount, if_neg hl]
        simpa using count_zero_of_sProj_nil l s h
  | .var γ :: s, h => by
      simp only [sProj, List.map_eq_nil_iff] at h
      simpa only [sFieldCount] using count_zero_of_sProj_nil l s h

-- ⊢  every segment index of an l-projection is bounded by the var count
theorem sProj_index_le {B : Type} (l : Label) :
    (s : List (Atom B)) → ∀ p ∈ sProj l s, p.1 ≤ (sVarSeq s).length
  | [], p, hp => by simp [sProj] at hp
  | .field l' τ :: s, p, hp => by
      simp only [sProj] at hp
      by_cases hl : l' = l
      · rw [if_pos hl] at hp
        rcases List.mem_cons.mp hp with rfl | hp'
        · simp
        · simpa only [sVarSeq] using sProj_index_le l s p hp'
      · rw [if_neg hl] at hp
        simpa only [sVarSeq] using sProj_index_le l s p hp
  | .var γ :: s, p, hp => by
      simp only [sProj, List.mem_map] at hp
      obtain ⟨q, hq, rfl⟩ := hp
      have := sProj_index_le l s q hq
      simp only [sVarSeq, List.length_cons]
      omega

-- ⊢  ps ≈ₚ qs  ⟹  reverse ps ≈ₚ reverse qs  (ProjEquiv is pointwise)
theorem ProjEquiv.reverse {B : Type} {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv ps.reverse qs.reverse
  | .nil => .nil
  | .cons hn hty h => by
      rw [List.reverse_cons, List.reverse_cons]
      exact h.reverse.append (.cons hn hty .nil)

-- ⊢  the head of a projection, at ANY index (head_zero generalised)
theorem ProjEquiv.head_eq {B : Type} {n : Nat} {τ : Ty B} {ps qs : List (Nat × Ty B)}
    (h : ProjEquiv ((n, τ) :: ps) qs) :
    ∃ σ rest, qs = (n, σ) :: rest ∧ TyEquiv τ σ ∧ ProjEquiv ps rest := by
  cases h with
  | cons hn hty ht => exact ⟨_, _, by rw [← hn], hty, ht⟩

-- ⊢  … and the LAST entry, via the reverse congruence
theorem ProjEquiv.last {B : Type} {n : Nat} {τ : Ty B} {ps qs : List (Nat × Ty B)}
    (h : ProjEquiv (ps ++ [(n, τ)]) qs) :
    ∃ σ rest, qs.reverse = (n, σ) :: rest ∧ TyEquiv τ σ := by
  have hr := h.reverse
  rw [List.reverse_append] at hr
  simp only [List.reverse_cons, List.reverse_nil, List.nil_append] at hr
  obtain ⟨σ, rest, hq, hty, -⟩ := hr.head_eq
  exact ⟨σ, rest, hq, hty⟩

-- ## THE MIRROR OF spine_extract
-- The l-field whose segment index is |vars(s)| comes after EVERY variable, and
-- it is the LAST l-occurrence, so everything to its right is a field with a
-- different label and ≈-comm bubbles it to the BACK. Stated on the REVERSED
-- projection, so "last entry" is a head and the induction stays left-to-right.
-- ⊢  reverse (proj_l s) = (|vars s|, σ) :: rest
--        ⟹   ∃ t.  ofSpine s ≈ᵣ (ofSpine t | l:σ)
theorem spine_extract_last {B : Type} {σ : Ty B} :
    (s : List (Atom B)) → (l : Label) → {rest : List (Nat × Ty B)} →
    (sProj l s).reverse = ((sVarSeq s).length, σ) :: rest →
    ∃ t, RowEquiv (ofSpine s) (.cat (ofSpine t) (.sing l σ))
  | [], l, rest, h => by simp [sProj] at h
  | .var γ :: s', l, rest, h => by
      simp only [sProj, sVarSeq, List.length_cons, ← List.map_reverse] at h
      cases hr : (sProj l s').reverse with
      | nil => rw [hr] at h; simp at h
      | cons pr tl =>
          rw [hr] at h
          obtain ⟨m, τ'⟩ := pr
          simp only [List.map_cons, List.cons.injEq, Prod.mk.injEq] at h
          obtain ⟨⟨hm, hτ⟩, -⟩ := h
          subst hτ
          have hm' : m = (sVarSeq s').length := by omega
          subst hm'
          obtain ⟨t', ht'⟩ := spine_extract_last s' l (rest := tl) hr
          refine ⟨.var γ :: t', ?_⟩
          show RowEquiv (.cat (Row.var γ) (ofSpine s')) _
          exact ((RowEquiv.cat (.refl _) ht').trans RowEquiv.assoc.symm)
  | .field l₀ τ₀ :: s', l, rest, h => by
      by_cases hl : l₀ = l
      · subst hl
        simp only [sProj, if_true, sVarSeq, List.reverse_cons] at h
        cases hr : (sProj l₀ s').reverse with
        | nil =>
            rw [hr] at h
            simp only [List.nil_append, List.cons.injEq, Prod.mk.injEq] at h
            obtain ⟨⟨hn, hτ⟩, -⟩ := h
            subst hτ
            have hvs : sVarSeq s' = [] := List.eq_nil_of_length_eq_zero hn.symm
            have hps : sProj l₀ s' = [] := by
              rw [← List.reverse_reverse (sProj l₀ s'), hr]; rfl
            refine ⟨s', ?_⟩
            show RowEquiv (.cat (Row.sing l₀ τ₀) (ofSpine s')) _
            refine field_comm_lfree l₀ τ₀ (ofSpine s') ?_ ?_
            · exact (spineVarFree_iff_varSeq_nil _).mpr (by rw [ofSpine_toSpine]; exact hvs)
            · rw [ofSpine_toSpine]; exact count_zero_of_sProj_nil l₀ s' hps
        | cons pr tl =>
            rw [hr] at h
            obtain ⟨m, τ'⟩ := pr
            simp only [List.cons_append, List.cons.injEq, Prod.mk.injEq] at h
            obtain ⟨⟨hm, hτ⟩, -⟩ := h
            subst hτ; subst hm
            obtain ⟨t', ht'⟩ := spine_extract_last s' l₀ (rest := tl) hr
            refine ⟨.field l₀ τ₀ :: t', ?_⟩
            show RowEquiv (.cat (Row.sing l₀ τ₀) (ofSpine s')) _
            exact ((RowEquiv.cat (.refl _) ht').trans RowEquiv.assoc.symm)
      · simp only [sProj, if_neg hl, sVarSeq] at h
        obtain ⟨t', ht'⟩ := spine_extract_last s' l (rest := rest) h
        refine ⟨.field l₀ τ₀ :: t', ?_⟩
        show RowEquiv (.cat (Row.sing l₀ τ₀) (ofSpine s')) _
        exact ((RowEquiv.cat (.refl _) ht').trans RowEquiv.assoc.symm)

-- ## THE HOST IS FORCED AT THE RIGHT END
-- Mirror of `proj_head_zero_var`. Index |vars| says no variable FOLLOWS the
-- l-field, so it can only have come from a variable of the spine hosting it at
-- the BACK. The split is on whether the TAIL still carries an l-field: if it
-- does the entry belongs to the tail and the index shifts back by the leading
-- variable's own var count; if it does not, the index bound forces the tail to
-- be var-free and pins the entry to the end of the leading variable.
-- ⊢  count_l(s) = 0,  reverse (proj_l (θ (ofSpine s))) starts at index |vars| with σ
--        ⟹   some γ ∈ vars(s) has  θγ ≈ᵣ (ρ' | l:σ)
theorem proj_last_var {B : Type} {θ : TySubst B} {l : Label} {σ : Ty B} :
    (s : List (Atom B)) → sFieldCount l s = 0 →
    {rest : List (Nat × Ty B)} →
    (sProj l ((ofSpine s).applySubst θ).toSpine).reverse =
      ((sVarSeq ((ofSpine s).applySubst θ).toSpine).length, σ) :: rest →
    ∃ γ ∈ sVarSeq s, ∃ ρ' : Row B, RowEquiv (θ.row γ) (.cat ρ' (.sing l σ))
  | [], _, rest, h => by simp [ofSpine, Row.applySubst, Row.toSpine, sProj] at h
  | .field l' τ' :: s, hc, rest, h => by
      simp only [sFieldCount] at hc
      have hl : ¬ l' = l := by intro hh; rw [if_pos hh] at hc; omega
      rw [if_neg hl] at hc
      have hcf : sFieldCount l s = 0 := by omega
      have hsp : ((ofSpine (Atom.field l' τ' :: s)).applySubst θ).toSpine =
          [Atom.field l' (τ'.applySubst θ)] ++
            ((ofSpine s).applySubst θ).toSpine := rfl
      rw [hsp, sProj_append, sVarSeq_append] at h
      simp only [sProj, if_neg hl, sVarSeq, List.length_nil, List.nil_append,
        map_add_zero] at h
      obtain ⟨γ, hγ, hres⟩ := proj_last_var s hcf h
      exact ⟨γ, by rw [sVarSeq]; exact hγ, hres⟩
  | .var γ :: s, hc, rest, h => by
      simp only [sFieldCount] at hc
      have hc' : sFieldCount l s = 0 := by omega
      have hsp : ((ofSpine (Atom.var γ :: s)).applySubst θ).toSpine =
          (θ.row γ).toSpine ++ ((ofSpine s).applySubst θ).toSpine := rfl
      rw [hsp, sProj_append, sVarSeq_append, List.length_append,
        List.reverse_append, ← List.map_reverse] at h
      cases hb : (sProj l ((ofSpine s).applySubst θ).toSpine).reverse with
      | cons pr tl =>
          rw [hb] at h
          obtain ⟨m, τ''⟩ := pr
          simp only [List.map_cons, List.cons_append, List.cons.injEq,
            Prod.mk.injEq] at h
          obtain ⟨⟨hm, hτ⟩, -⟩ := h
          subst hτ
          have hm' : m = (sVarSeq ((ofSpine s).applySubst θ).toSpine).length := by omega
          subst hm'
          obtain ⟨δ, hδ, hres⟩ := proj_last_var s hc' hb
          exact ⟨δ, by rw [sVarSeq]; exact List.mem_cons_of_mem _ hδ, hres⟩
      | nil =>
          rw [hb] at h
          simp only [List.map_nil, List.nil_append] at h
          have hmem : ((sVarSeq (θ.row γ).toSpine).length +
              (sVarSeq ((ofSpine s).applySubst θ).toSpine).length, σ)
              ∈ sProj l (θ.row γ).toSpine := by
            rw [← List.mem_reverse, h]; exact List.mem_cons_self
          have hle := sProj_index_le l (θ.row γ).toSpine _ hmem
          have hzero : (sVarSeq ((ofSpine s).applySubst θ).toSpine).length = 0 := by omega
          rw [hzero, Nat.add_zero] at h
          obtain ⟨t, ht⟩ := spine_extract_last (θ.row γ).toSpine l h
          exact ⟨γ, by rw [sVarSeq]; exact List.mem_cons_self, ofSpine t,
            (Row.toSpine_equiv _).trans ht⟩


--------------------- THE TWO LOAD-BEARING MIRROR LEMMAS ----------------------

-- ⊢  γ occurs in τ,  θγ hosts an l-field whose payload is θτ, AT THE BACK  ⟹  ⊥
-- `Row.rcdDepth` of a `.cat` is a `max`, which is symmetric, so the arithmetic
-- of `selfref_no_l_field` does not see which end the field sits at.
theorem selfref_no_l_field_R {B : Type} {θ : TySubst B} {γ : TyVar} {τ σ : Ty B}
    {l : Label} {ρ' : Row B}
    (hmem : γ ∈ Ty.allRowVars τ)
    (hty : TyEquiv (τ.applySubst θ) σ)
    (hγ : RowEquiv (θ.row γ) (.cat ρ' (.sing l σ))) : False := by
  have hlt := Ty.rcdDepth_applySubst_gt (θ := θ) (α := γ) τ
    (by rw [Ty.deepRowVars_eq_allRowVars]; exact hmem)
  have heq := TyEquiv.rcdDepth_eq hty
  have hd := RowEquiv.rcdDepth_eq hγ
  simp only [Row.rcdDepth] at hd
  omega

-- ⊢  HostShapeR l τ s₂ β,  θ ⊨ (ofSpine t₁ | l:τ) ≐ᵣ ofSpine s₂
--        ⟹   ∃ σ ρ'.  θτ ≈ₜ σ  ∧  θβ ≈ᵣ (ρ' | l:σ)
-- The host must expand with an l-field AT THE BACK: the field's segment index
-- is the side's full variable count, which says nothing but fields follow it,
-- and those all carry other labels — so `spine_extract_last` bubbles it out.
-- As in `host_forced`, the candidate the last index names need not be β a
-- priori; every OTHER variable of the side occurs in τ, and
-- `selfref_no_l_field_R` says such a variable cannot host this field.
theorem host_forced_R {B : Type} {θ : TySubst B} {β : TyVar} {l : Label} {τ : Ty B}
    {t₁ s₂ : List (Atom B)} (hs : HostShapeR l τ s₂ β)
    (hu : Unifies θ (.cat (ofSpine t₁) (.sing l τ)) (ofSpine s₂)) :
    ∃ (σ : Ty B) (ρ' : Row B),
      TyEquiv (τ.applySubst θ) σ ∧ RowEquiv (θ.row β) (.cat ρ' (.sing l σ)) := by
  obtain ⟨⟨rest, hvs, hrest⟩, hc, -⟩ := hs
  have hL : sProj l ((Row.cat (ofSpine t₁) (Row.sing l τ)).applySubst θ).toSpine =
      sProj l ((ofSpine t₁).applySubst θ).toSpine ++
        [((sVarSeq ((ofSpine t₁).applySubst θ).toSpine).length, τ.applySubst θ)] := by
    show sProj l (((ofSpine t₁).applySubst θ).toSpine ++
                  ((Row.sing l τ).applySubst θ).toSpine) = _
    rw [sProj_append]
    simp [Row.applySubst, Row.toSpine, sProj]
  have hV : sVarSeq ((Row.cat (ofSpine t₁) (Row.sing l τ)).applySubst θ).toSpine =
      sVarSeq ((ofSpine t₁).applySubst θ).toSpine := by
    show sVarSeq (((ofSpine t₁).applySubst θ).toSpine ++
                  ((Row.sing l τ).applySubst θ).toSpine) = _
    rw [sVarSeq_append]; simp [Row.applySubst, Row.toSpine, sVarSeq]
  have hvar := (RowEquiv.char hu).1
  have hp := (RowEquiv.char hu).2 l
  rw [hL] at hp
  obtain ⟨σ, rest', hq, hty⟩ := ProjEquiv.last hp
  have hidx : (sVarSeq ((ofSpine t₁).applySubst θ).toSpine).length =
      (sVarSeq ((ofSpine s₂).applySubst θ).toSpine).length := by
    rw [← hV, hvar]
  rw [hidx] at hq
  obtain ⟨γ, hγmem, ρ', hγ⟩ := proj_last_var s₂ hc hq
  rw [hvs] at hγmem
  rcases List.mem_append.mp hγmem with hmem | hmem
  · exact (selfref_no_l_field_R (hrest _ hmem) hty hγ).elim
  · rw [List.mem_singleton] at hmem
    subst hmem
    exact ⟨σ, ρ', hty, hγ⟩

-- ## THE ALGEBRAIC SHIFT, MIRRORED
-- `expand_shift` pulls the invented field out to the FRONT and needs ≈-comm to
-- cross the l-free fields before the host. The mirror pulls it out to the BACK,
-- and is EASIER: the host is last, so the recursion reaches it only after every
-- atom to its left, and each of those steps is pure associativity. Commutation
-- is needed exactly once — in the base case, to cross the l-free var-free
-- remainder that sits to the host's right.
theorem expand_shift_R {B : Type} {θ : TySubst B} {l : Label} {σ : Ty B}
    {β β' : TyVar}
    (hβ : RowEquiv (θ.row β) (.cat (θ.row β') (.sing l σ))) :
    (s : List (Atom B)) → {rest : List TyVar} → β ∉ rest →
    sVarSeq s = rest ++ [β] → sFieldCount l s = 0 →
    RowEquiv ((ofSpine s).applySubst θ)
             (.cat ((ofSpine (renameVar β β' s)).applySubst θ) (.sing l σ))
  | [], rest, _, hv, _ => by simp only [sVarSeq] at hv; exact absurd hv.symm (by simp)
  | .var γ :: s, rest, hnr, hv, hc => by
      simp only [sVarSeq] at hv
      cases rest with
      | nil =>
          simp only [List.nil_append, List.cons.injEq] at hv
          obtain ⟨rfl, hs⟩ := hv
          show RowEquiv (.cat (θ.row γ) ((ofSpine s).applySubst θ)) _
          rw [renameVar, if_pos rfl, renameVar_not_mem γ β' s (by rw [hs]; exact fun h => nomatch h)]
          have hvf : ((ofSpine s).applySubst θ).SpineVarFree :=
            spineVarFree_applySubst θ
              ((spineVarFree_iff_varSeq_nil _).2 (by rw [ofSpine_toSpine]; exact hs))
          have hlf : sFieldCount l ((ofSpine s).applySubst θ).toSpine = 0 := by
            rw [sFieldCount_applySubst_varFree θ l
              ((spineVarFree_iff_varSeq_nil _).2 (by rw [ofSpine_toSpine]; exact hs)),
              ofSpine_toSpine]
            simpa only [sFieldCount] using hc
          exact ((RowEquiv.cat hβ (.refl _)).trans RowEquiv.assoc).trans
            (((RowEquiv.cat (.refl _)
                (field_comm_lfree l σ _ hvf hlf)).trans RowEquiv.assoc.symm))
      | cons γ₀ rest' =>
          simp only [List.cons_append, List.cons.injEq] at hv
          obtain ⟨rfl, hs⟩ := hv
          have hne : ¬ γ = β := fun h => hnr (h ▸ List.mem_cons_self)
          have ih := expand_shift_R hβ s (rest := rest')
            (fun hm => hnr (List.mem_cons_of_mem _ hm)) hs (by simpa only [sFieldCount] using hc)
          show RowEquiv (.cat (θ.row γ) ((ofSpine s).applySubst θ)) _
          rw [renameVar, if_neg hne]
          exact ((RowEquiv.cat (.refl _) ih).trans RowEquiv.assoc.symm)
  | .field l' τ' :: s, rest, hnr, hv, hc => by
      simp only [sVarSeq] at hv
      simp only [sFieldCount] at hc
      have hl : ¬ l' = l := by intro hh; rw [if_pos hh] at hc; omega
      rw [if_neg hl] at hc
      have ih := expand_shift_R hβ s (rest := rest) hnr hv (by omega)
      show RowEquiv (.cat (.sing l' (τ'.applySubst θ)) ((ofSpine s).applySubst θ)) _
      rw [renameVar]
      exact ((RowEquiv.cat (.refl _) ih).trans RowEquiv.assoc.symm)


--------------------- REFLECTION, BOTH DIRECTIONS -----------------------------

-- A trailing field, as a `cat` — the shape the mirror lemmas are stated in.
theorem ofSpine_snoc {B : Type} (t : List (Atom B)) (l : Label) (τ : Ty B) :
    RowEquiv (ofSpine (t ++ [Atom.field l τ])) (.cat (ofSpine t) (.sing l τ)) :=
  (ofSpine_append t [Atom.field l τ]).trans
    (RowEquiv.cat (.refl _) RowEquiv.unitR)

theorem ftv_cat_snoc {B : Type} (t : List (Atom B)) (l : Label) (τ : Ty B) :
    (Row.cat (ofSpine t) (Row.sing l τ)).ftv = sFtv (t ++ [Atom.field l τ]) := by
  rw [sFtv_append]
  simp only [Row.ftv, ← sFtv_ofSpine, sFtv, List.append_nil]

-- BACKWARD (soundness): a θ that meets the emitted binding and equation and
-- unifies the residual unified the original.
theorem expandR_reflect {B : Type} {θ : TySubst B} {l : Label} {τ δ : Ty B}
    {β β' : TyVar} {t₁ s₂ : List (Atom B)}
    (hs : HostShapeR l τ s₂ β)
    (hβ : RowEquiv (θ.row β) (.cat (θ.row β') (.sing l δ)))
    (hty : TyEquiv (τ.applySubst θ) δ)
    (hrec : Unifies θ (ofSpine t₁) (ofSpine (renameVar β β' s₂))) :
    Unifies θ (.cat (ofSpine t₁) (.sing l τ)) (ofSpine s₂) := by
  obtain ⟨⟨rest, hvs, hrest⟩, hc, hnot⟩ := hs
  show RowEquiv (.cat ((ofSpine t₁).applySubst θ) (.sing l (τ.applySubst θ))) _
  exact (RowEquiv.cat hrec (RowEquiv.sing hty)).trans
    (expand_shift_R hβ s₂ (fun hm => hnot (hrest _ hm)) hvs hc).symm

-- FORWARD (completeness): a unifier of the original EXTENDS to one that meets
-- the binding and the emitted equation and unifies the residual. The extension
-- only touches δ and β′, which are FRESH — which is why the move does not
-- shrink the unifier set (unlike matchR/groundMatch).
theorem expandR_reflect_fwd {B : Type} {θ : TySubst B} {l : Label} {τ : Ty B}
    {β dv β' : TyVar} {t₁ s₂ : List (Atom B)}
    (hs : HostShapeR l τ s₂ β)
    (hd₁ : dv ∉ sFtv (t₁ ++ [Atom.field l τ])) (hd₂ : dv ∉ sFtv s₂)
    (hb₁ : β' ∉ sFtv (t₁ ++ [Atom.field l τ])) (hb₂ : β' ∉ sFtv s₂)
    (hu : Unifies θ (.cat (ofSpine t₁) (.sing l τ)) (ofSpine s₂)) :
    ∃ θ' : TySubst B,
      RowEquiv (θ'.row β) (.cat (θ'.row β') (.sing l (θ'.ty dv))) ∧
      TyEquiv (τ.applySubst θ') (θ'.ty dv) ∧
      Unifies θ' (ofSpine t₁) (ofSpine (renameVar β β' s₂)) ∧
      Unifies θ' (.cat (ofSpine t₁) (.sing l τ)) (ofSpine s₂) ∧
      (∀ γ, γ ≠ dv → γ ≠ β' → θ.ty γ = θ'.ty γ ∧ θ.row γ = θ'.row γ) := by
  obtain ⟨σ, ρ', hty, hβ⟩ := host_forced_R hs hu
  obtain ⟨⟨rest, hvs, hrest⟩, hc, hnot⟩ := hs
  have hββ' : β ≠ β' := fun h =>
    hb₂ (h ▸ mem_sFtv_of_mem_sVarSeq s₂ (by rw [hvs]; exact List.mem_append_right _ List.mem_cons_self))
  have hdv : ((θ.setTy dv σ).setRow β' ρ').ty dv = σ := by
    show (if dv = dv then σ else θ.ty dv) = σ
    rw [if_pos rfl]
  have hβ'' : RowEquiv (((θ.setTy dv σ).setRow β' ρ').row β)
      (.cat (((θ.setTy dv σ).setRow β' ρ').row β') (.sing l σ)) := by
    show RowEquiv (if β = β' then ρ' else θ.row β)
      (.cat (if β' = β' then ρ' else θ.row β') (.sing l σ))
    rw [if_neg hββ', if_pos rfl]
    exact hβ
  have hd₁' : dv ∉ (Row.cat (ofSpine t₁) (Row.sing l τ)).ftv := by
    rw [ftv_cat_snoc]; exact hd₁
  have hb₁' : β' ∉ (Row.cat (ofSpine t₁) (Row.sing l τ)).ftv := by
    rw [ftv_cat_snoc]; exact hb₁
  have hu' : Unifies ((θ.setTy dv σ).setRow β' ρ')
      (.cat (ofSpine t₁) (.sing l τ)) (ofSpine s₂) := by
    unfold Unifies
    rw [Row.applySubst_setRow_of_not_mem _ hb₁',
        Row.applySubst_setTy_of_not_mem _ hd₁',
        Row.applySubst_setRow_of_not_mem _ (by rw [← sFtv_ofSpine]; exact hb₂),
        Row.applySubst_setTy_of_not_mem _ (by rw [← sFtv_ofSpine]; exact hd₂)]
    exact hu
  refine ⟨(θ.setTy dv σ).setRow β' ρ', by rw [hdv]; exact hβ'', ?_, ?_, hu', ?_⟩
  · have hd : dv ∉ τ.ftv := fun h =>
      hd₁ (by rw [sFtv_append]; exact List.mem_append_right _ (by simp only [sFtv, List.append_nil]; exact h))
    have hb : β' ∉ τ.ftv := fun h =>
      hb₁ (by rw [sFtv_append]; exact List.mem_append_right _ (by simp only [sFtv, List.append_nil]; exact h))
    rw [hdv, Ty.applySubst_setRow_of_not_mem τ hb,
        Ty.applySubst_setTy_of_not_mem τ hd]
    exact hty
  · have key : RowEquiv
        (.cat ((ofSpine t₁).applySubst ((θ.setTy dv σ).setRow β' ρ'))
              (.sing l (τ.applySubst ((θ.setTy dv σ).setRow β' ρ'))))
        (.cat ((ofSpine (renameVar β β' s₂)).applySubst ((θ.setTy dv σ).setRow β' ρ'))
              (.sing l σ)) :=
      hu'.trans (expand_shift_R hβ'' s₂ (fun hm => hnot (hrest _ hm)) hvs hc)
    exact key.field_cancel_right.2
  · intro γ hγd hγb
    exact ⟨by simp only [TySubst.setRow, TySubst.setTy, if_neg hγd],
           by simp only [TySubst.setRow, TySubst.setTy, if_neg hγb]⟩

-- … in the form the driver arm meets it: the original side is a SNOC.
theorem expandR_reflect' {B : Type} {θ : TySubst B} {l : Label} {τ δ : Ty B}
    {β β' : TyVar} {t₁ s₂ : List (Atom B)}
    (hs : HostShapeR l τ s₂ β)
    (hβ : RowEquiv (θ.row β) (.cat (θ.row β') (.sing l δ)))
    (hty : TyEquiv (τ.applySubst θ) δ)
    (hrec : Unifies θ (ofSpine t₁) (ofSpine (renameVar β β' s₂))) :
    Unifies θ (ofSpine (t₁ ++ [Atom.field l τ])) (ofSpine s₂) := by
  have h := expandR_reflect hs hβ hty hrec
  unfold Unifies at h ⊢
  exact (RowEquiv.applySubst θ (ofSpine_snoc t₁ l τ)).trans h

--------------------- THE ARM-LEVEL LEMMAS ------------------------------------

-- ⊢  the advanced supply still avoids the residual (expandL_avoids, mirrored)
theorem expandR_avoids {B : Type} {Θ : DepGraph} {S : Supply} {s₁ s₂ : List (Atom B)}
    {β : TyVar} {l : Label} {τ : Ty B} {t₁ t₂ : List (Atom B)}
    (hS : S.Avoids (sFtv s₁ ++ sFtv s₂))
    (h : expandR Θ S s₁ s₂ = some (β, l, τ, t₁, t₂)) :
    S.fresh.2.fresh.2.Avoids (sFtv t₁ ++ sFtv t₂) := by
  obtain ⟨hs1, -, hren⟩ := expandR_spec h
  have hsub : sFtv t₁ ++ sFtv t₂ ⊆ S.fresh.2.fresh.1 :: (sFtv s₁ ++ sFtv s₂) := by
    intro x hx
    rcases List.mem_append.mp hx with hh | hh
    · refine List.mem_cons_of_mem _ (List.mem_append_left _ ?_)
      rw [hs1, sFtv_append]
      exact List.mem_append_left _ hh
    · rw [hren] at hh
      rcases List.mem_cons.mp (sFtv_renameVar _ _ s₂ x hh) with rfl | hh'
      · exact List.mem_cons_self
      · exact List.mem_cons_of_mem _ (List.mem_append_right _ hh')
  refine Supply.Avoids.mono hsub ?_
  exact hS.advance.cons_fresh

-- FORWARD REFLECTION for the arm: a unifier of the original yields one of the
-- residual (a different substitution — it fixes δ and β′, which the original
-- problem does not mention).
theorem expandR_reflect_fwd' {B : Type} {Θ : DepGraph} {S : Supply} {θ : TySubst B}
    {s₁ s₂ : List (Atom B)} {β : TyVar} {l : Label} {τ : Ty B}
    {t₁ t₂ : List (Atom B)}
    (hS : S.Avoids (sFtv s₁ ++ sFtv s₂))
    (h : expandR Θ S s₁ s₂ = some (β, l, τ, t₁, t₂))
    (hu : Unifies θ (ofSpine s₁) (ofSpine s₂)) :
    ∃ θ' : TySubst B, Unifies θ' (ofSpine t₁) (ofSpine t₂) := by
  obtain ⟨hs1, hshape, hren⟩ := expandR_spec h
  have hd := Supply.fresh_not_mem hS
  have hb := Supply.fresh_not_mem hS.advance
  rw [List.mem_append] at hd hb
  rw [hs1] at hu
  have hu' : Unifies θ (.cat (ofSpine t₁) (.sing l τ)) (ofSpine s₂) := by
    unfold Unifies at hu ⊢
    exact (RowEquiv.applySubst θ (ofSpine_snoc t₁ l τ)).symm.trans hu
  obtain ⟨θ', -, -, hrec, -, -⟩ :=
    expandR_reflect_fwd hshape
      (fun hm => hd (.inl (hs1 ▸ hm))) (fun hm => hd (.inr hm))
      (fun hm => hb (.inl (hs1 ▸ hm))) (fun hm => hb (.inr hm)) hu'
  exact ⟨θ', by rw [hren]; exact hrec⟩


end MinimalCalculus
