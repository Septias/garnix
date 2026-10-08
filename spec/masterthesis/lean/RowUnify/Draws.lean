-- DRAWS: a success draws fewer names than it binds keys.
--
-- Part of RowUnify; see RowUnify.lean for the overview.
--
-- U-host is the only arm that draws a name, and it draws one per firing while
-- binding one key (its host β). On its own that is a tie, and a tie is what
-- breaks the old termination measure: an inner U-host binds γ ≔ (l:τ | γ′),
-- and applied to an outer residual the count of variables stays put while the
-- size grows. The tie never stands, because U-host's residual can never be
-- solved without a key: β′ sits on one side only, so the identity does not
-- unify the two sides. So every success that binds anything binds MORE keys
-- than it draws names, and every success that binds nothing draws nothing.
--
-- `nv_residual` turns that into the termination measure: the residual of a
-- solved stage has strictly fewer distinct variables than the stage's problem.

import RowUnify.OccursLift

namespace MinimalCalculus

------------------------- COUNTING DISTINCT NAMES -------------------------------

/-- `l` without repeats. -/
def dd {α : Type} [DecidableEq α] : List α → List α
  | [] => []
  | x :: xs => if x ∈ xs then dd xs else x :: dd xs

theorem mem_dd {α : Type} [DecidableEq α] {x : α} : {l : List α} → x ∈ dd l ↔ x ∈ l
  | [] => by simp [dd]
  | y :: ys => by
      unfold dd
      split
      · rename_i hy
        rw [mem_dd]
        constructor
        · exact List.mem_cons_of_mem _
        · intro h
          rcases List.mem_cons.mp h with rfl | h
          · exact hy
          · exact h
      · simp [mem_dd]

theorem nodup_dd {α : Type} [DecidableEq α] : (l : List α) → (dd l).Nodup
  | [] => List.nodup_nil
  | y :: ys => by
      unfold dd
      split
      · exact nodup_dd ys
      · rename_i h
        exact List.nodup_cons.mpr ⟨fun h' => h (mem_dd.mp h'), nodup_dd ys⟩

-- ⊢  a repeat-free list inside `M` is no longer than `M`
theorem length_le_of_nodup_sub {α : Type} [DecidableEq α] :
    {L M : List α} → L.Nodup → (∀ x ∈ L, x ∈ M) → L.length ≤ M.length
  | [], _, _, _ => Nat.zero_le _
  | a :: L, M, hn, hs => by
      obtain ⟨ha, hn'⟩ := List.nodup_cons.mp hn
      have hm : a ∈ M := hs a List.mem_cons_self
      have ih := length_le_of_nodup_sub (M := M.erase a) hn' (fun x hx =>
        (List.mem_erase_of_ne (fun h => by subst h; exact ha hx)).mpr (hs x (List.mem_cons_of_mem _ hx)))
      rw [List.length_erase_of_mem hm] at ih
      have := List.length_pos_of_mem hm
      simp only [List.length_cons]
      omega

/-- How many DISTINCT names `P` mentions. -/
def nv {α : Type} [DecidableEq α] (P : List α) : Nat := (dd P).length

theorem nv_mono {α : Type} [DecidableEq α] {P P' : List α} (h : ∀ x ∈ P, x ∈ P') :
    nv P ≤ nv P' :=
  length_le_of_nodup_sub (nodup_dd P) (fun x hx => mem_dd.mpr (h x (mem_dd.mp hx)))

theorem nv_pos {α : Type} [DecidableEq α] {P : List α} {x : α} (h : x ∈ P) : 0 < nv P :=
  List.length_pos_of_mem (mem_dd.mpr h)

-- ⊢  two disjoint parts of `Z` count at most `Z`
theorem nv_add_le {α : Type} [DecidableEq α] {X Y Z : List α} (hd : ∀ x ∈ X, x ∉ Y)
    (hX : ∀ x ∈ X, x ∈ Z) (hY : ∀ x ∈ Y, x ∈ Z) : nv X + nv Y ≤ nv Z := by
  have h := length_le_of_nodup_sub (L := dd X ++ dd Y) (M := dd Z)
    (List.nodup_append.mpr ⟨nodup_dd X, nodup_dd Y,
      fun a ha b hb hab => hd a (mem_dd.mp ha) (hab ▸ mem_dd.mp hb)⟩)
    (fun x hx => mem_dd.mpr (by
      rcases List.mem_append.mp hx with h | h
      · exact hX x (mem_dd.mp h)
      · exact hY x (mem_dd.mp h)))
  simpa [nv] using h

-- ⊢  THE COUNT A STAGE LEAVES. The residual lies in Q ∪ D off the keys K, and
--    the keys lie in Q ∪ D: together they have at most as many names as Q
--    plus D. With fewer draws than keys (|D| < |K|), the residual has fewer
--    names than Q.
theorem nv_residual {α : Type} [DecidableEq α] {Q D K Res : List α}
    (hK : ∀ x ∈ K, x ∈ Q ∨ x ∈ D) (hRes : ∀ x ∈ Res, (x ∈ Q ∨ x ∈ D) ∧ x ∉ K) :
    nv Res + nv K ≤ nv Q + D.length := by
  let p : α → Bool := fun x => decide (x ∈ K)
  have h1 : nv Res ≤ ((dd Q).filter (fun a => decide ¬p a = true) ++
      D.filter (fun a => decide ¬p a = true)).length :=
    length_le_of_nodup_sub (nodup_dd Res) (fun x hx => by
      obtain ⟨hq, hk⟩ := hRes x (mem_dd.mp hx)
      simp only [List.mem_append, List.mem_filter, mem_dd, p, decide_eq_true_eq, hk,
        not_false_eq_true, decide_true, and_true]
      exact hq)
  have h2 : nv K ≤ ((dd Q).filter p ++ D.filter p).length :=
    length_le_of_nodup_sub (nodup_dd K) (fun x hx => by
      have hk := mem_dd.mp hx
      simp only [List.mem_append, List.mem_filter, mem_dd, p, decide_eq_true_eq, hk, and_true]
      exact hK x hk)
  have e1 := List.length_eq_countP_add_countP p (l := dd Q)
  have e2 := List.length_eq_countP_add_countP p (l := D)
  simp only [List.countP_eq_length_filter] at e1 e2
  simp only [List.length_append] at h1 h2
  simp only [nv] at h1 h2 ⊢
  omega

------------------------- THE NAMES A RUN CAN DRAW ------------------------------

/-- Every name the supply hands out between `S` and `S′`. -/
def drawn (S S' : Supply) : List (Srt × TyVar) :=
  (List.range' S.next (S'.next - S.next)).map fun k => (.row, natName k)

theorem drawn_nodup (S S' : Supply) : (drawn S S').Nodup := by
  unfold drawn
  exact List.pairwise_map.mpr ((List.nodup_range' (s := S.next) (n := S'.next - S.next)).imp
    fun hne he => hne (natName_inj (Prod.mk.inj he).2))

theorem drawn_length (S S' : Supply) : (drawn S S').length = S'.next - S.next := by
  simp [drawn]

theorem FreshIn.mem_drawn {S S' : Supply} {R : List (Srt × TyVar)} (h : FreshIn S S' R)
    {x : Srt × TyVar} (hx : x ∈ R) : x ∈ drawn S S' := by
  obtain ⟨lo, hi, he⟩ := h x hx
  rw [he]
  exact List.mem_map.mpr ⟨_, List.mem_range'_1.mpr ⟨lo, by omega⟩, rfl⟩

------------------------- THE KEYS OF A COMPOSITE --------------------------------

theorem Sol.mem_domS_comp_left {B : Type} {s₁ s₂ : Sol B} {x : Srt × TyVar}
    (h : x ∈ s₁.domS) : x ∈ (s₂.comp s₁).domS := by
  simp only [Sol.domS, Sol.comp, List.map_append, List.map_map, List.mem_append,
    List.mem_map, Function.comp] at h ⊢
  rcases h with (⟨p, hp, rfl⟩ | ⟨p, hp, rfl⟩) | ⟨p, hp, rfl⟩
  · exact .inl (.inl (.inl ⟨p, hp, rfl⟩))
  · exact .inl (.inr (.inl ⟨p, hp, rfl⟩))
  · exact .inr (.inl ⟨p, hp, rfl⟩)

theorem Sol.mem_domS_comp_right {B : Type} {s₁ s₂ : Sol B} {x : Srt × TyVar}
    (h : x ∈ s₂.domS) : x ∈ (s₂.comp s₁).domS := by
  simp only [Sol.domS, Sol.comp, List.map_append, List.map_map, List.mem_append,
    List.mem_map, Function.comp] at h ⊢
  rcases h with (⟨p, hp, rfl⟩ | ⟨p, hp, rfl⟩) | ⟨p, hp, rfl⟩
  · exact .inl (.inl (.inr ⟨p, hp, rfl⟩))
  · exact .inl (.inr (.inr ⟨p, hp, rfl⟩))
  · exact .inr (.inr ⟨p, hp, rfl⟩)

theorem Sol.domS_comp_nil {B : Type} {s₁ s₂ : Sol B} (h₁ : s₁.domS = []) (h₂ : s₂.domS = []) :
    (s₂.comp s₁).domS = [] :=
  List.eq_nil_iff_forall_not_mem.mpr fun _ hx => by
    rcases Sol.mem_domS_comp hx with h | h
    · rw [h₁] at h; cases h
    · rw [h₂] at h; cases h

------------------------- THE INVARIANT ----------------------------------------

/-- From `S` to `S′`, `s` drew fewer names than it has keys — or it has no key
and drew nothing. -/
def Sol.Draws {B : Type} (S S' : Supply) (s : Sol B) : Prop :=
  S'.next < S.next + nv s.domS ∨ (s.domS = [] ∧ S'.next = S.next)

-- ⊢  drawing nothing
theorem Sol.Draws.same {B : Type} (S : Supply) (s : Sol B) : s.Draws S S := by
  match hd : s.domS with
  | [] => exact .inr ⟨hd, rfl⟩
  | x :: _ =>
      have := nv_pos (P := s.domS) (x := x) (by rw [hd]; exact List.mem_cons_self)
      exact .inl (by omega)

-- ⊢  STAGED: two stages with disjoint keys add up
theorem Sol.Draws.comp {B : Type} {S S₁ S' : Supply} {s₁ s₂ : Sol B}
    (h₁ : s₁.Draws S S₁) (h₂ : s₂.Draws S₁ S') (hd : ∀ x ∈ s₁.domS, x ∉ s₂.domS) :
    (s₂.comp s₁).Draws S S' := by
  have hn := nv_add_le hd (fun _ hx => Sol.mem_domS_comp_left (s₂ := s₂) hx)
    (fun _ hx => Sol.mem_domS_comp_right (s₁ := s₁) hx)
  rcases h₁ with h₁ | ⟨e₁, f₁⟩ <;> rcases h₂ with h₂ | ⟨e₂, f₂⟩
  · exact .inl (by omega)
  · exact .inl (by omega)
  · exact .inl (by omega)
  · exact .inr ⟨Sol.domS_comp_nil e₁ e₂, by omega⟩

-- ⊢  U-HOST: one draw and one key, on top of a residual that binds something
--    other than β
theorem Sol.Draws.host {B : Type} {S S' : Supply} {s₀ : Sol B} {β : TyVar} {l : Label}
    {τ : Ty B} (h₀ : s₀.Draws S.fresh.2 S') (hne : s₀.domS ≠ [])
    (hβ : (.row, β) ∉ s₀.domS) :
    (s₀.comp ⟨[], [(β, .cat (.sing l τ) (.var S.fresh.1))], []⟩).Draws S S' := by
  have hn := nv_add_le
    (X := (⟨[], [(β, .cat (.sing l τ) (.var S.fresh.1))], []⟩ : Sol B).domS) (Y := s₀.domS)
    (fun x hx => by
      have : x = (.row, β) := by simpa [Sol.domS] using hx
      subst this; exact hβ)
    (fun _ hx => Sol.mem_domS_comp_left hx) (fun _ hx => Sol.mem_domS_comp_right hx)
  have h1 : nv (⟨[], [(β, .cat (.sing l τ) (.var S.fresh.1))], []⟩ : Sol B).domS = 1 := by
    simp [Sol.domS, nv, dd]
  rcases h₀ with h₀ | ⟨e, -⟩
  · exact .inl (by simp only [Supply.fresh] at h₀; omega)
  · exact absurd e hne

------------------------- ARMS THAT DRAW NOTHING --------------------------------

theorem bindTy_supply_eq {B : Type} {S : Supply} {α : TyVar} {τ : Ty B} {s : Sol B}
    {S' : Supply} (h : bindTy S α τ = .success s S') : S' = S := by
  unfold bindTy at h
  split at h
  · simp only [UResM.success.injEq] at h; exact h.2.symm
  · split at h
    · cases h
    · simp only [UResM.success.injEq] at h; exact h.2.symm

theorem solveVarM_supply_eq {B : Type} {S : Supply} {u₁ u₂ : List (Atom B)} {s : Sol B}
    {S' : Supply} (h : solveVarM S u₁ u₂ = some (.success s S')) : S' = S := by
  match u₁ with
  | [] => simp [solveVarM] at h
  | .field _ _ :: _ => simp [solveVarM] at h
  | .dfield _ _ :: _ => simp [solveVarM] at h
  | [.var α] =>
      simp only [solveVarM] at h
      split at h
      · simp only [Option.some.injEq, UResM.success.injEq] at h; exact h.2.symm
      · split at h
        · split at h <;> (try split at h) <;> simp at h
        · simp only [Option.some.injEq, UResM.success.injEq] at h; exact h.2.symm
  | .var _ :: _ :: _ => simp [solveVarM] at h

theorem unifyTyF_flat_supply {B : Type} [DecidableEq B] (fuel : Nat) (S : Supply)
    (τ τ' : Ty B) {s : Sol B} {S' : Supply} (hrec : tyRec τ τ' = false)
    (h : unifyTyF S fuel τ τ' = .success s S') : S' = S := by
  cases τ <;> cases τ' <;> (try simp [tyRec] at hrec) <;> cases fuel <;> first
    | exact bindTy_supply_eq h
    | exact unifyKey_supply h
    | (simp only [unifyTyF, UResM.success.injEq] at h; exact h.2.symm)
    | (simp only [unifyTyF] at h
       split at h <;> first
         | (simp only [UResM.success.injEq] at h; exact h.2.symm)
         | cases h)
    | cases h

------------------------- U-HOST'S RESIDUAL BINDS SOMETHING ---------------------

theorem Sol.Sat_of_domS_nil {B : Type} {θ : TySubst B} {s : Sol B} (h : s.domS = []) :
    Sol.Sat θ s := by
  simp only [Sol.domS, List.append_eq_nil_iff, List.map_eq_nil_iff] at h
  obtain ⟨⟨h₁, h₂⟩, h₃⟩ := h
  exact ⟨(fun p hp => by rw [h₁] at hp; cases hp), (fun p hp => by rw [h₂] at hp; cases hp),
         (fun p hp => by rw [h₃] at hp; cases hp)⟩

-- ⊢  a keyless success leaves both sides as they were, so they are ≈ and have
--    the same variables
theorem keyless_vars {B : Type} [DecidableEq B] {fuel : Nat} {S : Supply}
    {x y : List (Atom B)} {s : Sol B} {S' : Supply}
    (h : unifySpineMF S fuel x y = .success s S') (hd : s.domS = []) :
    sVarSeq x = sVarSeq y := by
  have key := (unifyM_success_sound (θ := TySubst.id B) fuel).2 S x y h
    (Sol.Sat_of_domS_nil hd)
  rw [Row.applySubst_id, Row.applySubst_id] at key
  have hv := (RowEquiv.char key).vars
  rwa [ofSpine_toSpine, ofSpine_toSpine] at hv

theorem mem_sVarSeq_renameVar {B : Type} {β β' : TyVar} :
    (s : List (Atom B)) → β ∈ sVarSeq s → β' ∈ sVarSeq (renameVar β β' s)
  | [], h => by simp [sVarSeq] at h
  | .var γ :: s, h => by
      by_cases hg : γ = β
      · simp [renameVar, hg, sVarSeq]
      · simp only [sVarSeq, List.mem_cons] at h
        rcases h with h | h
        · exact absurd h.symm hg
        · simp only [renameVar, if_neg hg, sVarSeq, List.mem_cons]
          exact .inr (mem_sVarSeq_renameVar s h)
  | .field _ _ :: s, h => by
      simp only [sVarSeq, renameVar] at h ⊢; exact mem_sVarSeq_renameVar s h
  | .dfield _ _ :: s, h => by
      simp only [sVarSeq, renameVar] at h ⊢; exact mem_sVarSeq_renameVar s h

-- ⊢  U-HOST'S CASE, either orientation (the residual call is on (x, y))
theorem host_draws {B : Type} [DecidableEq B] {fuel : Nat}
    (ih : ∀ (S : Supply) (u₁ u₂ : List (Atom B)) {s : Sol B} {S' : Supply},
      Below S (sSorted u₁ ++ sSorted u₂) → unifySpineMF S fuel u₁ u₂ = .success s S' →
      s.Draws S S')
    {S : Supply} {u₁ u₂ : List (Atom B)} {β : TyVar} {l : Label} {τ : Ty B}
    {t₁ t₂ x y : List (Atom B)} {s : Sol B} {S' : Supply}
    (hB : Below S (sSorted u₁ ++ sSorted u₂))
    (he : hostL S u₁ u₂ = some (β, l, τ, t₁, t₂))
    (hxy : (x = t₁ ∧ y = t₂) ∨ (x = t₂ ∧ y = t₁))
    (h : hostResM S β l τ (unifySpineMF S.fresh.2 fuel x y) = .success s S') :
    s.Draws S S' := by
  obtain ⟨s₀, hr, rfl⟩ := hostResM_success h
  obtain ⟨hs1, ⟨⟨rest, hvv, -⟩, -⟩, -, -, hren⟩ := hostL_spec he
  obtain ⟨hβV, -, hres⟩ := host_residual hB he
  have hres' : ∀ z ∈ sSorted x ++ sSorted y,
      z ∈ (sSorted u₁ ++ sSorted u₂) ++ [(.row, S.fresh.1)] ∧ z ≠ (.row, β) := by
    rcases hxy with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
    · exact hres
    · exact fun z hz => hres z (append_sub_swap (fun _ h => h) hz)
  have hS₁ : S.next ≤ S.fresh.2.next := Nat.le_succ _
  have hf₁ : FreshIn S S.fresh.2 [(.row, S.fresh.1)] := by
    intro z hz
    obtain rfl := List.mem_singleton.mp hz
    simp only [Supply.fresh, natName_length]
    exact ⟨Nat.le_refl _, Nat.lt_succ_self _, trivial⟩
  have hB₀ := Below.residual hB hS₁ hf₁ (fun z hz => (hres' z hz).1)
  -- the residual binds something: β′ sits on the host side only
  have hne : s₀.domS ≠ [] := fun hd => by
    have hv := keyless_vars hr hd
    have h₂ : S.fresh.1 ∈ sVarSeq t₂ := by
      rw [hren]; exact mem_sVarSeq_renameVar u₂ (by rw [hvv]; exact List.mem_cons_self)
    have h₁ : S.fresh.1 ∉ sVarSeq t₁ := fun hm => by
      have := hB _ (List.mem_append_left _
        (by rw [hs1]; exact List.mem_append_right _ (sVarSeq_mem_sSorted t₁ hm)))
      simp only [Supply.fresh, natName_length] at this
      exact Nat.lt_irrefl _ this
    rcases hxy with ⟨rfl, rfl⟩ | ⟨rfl, rfl⟩
    · exact h₁ (by rw [hv]; exact h₂)
    · exact h₁ (by rw [← hv]; exact h₂)
  -- …and not β, which the residual and the later draws both avoid
  have hβ : (.row, β) ∉ s₀.domS := fun hd => by
    obtain ⟨R₀, hf₀, g₀⟩ := (unifyM_good fuel).2 _ _ _ hB₀ hr
    rcases List.mem_append.mp (g₀.dom _ hd) with hm | hm
    · exact (hres' _ hm).2 rfl
    · have lo : S.next + 1 ≤ β.length := (hf₀ _ hm).1
      have hi : β.length < S.next := hB _ hβV
      omega
  exact Sol.Draws.host (ih _ _ _ hB₀ hr) hne hβ

------------------------- THE INDUCTION ----------------------------------------

-- ⊢  a staged composite's two stages bind disjoint keys
theorem stage_disjoint {B : Type} {S S₁ S' : Supply} {Q Res R₁ R₂ : List (Srt × TyVar)}
    {s₁ s₂ : Sol B} (hB : Below S Q) (hS₁ : S.next ≤ S₁.next)
    (hf₁ : FreshIn S S₁ R₁) (hf₂ : FreshIn S₁ S' R₂)
    (g₁ : s₁.Good (Q ++ R₁)) (g₂ : s₂.Good (Res ++ R₂))
    (hR : ∀ x ∈ Res, x ∈ Q ++ R₁ ∧ x ∉ s₁.domS) : ∀ x ∈ s₁.domS, x ∉ s₂.domS := by
  intro x hx₁ hx₂
  rcases List.mem_append.mp (g₂.dom x hx₂) with h | h
  · exact (hR x h).2 hx₁
  · have lo := (hf₂ x h).1
    rcases List.mem_append.mp (g₁.dom x hx₁) with h' | h'
    · have := hB x h'; omega
    · have := (hf₁ x h').2.1; omega

-- ⊢  EVERY SUCCESS DRAWS FEWER NAMES THAN IT BINDS KEYS, or binds and draws
--    nothing — from a supply above the problem, at both sorts
theorem unifyM_draws {B : Type} [DecidableEq B] (fuel : Nat) :
    (∀ (S : Supply) (τ τ' : Ty B) {s : Sol B} {S' : Supply},
        Below S (Ty.sortedFtv τ ++ Ty.sortedFtv τ') →
        unifyTyF S fuel τ τ' = .success s S' → s.Draws S S') ∧
    (∀ (S : Supply) (s₁ s₂ : List (Atom B)) {s : Sol B} {S' : Supply},
        Below S (sSorted s₁ ++ sSorted s₂) →
        unifySpineMF S fuel s₁ s₂ = .success s S' → s.Draws S S') := by
  have hnilL : ∀ (S : Supply) (s₂ : List (Atom B)) (fuel : Nat) {s : Sol B} {S' : Supply},
      unifySpineMF S fuel [] s₂ = .success s S' → s.Draws S S' := by
    intro S s₂ fuel s S' h
    simp only [unifySpineMF] at h
    cases hae : allVarsEmpty s₂ with
    | none => simp [hae] at h
    | some σ' =>
        simp only [hae, UResM.success.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact Sol.Draws.same _ _
  have hnilR : ∀ (S : Supply) (a : Atom B) (s₁ : List (Atom B)) (fuel : Nat) {s : Sol B}
      {S' : Supply}, unifySpineMF S fuel (a :: s₁) [] = .success s S' → s.Draws S S' := by
    intro S a s₁ fuel s S' h
    simp only [unifySpineMF] at h
    cases hae : allVarsEmpty (a :: s₁) with
    | none => simp [hae] at h
    | some σ' =>
        simp only [hae, UResM.success.injEq] at h
        obtain ⟨rfl, rfl⟩ := h
        exact Sol.Draws.same _ _
  induction fuel with
  | zero =>
      refine ⟨fun S τ τ' s S' _ h => ?_, fun S s₁ s₂ s S' _ h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => rw [unifyTyF_flat_supply 0 S τ τ' hrec h]; exact Sol.Draws.same _ _
        | true =>
          rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
          · cases h
          · cases h
      · cases s₁ with
        | nil => exact hnilL S s₂ 0 h
        | cons a s₁ =>
          cases s₂ with
          | nil => exact hnilR S a s₁ 0 h
          | cons b s₂ => cases h
  | succ fuel ih =>
      -- an eq-emitting arm: the first stage under `Q`, the residual under its
      -- solution, keys disjoint by `stage_disjoint`
      have arm : ∀ (S : Supply) (τ τ' : Ty B) (t₁ t₂ : List (Atom B))
          (Q : List (Srt × TyVar)) {s : Sol B} {S' : Supply},
          Ty.sortedFtv τ ++ Ty.sortedFtv τ' ⊆ Q → sSorted t₁ ++ sSorted t₂ ⊆ Q →
          Below S Q →
          ((unifyTyF S fuel τ τ').seq fun θ' S'' =>
              unifySpineMF S'' fuel (sApplySubst θ' t₁) (sApplySubst θ' t₂))
            = .success s S' → s.Draws S S' := by
        intro S τ τ' t₁ t₂ Q s S' hQt hQr hB h
        obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
        have hS₁ := (unifyM_supply_mono fuel).1 _ _ _ hty
        have hB₁ : Below S (Ty.sortedFtv τ ++ Ty.sortedFtv τ') := fun x hx => hB x (hQt hx)
        obtain ⟨R₁, hf₁, g₁⟩ := (unifyM_good fuel).1 S τ τ' hB₁ hty
        have g₁' := g₁.freshMono hQt
        have hR : ∀ x ∈ sSorted (sApplySubst s₁.toSubst t₁) ++
            sSorted (sApplySubst s₁.toSubst t₂), x ∈ Q ++ R₁ ∧ x ∉ s₁.domS := fun x hx => by
          rcases List.mem_append.mp hx with hx | hx
          · exact g₁'.clears_spine (fun _ hy => List.mem_append_left _
              (hQr (List.mem_append_left _ hy))) hx
          · exact g₁'.clears_spine (fun _ hy => List.mem_append_left _
              (hQr (List.mem_append_right _ hy))) hx
        have hBr := Below.residual hB hS₁ hf₁ (fun x hx => (hR x hx).1)
        obtain ⟨R₂, hf₂, g₂⟩ := (unifyM_good fuel).2 S₁ _ _ hBr hrow
        exact (ih.1 S τ τ' hB₁ hty).comp (ih.2 S₁ _ _ hBr hrow)
          (stage_disjoint hB hS₁ hf₁ hf₂ g₁' g₂ hR)
      refine ⟨fun S τ τ' s S' hB h => ?_, fun S s₁ s₂ s S' hB h => ?_⟩
      · cases hrec : tyRec τ τ' with
        | false => rw [unifyTyF_flat_supply (fuel + 1) S τ τ' hrec h]; exact Sol.Draws.same _ _
        | true =>
        rcases tyRec_true hrec with ⟨a₁, b₁, a₂, b₂, rfl, rfl⟩ | ⟨ρ₁, ρ₂, rfl, rfl⟩
        · replace h : ((unifyTyF S fuel a₁ a₂).seq fun θ' S'' =>
              unifyTyF S'' fuel (b₁.applySubst θ') (b₂.applySubst θ'))
            = .success s S' := h
          obtain ⟨s₁, S₁, s₂, hty, hrow, rfl⟩ := UResM.seq_success h
          have hS₁ := (unifyM_supply_mono fuel).1 _ _ _ hty
          have hQ : Ty.sortedFtv a₁ ++ Ty.sortedFtv a₂ ⊆
              Ty.sortedFtv (.fn a₁ b₁) ++ Ty.sortedFtv (.fn a₂ b₂) := fun x hx => by
            simp only [Ty.sortedFtv, List.mem_append] at hx ⊢
            rcases hx with hx | hx
            · exact .inl (.inl hx)
            · exact .inr (.inl hx)
          have hB₁ : Below S (Ty.sortedFtv a₁ ++ Ty.sortedFtv a₂) := fun x hx => hB x (hQ hx)
          obtain ⟨R₁, hf₁, g₁⟩ := (unifyM_good fuel).1 S a₁ a₂ hB₁ hty
          have g₁' := g₁.freshMono hQ
          have hR : ∀ x ∈ Ty.sortedFtv (b₁.applySubst s₁.toSubst) ++
              Ty.sortedFtv (b₂.applySubst s₁.toSubst),
              x ∈ Ty.sortedFtv (.fn a₁ b₁) ++ Ty.sortedFtv (.fn a₂ b₂) ++ R₁ ∧
              x ∉ s₁.domS := fun x hx => by
            rcases List.mem_append.mp hx with hx | hx
            · exact g₁'.clears_ty (fun y hy => List.mem_append_left _ (by
                simp only [Ty.sortedFtv, List.mem_append]; exact .inl (.inr hy))) hx
            · exact g₁'.clears_ty (fun y hy => List.mem_append_left _ (by
                simp only [Ty.sortedFtv, List.mem_append]; exact .inr (.inr hy))) hx
          have hBr := Below.residual hB hS₁ hf₁ (fun x hx => (hR x hx).1)
          obtain ⟨R₂, hf₂, g₂⟩ := (unifyM_good fuel).1 S₁ _ _ hBr hrow
          exact (ih.1 S a₁ a₂ hB₁ hty).comp (ih.1 S₁ _ _ hBr hrow)
            (stage_disjoint hB hS₁ hf₁ hf₂ g₁' g₂ hR)
        · replace h : unifySpineMF S fuel ρ₁.toSpine ρ₂.toSpine = .success s S' := h
          have hsub : sSorted ρ₁.toSpine ++ sSorted ρ₂.toSpine ⊆
              Ty.sortedFtv (.rcd ρ₁) ++ Ty.sortedFtv (.rcd ρ₂) := fun x hx => by
            rcases List.mem_append.mp hx with hx | hx
            · exact List.mem_append_left _ (sSorted_toSpine _ _ hx)
            · exact List.mem_append_right _ (sSorted_toSpine _ _ hx)
          exact ih.2 S _ _ (fun x hx => hB x (hsub hx)) h
      · cases s₁ with
        | nil => exact hnilL S s₂ _ h
        | cons a s₁ =>
          cases s₂ with
          | nil => exact hnilR S a s₁ _ h
          | cons b s₂ =>
            unfold unifySpineMF at h
            cases hsl : stripL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl] at h
              have hsub := sSorted_sub_pair (stripL_atoms hsl).1 (stripL_atoms hsl).2
              exact ih.2 S t₁ t₂ (fun x hx => hB x (hsub hx)) h
            | none =>
            cases hsr : stripR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨t₁, t₂⟩ := p; simp only [hsl, hsr] at h
              have hsub := sSorted_sub_pair (stripR_atoms hsr).1 (stripR_atoms hsr).2
              exact ih.2 S t₁ t₂ (fun x hx => hB x (hsub hx)) h
            | none =>
            cases hv1 : solveVarM S (a :: s₁) (b :: s₂) with
            | some r =>
              simp only [hsl, hsr, hv1] at h
              rw [solveVarM_supply_eq (hv1.trans (congrArg some h))]
              exact Sol.Draws.same _ _
            | none =>
            cases hv2 : solveVarM S (b :: s₂) (a :: s₁) with
            | some r =>
              simp only [hsl, hsr, hv1, hv2] at h
              rw [solveVarM_supply_eq (hv2.trans (congrArg some h))]
              exact Sol.Draws.same _ _
            | none =>
            cases hml : matchL (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p; simp only [hsl, hsr, hv1, hv2, hml] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchL_atoms hml).ty_sub
                (matchL_atoms hml).res_sub hB h
            | none =>
            cases hml2 : matchL (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p; simp only [hsl, hsr, hv1, hv2, hml, hml2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchL_atoms hml2).swap.ty_sub
                (matchL_atoms hml2).swap.res_sub hB h
            | none =>
            cases hmr : matchR (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchR_atoms hmr).ty_sub
                (matchR_atoms hmr).res_sub hB h
            | none =>
            cases hmr2 : matchR (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (matchR_atoms hmr2).swap.ty_sub
                (matchR_atoms hmr2).swap.res_sub hB h
            | none =>
            cases hg : groundMatch (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨τ0, τ0', t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg] at h
              exact arm S τ0 τ0' t₁ t₂ _ (groundMatch_atoms hg).ty_sub
                (groundMatch_atoms hg).res_sub hB h
            | none =>
            cases hg2 : groundMatch (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨τ0', τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2] at h
              exact arm S τ0 τ0' t₁ t₂ _ (groundMatch_atoms hg2).swap.ty_sub
                (groundMatch_atoms hg2).swap.res_sub hB h
            | none =>
            cases hpc : projClash (a :: s₁) (b :: s₂) with
            | true =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc] at h
              cases h
            | false =>
            cases hh1 : hostL S (a :: s₁) (b :: s₂) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₁, t₂⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1,
                Bool.false_eq_true, ite_false] at h
              exact host_draws ih.2 hB hh1 (.inl ⟨rfl, rfl⟩) h
            | none =>
            cases hh2 : hostL S (b :: s₂) (a :: s₁) with
            | some p =>
              obtain ⟨β0, l0, τ0, t₂, t₁⟩ := p
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1, hh2,
                Bool.false_eq_true, ite_false] at h
              exact host_draws ih.2 (fun x hx => hB x (append_sub_swap (fun _ h => h) hx))
                hh2 (.inr ⟨rfl, rfl⟩) h
            | none =>
              simp only [hsl, hsr, hv1, hv2, hml, hml2, hmr, hmr2, hg, hg2, hpc, hh1, hh2,
                Bool.false_eq_true, ite_false] at h
              cases h

end MinimalCalculus
