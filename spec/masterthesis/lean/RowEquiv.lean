-- The ≈-characterization: rows mod ≈-assoc/units as spines (Atom lists),
-- the trace-monoid normal form (toSpine/ofSpine, sVarSeq, sProj), the
-- characterization rowEquiv_iff_char, and full *cancellativity*. The algebraic
-- foundation the row-unification algorithm (RowUnify) consumes.

import minimal

namespace MinimalCalculus

------------------------- THE ≈-CHARACTERIZATION ------------------------------
-- Rows mod ≈-assoc/units are SPINES (lists of atoms a := l: τ | α);
-- a spine factors into segments (var-free runs)
-- separated by vars. ≈-comm swaps adjacent DISTINCT labels only, so within a
-- segment distinct labels commute freely while equal labels keep their
-- relative order, and nothing crosses a var. Characterization:
--
--   ρ₁ ≈ ρ₂   iff   same var sequence and ∀ l, the l-projections agree
--                   pointwise (equal segment index, ≈-equivalent types)
--
-- The l-projection records, for every l-field in row order, its segment index
-- (= number of vars strictly before it) and its type. This is the
-- trace-monoid presentation; cancellativity of shared end-vars — what makes
-- U-var-refl sound AND complete — falls out as a corollary.

-- FC-labels phase B adds a third atom, the KEYED field ${α}: τ. A literal key
-- IS the literal field l (≈-rule `dsingLab`), so only a label-variable key
-- makes a keyed atom. A keyed field is a BARRIER like a var: nothing commutes
-- past it.
-- "var sequence" below reads "barrier sequence" (vars and keys, in order), a
-- segment is a barrier-free run, and the keyed fields' TYPES are a third
-- invariant (pointwise ≈, in order).
inductive Atom (B : Type) : Type where
  | field  : Label → Ty B → Atom B
  | var    : TyVar → Atom B
  | dfield : TyVar → Ty B → Atom B

/-- the spine atom of a keyed field. -/
def Atom.ofKey {B : Type} : Key → Ty B → Atom B
  | .lit l, τ => .field l τ
  | .var α, τ => .dfield α τ

def Row.toSpine {B : Type} : Row B → List (Atom B)
  | .empty     => []
  | .var α     => [.var α]
  | .sing l τ  => [.field l τ]
  | .cat ρ₁ ρ₂ => ρ₁.toSpine ++ ρ₂.toSpine
  | .dsing q τ => [Atom.ofKey q τ]

-- Fold a spine back into a right-nested row (the ≈-normal shape).
def ofSpine {B : Type} : List (Atom B) → Row B
  | []               => .empty
  | .field l τ :: s  => .cat (.sing l τ) (ofSpine s)
  | .var α :: s      => .cat (.var α) (ofSpine s)
  | .dfield α τ :: s => .cat (.dsing (.var α) τ) (ofSpine s)

-- A barrier's identity: a var, or a key (its field type is tracked apart).
inductive Barrier (B : Type) : Type where
  | var : TyVar → Barrier B
  | key : TyVar → Barrier B

-- Invariant 1: the barrier sequence.
def sBarSeq {B : Type} : List (Atom B) → List (Barrier B)
  | []               => []
  | .field _ _ :: s  => sBarSeq s
  | .var α :: s      => .var α :: sBarSeq s
  | .dfield o _ :: s => .key o :: sBarSeq s

-- The var sequence (keys skipped) — what the unifier's var bookkeeping reads.
def sVarSeq {B : Type} : List (Atom B) → List TyVar
  | []               => []
  | .field _ _ :: s  => sVarSeq s
  | .var α :: s      => α :: sVarSeq s
  | .dfield _ _ :: s => sVarSeq s

-- Invariant 2: the l-projection — (segment index, type) per l-field, in order.
def sProj {B : Type} (l : Label) : List (Atom B) → List (Nat × Ty B)
  | []               => []
  | .field l' τ :: s => if l' = l then (0, τ) :: sProj l s else sProj l s
  | .var _ :: s      => (sProj l s).map (fun p => (p.1 + 1, p.2))
  | .dfield _ _ :: s => (sProj l s).map (fun p => (p.1 + 1, p.2))

-- Invariant 3: the keyed fields' types, in order.
def sDProj {B : Type} : List (Atom B) → List (Nat × Ty B)
  | []               => []
  | .field _ _ :: s  => sDProj s
  | .var _ :: s      => sDProj s
  | .dfield _ τ :: s => (0, τ) :: sDProj s

-- ⊢  vars(s₁ ++ s₂) = vars(s₁) ++ vars(s₂)
theorem sVarSeq_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    sVarSeq (s₁ ++ s₂) = sVarSeq s₁ ++ sVarSeq s₂
  | [], _ => rfl
  | .field _ _ :: s₁, s₂ => sVarSeq_append s₁ s₂
  | .var _ :: s₁, s₂ => congrArg (_ :: ·) (sVarSeq_append s₁ s₂)
  | .dfield _ _ :: s₁, s₂ => sVarSeq_append s₁ s₂

-- ⊢  bars(s₁ ++ s₂) = bars(s₁) ++ bars(s₂)
theorem sBarSeq_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    sBarSeq (s₁ ++ s₂) = sBarSeq s₁ ++ sBarSeq s₂
  | [], _ => rfl
  | .field _ _ :: s₁, s₂ => sBarSeq_append s₁ s₂
  | .var _ :: s₁, s₂ => congrArg (_ :: ·) (sBarSeq_append s₁ s₂)
  | .dfield _ _ :: s₁, s₂ => congrArg (_ :: ·) (sBarSeq_append s₁ s₂)

theorem sDProj_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    sDProj (s₁ ++ s₂) = sDProj s₁ ++ sDProj s₂
  | [], _ => rfl
  | .field _ _ :: s₁, s₂ => sDProj_append s₁ s₂
  | .var _ :: s₁, s₂ => sDProj_append s₁ s₂
  | .dfield _ _ :: s₁, s₂ => congrArg (_ :: ·) (sDProj_append s₁ s₂)

-- The var sequence is read off the barrier sequence.
def Barrier.vars {B : Type} : List (Barrier B) → List TyVar
  | [] => []
  | .var α :: bs => α :: Barrier.vars bs
  | .key _ :: bs => Barrier.vars bs

theorem sVarSeq_eq_vars {B : Type} : (s : List (Atom B)) →
    sVarSeq s = Barrier.vars (sBarSeq s)
  | [] => rfl
  | .field _ _ :: s => sVarSeq_eq_vars s
  | .var _ :: s => congrArg (_ :: ·) (sVarSeq_eq_vars s)
  | .dfield _ _ :: s => sVarSeq_eq_vars s

-- ⊢  proj_l(s₁ ++ s₂) = proj_l(s₁) ++ map(·+|bars s₁|) proj_l(s₂)
theorem sProj_append {B : Type} (l : Label) : (s₁ s₂ : List (Atom B)) →
    sProj l (s₁ ++ s₂) =
      sProj l s₁ ++ (sProj l s₂).map (fun p => (p.1 + (sBarSeq s₁).length, p.2))
  | [], s₂ => by simp [sProj, sBarSeq]
  | .field l' τ :: s₁, s₂ => by
      by_cases h : l' = l <;>
        simp [sProj, sBarSeq, h, sProj_append l s₁ s₂]
  | .var _ :: s₁, s₂ => by
      simp [sProj, sBarSeq, sProj_append l s₁ s₂, List.map_map,
            Function.comp, Nat.add_assoc]
  | .dfield _ _ :: s₁, s₂ => by
      simp [sProj, sBarSeq, sProj_append l s₁ s₂, List.map_map,
            Function.comp, Nat.add_assoc]

-- Pointwise equivalence of projections: equal segment indices, ≈-equal types.
inductive ProjEquiv {B : Type} : List (Nat × Ty B) → List (Nat × Ty B) → Prop where
  | nil  : ProjEquiv [] []
  | cons : n₁ = n₂ → TyEquiv τ₁ τ₂ → ProjEquiv ps qs →
           ProjEquiv ((n₁, τ₁) :: ps) ((n₂, τ₂) :: qs)

-- ⊢  ps ≈ₚ ps
theorem ProjEquiv.refl {B : Type} : (ps : List (Nat × Ty B)) → ProjEquiv ps ps
  | [] => .nil
  | (_, τ) :: ps => .cons rfl (.refl τ) (ProjEquiv.refl ps)

-- ⊢  ps = qs   ⟹   ps ≈ₚ qs
theorem ProjEquiv.of_eq {B : Type} {ps qs : List (Nat × Ty B)} (h : ps = qs) :
    ProjEquiv ps qs := h ▸ ProjEquiv.refl ps

-- ⊢  ps ≈ₚ qs   ⟹   qs ≈ₚ ps
theorem ProjEquiv.symm {B : Type} {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv qs ps
  | .nil => .nil
  | .cons hn hty h => .cons hn.symm hty.symm h.symm

-- ⊢  ps ≈ₚ qs,  qs ≈ₚ rs   ⟹   ps ≈ₚ rs
theorem ProjEquiv.trans {B : Type} {ps qs rs : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv qs rs → ProjEquiv ps rs
  | .nil, h => h
  | .cons hn hty h, .cons hn' hty' h' =>
      .cons (hn.trans hn') (hty.trans hty') (h.trans h')

-- ⊢  ps ≈ₚ qs,  ps' ≈ₚ qs'   ⟹   (ps ++ ps') ≈ₚ (qs ++ qs')
theorem ProjEquiv.append {B : Type} {ps qs ps' qs' : List (Nat × Ty B)} :
    ProjEquiv ps qs → ProjEquiv ps' qs' → ProjEquiv (ps ++ ps') (qs ++ qs')
  | .nil, h => h
  | .cons hn hty h, h' => .cons hn hty (h.append h')

-- ⊢  ps ≈ₚ qs   ⟹   map(·+k) ps ≈ₚ map(·+k) qs
theorem ProjEquiv.mapShift {B : Type} (k : Nat) {ps qs : List (Nat × Ty B)} :
    ProjEquiv ps qs →
    ProjEquiv (ps.map (fun p => (p.1 + k, p.2))) (qs.map (fun p => (p.1 + k, p.2)))
  | .nil => .nil
  | .cons hn hty h => .cons (by rw [hn]) hty (ProjEquiv.mapShift k h)

-- ⊢  [] ≈ₚ qs   ⟹   qs = []
theorem ProjEquiv.nil_inv {B : Type} {qs : List (Nat × Ty B)}
    (h : ProjEquiv [] qs) : qs = [] := by cases h; rfl

-- ⊢  (n,τ)::ps ≈ₚ qs
--        ⟹  ∃ τ' rest. qs = (n,τ')::rest ∧ τ ≈ τ' ∧ ps ≈ₚ rest
theorem ProjEquiv.cons_inv {B : Type} {n : Nat} {τ : Ty B}
    {ps qs : List (Nat × Ty B)} (h : ProjEquiv ((n, τ) :: ps) qs) :
    ∃ τ' rest, qs = (n, τ') :: rest ∧ TyEquiv τ τ' ∧ ProjEquiv ps rest := by
  cases h with
  | cons hn hty h' => exact ⟨_, _, by rw [← hn], hty, h'⟩

-- Un-shifting: the segment indices are injective in +1 (used to strip a
-- leading var off both sides).
-- ⊢  map(·+1) ps ≈ₚ map(·+1) qs   ⟹   ps ≈ₚ qs
theorem ProjEquiv.unshift {B : Type} :
    {ps qs : List (Nat × Ty B)} →
    ProjEquiv (ps.map (fun p => (p.1 + 1, p.2))) (qs.map (fun p => (p.1 + 1, p.2))) →
    ProjEquiv ps qs
  | [], [], _ => .nil
  | [], _ :: _, h => nomatch h
  | _ :: _, [], h => nomatch h
  | _ :: _, _ :: _, h => by
      cases h with
      | cons hn hty h' =>
          exact .cons (Nat.add_right_cancel hn) hty (ProjEquiv.unshift h')

-- A shifted projection never starts at segment index 0 (a spine that leads
-- with a var has an empty first segment).
-- ⊢  ¬ ( map(·+1) ps ≈ₚ (0,τ)::qs )
theorem ProjEquiv.no_zero_head {B : Type} {qs : List (Nat × Ty B)} {τ : Ty B} :
    (ps : List (Nat × Ty B)) →
    ¬ ProjEquiv (ps.map (fun p => (p.1 + 1, p.2))) ((0, τ) :: qs)
  | [], h => nomatch h
  | _ :: _, h => by
      cases h with
      | cons hn _ _ => exact Nat.succ_ne_zero _ hn

-- ⊢  map(·+1) ps = (0,τ)::qs   ⟹   False
theorem map_shift_ne_zero_head {B : Type} {ps qs : List (Nat × Ty B)} {τ : Ty B}
    (h : ps.map (fun p => (p.1 + 1, p.2)) = (0, τ) :: qs) : False := by
  cases ps with
  | nil => exact nomatch h
  | cons p ps =>
      injection h with hhead _
      injection hhead with h0 _
      exact Nat.succ_ne_zero _ h0

-- ## The characterization predicate
def Row.Char {B : Type} (ρ₁ ρ₂ : Row B) : Prop :=
  sBarSeq ρ₁.toSpine = sBarSeq ρ₂.toSpine ∧
  (∀ l, ProjEquiv (sProj l ρ₁.toSpine) (sProj l ρ₂.toSpine)) ∧
  ProjEquiv (sDProj ρ₁.toSpine) (sDProj ρ₂.toSpine)

-- ⊢  spine ρ₁ = spine ρ₂   ⟹   Char(ρ₁, ρ₂)
theorem Row.Char.of_eq {B : Type} {ρ₁ ρ₂ : Row B}
    (h : ρ₁.toSpine = ρ₂.toSpine) : Row.Char ρ₁ ρ₂ :=
  ⟨by rw [h], fun _ => .of_eq (by rw [h]), .of_eq (by rw [h])⟩

-- the var sequences agree, too
theorem Row.Char.vars {B : Type} {ρ₁ ρ₂ : Row B} (h : Row.Char ρ₁ ρ₂) :
    sVarSeq ρ₁.toSpine = sVarSeq ρ₂.toSpine := by
  rw [sVarSeq_eq_vars, sVarSeq_eq_vars, h.1]

-- ## Soundness: ≈ preserves both invariants
-- Every ≈-axiom is invariant-preserving: assoc/units don't move atoms
-- (spines are EQUAL), comm swaps distinct labels (all projections are equal
-- lists), congruence is pointwise, and no rule crosses a var.
-- ⊢  ρ₁ ≈ᵣ ρ₂   ⟹   Char(ρ₁, ρ₂)      (≈ soundness)
theorem RowEquiv.char {B : Type} : {ρ₁ ρ₂ : Row B} → ρ₁ ≈ᵣ ρ₂ → Row.Char ρ₁ ρ₂
  | _, _, .refl _ => ⟨rfl, fun _ => .refl _, .refl _⟩
  | _, _, .symm h =>
      ⟨(RowEquiv.char h).1.symm, fun l => ((RowEquiv.char h).2.1 l).symm,
       (RowEquiv.char h).2.2.symm⟩
  | _, _, .trans h₁ h₂ =>
      ⟨(RowEquiv.char h₁).1.trans (RowEquiv.char h₂).1,
       fun l => ((RowEquiv.char h₁).2.1 l).trans ((RowEquiv.char h₂).2.1 l),
       (RowEquiv.char h₁).2.2.trans (RowEquiv.char h₂).2.2⟩
  | .sing l τ₁, .sing _ τ₂, .sing hty =>
      ⟨rfl, fun l' => by
        by_cases h : l = l'
        · simp only [Row.toSpine, sProj, if_pos h]
          exact .cons rfl hty .nil
        · simp only [Row.toSpine, sProj, if_neg h]
          exact .nil, .nil⟩
  | .cat ρ₁ ρ₂, .cat ρ₁' ρ₂', .cat h₁ h₂ => by
      obtain ⟨hv₁, hp₁, hd₁⟩ := RowEquiv.char h₁
      obtain ⟨hv₂, hp₂, hd₂⟩ := RowEquiv.char h₂
      refine ⟨?_, fun l => ?_, ?_⟩
      · simp only [Row.toSpine, sBarSeq_append, hv₁, hv₂]
      · simp only [Row.toSpine, sProj_append, hv₁]
        exact (hp₁ l).append ((hp₂ l).mapShift _)
      · simp only [Row.toSpine, sDProj_append]
        exact hd₁.append hd₂
  | _, _, .assoc => Row.Char.of_eq (List.append_assoc _ _ _)
  | _, _, .unitL => Row.Char.of_eq (List.nil_append _)
  | _, _, .unitR => Row.Char.of_eq (List.append_nil _)
  | .cat (.sing l₁ τ₁) (.sing l₂ τ₂), _, .comm hne =>
      ⟨rfl, fun l => by
        by_cases h₁ : l₁ = l <;> by_cases h₂ : l₂ = l
        · exact absurd (h₁.trans h₂.symm) hne
        · simp only [Row.toSpine, List.cons_append, List.nil_append, sProj,
                     if_pos h₁, if_neg h₂]
          exact .refl _
        · simp only [Row.toSpine, List.cons_append, List.nil_append, sProj,
                     if_neg h₁, if_pos h₂]
          exact .refl _
        · simp only [Row.toSpine, List.cons_append, List.nil_append, sProj,
                     if_neg h₁, if_neg h₂]
          exact .nil, .nil⟩
  | .dsing q τ₁, .dsing _ τ₂, .dsing hty => by
      cases q with
      | var _ => exact ⟨rfl, fun _ => .nil, .cons rfl hty .nil⟩
      | lit l =>
          refine ⟨rfl, fun l' => ?_, .nil⟩
          by_cases h : l = l'
          · simp only [Row.toSpine, Atom.ofKey, sProj, if_pos h]
            exact .cons rfl hty .nil
          · simp only [Row.toSpine, Atom.ofKey, sProj, if_neg h]
            exact .nil
  | _, _, .dsingLab => Row.Char.of_eq rfl

-- ## Refold: every row is ≈ to its right-nested spine form
-- (assoc + units are exactly the axioms this consumes).
-- ⊢  ofSpine(s₁ ++ s₂) ≈ᵣ (ofSpine s₁ | ofSpine s₂)
theorem ofSpine_append {B : Type} : (s₁ s₂ : List (Atom B)) →
    RowEquiv (ofSpine (s₁ ++ s₂)) (.cat (ofSpine s₁) (ofSpine s₂))
  | [], _ => RowEquiv.unitL.symm
  | .field _ _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm
  | .var _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm
  | .dfield _ _ :: s₁, s₂ =>
      (RowEquiv.cat (.refl _) (ofSpine_append s₁ s₂)).trans RowEquiv.assoc.symm

-- ⊢  ${q}: τ ≈ᵣ ofSpine [ofKey q τ]
theorem ofKey_equiv {B : Type} (q : Key) (τ : Ty B) :
    RowEquiv (.dsing q τ) (ofSpine [Atom.ofKey q τ]) := by
  cases q with
  | lit l => exact RowEquiv.dsingLab.trans RowEquiv.unitR.symm
  | var α => exact RowEquiv.unitR.symm

-- ⊢  ofSpine (ofKey q τ :: s) ≈ᵣ (${q}: τ | ofSpine s)
theorem ofSpine_cons_ofKey {B : Type} (q : Key) (τ : Ty B) (s : List (Atom B)) :
    RowEquiv (ofSpine (Atom.ofKey q τ :: s)) (.cat (.dsing q τ) (ofSpine s)) := by
  cases q with
  | lit l => exact .cat RowEquiv.dsingLab.symm (.refl _)
  | var α => exact .refl _

-- ⊢  ρ ≈ᵣ ofSpine(spine ρ)      (every row ≈ its refolded spine)
theorem Row.toSpine_equiv {B : Type} : (ρ : Row B) → RowEquiv ρ (ofSpine ρ.toSpine)
  | .empty     => .refl _
  | .var _     => RowEquiv.unitR.symm
  | .sing _ _  => RowEquiv.unitR.symm
  | .cat ρ₁ ρ₂ =>
      (RowEquiv.cat (toSpine_equiv ρ₁) (toSpine_equiv ρ₂)).trans
        (ofSpine_append _ _).symm
  | .dsing q τ => ofKey_equiv q τ

-- ## Extraction
-- If the l-projection of a spine starts at segment index 0, the first l-field
-- sits in the leading segment: everything before it is a field with a
-- DIFFERENT label (index 0 ⟹ no preceding barrier; first l-occurrence ⟹ no
-- preceding l-field), so ≈-comm bubbles it to the front. All other
-- invariants are untouched — removing a field crosses no barrier.
-- ⊢  proj_l(s) = (0,τ)::rest   ⟹
--       ∃ t.  ofSpine s ≈ᵣ (l:τ | ofSpine t)  ∧  proj_l(t) = rest
--             ∧  (∀ l'≠l. proj_l'(t) = proj_l'(s))  ∧  bars(t) = bars(s)
--             ∧  dproj(t) = dproj(s)
theorem spine_extract {B : Type} {τ : Ty B} {rest : List (Nat × Ty B)} :
    (s : List (Atom B)) → (l : Label) →
    sProj l s = (0, τ) :: rest →
    ∃ t, RowEquiv (ofSpine s) (.cat (.sing l τ) (ofSpine t)) ∧
         sProj l t = rest ∧
         (∀ l', l' ≠ l → sProj l' t = sProj l' s) ∧
         sBarSeq t = sBarSeq s ∧ sDProj t = sDProj s
  | [], l, h => by simp [sProj] at h
  | .var β :: s', l, h => by
      simp only [sProj] at h
      exact (map_shift_ne_zero_head h).elim
  | .dfield _ _ :: s', l, h => by
      simp only [sProj] at h
      exact (map_shift_ne_zero_head h).elim
  | .field l₀ τ₀ :: s', l, h => by
      by_cases hl : l₀ = l
      · rw [hl] at h ⊢
        simp [sProj] at h
        obtain ⟨hτ, hrest⟩ := h
        subst hτ
        exact ⟨s', .refl _, hrest,
          fun l' hl' => by
            have hne : ¬ l = l' := fun hh => hl' hh.symm
            simp [sProj, if_neg hne],
          rfl, rfl⟩
      · have hEq : sProj l (.field l₀ τ₀ :: s') = sProj l s' := by
          simp [sProj, if_neg hl]
        rw [hEq] at h
        obtain ⟨t', hequiv, hproj, hothers, hvars, hd⟩ := spine_extract s' l h
        refine ⟨.field l₀ τ₀ :: t', ?_, ?_, ?_, ?_, ?_⟩
        · exact (RowEquiv.cat (.refl _) hequiv).trans
            (RowEquiv.assoc.symm.trans
              ((RowEquiv.cat (.comm hl) (.refl _)).trans RowEquiv.assoc))
        · simp [sProj, if_neg hl, hproj]
        · intro l' hl'
          by_cases hl₀ : l₀ = l'
          · simp [sProj, hl₀, hothers l' hl']
          · simp [sProj, if_neg hl₀, hothers l' hl']
        · simp [sBarSeq, hvars]
        · simp [sDProj, hd]

-- A spine with no barriers and no fields is empty.
-- ⊢  bars(s) = []  ∧  (∀ l. proj_l(s) = [])   ⟹   s = []
theorem spine_nil_of {B : Type} : (s : List (Atom B)) →
    sBarSeq s = [] → (∀ l, sProj l s = []) → s = []
  | [], _, _ => rfl
  | .var _ :: _, hv, _ => nomatch hv
  | .dfield _ _ :: _, hv, _ => nomatch hv
  | .field l _ :: _, _, hp => by have := hp l; simp [sProj] at this

-- ## Completeness: the invariants pin the spine up to ≈
-- Walk s₁ atom by atom. A leading barrier forces s₂'s first segment empty (no
-- index-0 projection entry), so s₂ leads with the SAME barrier (barrier
-- sequence; a key's type by the third invariant) — strip both (unshift). A
-- leading field l:τ is s₁'s first l-occurrence at index 0, so s₂'s
-- l-projection also starts (0, τ') with τ ≈ τ' — extract it, recurse on the
-- remainders.
-- ⊢  bars(s₁) = bars(s₂)  ∧  (∀ l. proj_l(s₁) ≈ₚ proj_l(s₂))
--      ∧  dproj(s₁) ≈ₚ dproj(s₂)   ⟹   ofSpine s₁ ≈ᵣ ofSpine s₂  (≈ completeness)
theorem char_complete {B : Type} :
    (s₁ s₂ : List (Atom B)) →
    sBarSeq s₁ = sBarSeq s₂ →
    (∀ l, ProjEquiv (sProj l s₁) (sProj l s₂)) →
    ProjEquiv (sDProj s₁) (sDProj s₂) →
    RowEquiv (ofSpine s₁) (ofSpine s₂)
  | [], s₂, hv, hp, _ => by
      rw [spine_nil_of s₂ hv.symm (fun l => (hp l).nil_inv)]
      exact .refl _
  | .var α :: t₁, s₂, hv, hp, hd => by
      cases s₂ with
      | nil => simp [sBarSeq] at hv
      | cons a t₂ =>
          cases a with
          | field l' τ' =>
              have h := hp l'
              simp [sProj] at h
              exact absurd h (ProjEquiv.no_zero_head _)
          | var β =>
              simp only [sBarSeq] at hv
              injection hv with hαβ hv'
              injection hαβ with hαβ
              subst hαβ
              exact .cat (.refl _)
                (char_complete t₁ t₂ hv' (fun l => (hp l).unshift) hd)
          | dfield _ _ => simp [sBarSeq] at hv
  | .dfield q τ :: t₁, s₂, hv, hp, hd => by
      cases s₂ with
      | nil => simp [sBarSeq] at hv
      | cons a t₂ =>
          cases a with
          | field l' τ' =>
              have h := hp l'
              simp [sProj] at h
              exact absurd h (ProjEquiv.no_zero_head _)
          | var β => simp [sBarSeq] at hv
          | dfield q' τ' =>
              simp only [sBarSeq] at hv
              injection hv with hqq hv'
              injection hqq with hqq
              subst hqq
              simp only [sDProj] at hd
              obtain ⟨_, _, heq, hty, hd'⟩ := hd.cons_inv
              injection heq with hhd htl
              injection hhd with _ hτ'
              subst hτ'; subst htl
              exact .cat (.dsing hty)
                (char_complete t₁ t₂ hv' (fun l => (hp l).unshift) hd')
  | .field l τ :: t₁, s₂, hv, hp, hd => by
      have h := hp l
      simp [sProj] at h
      obtain ⟨τ', rest, hs₂, hty, hrest⟩ := h.cons_inv
      obtain ⟨t₂, hequiv, hproj, hothers, hvars, hds⟩ := spine_extract s₂ l hs₂
      have hv' : sBarSeq t₁ = sBarSeq t₂ := by
        simp only [sBarSeq] at hv
        rw [hv, ← hvars]
      have hp' : ∀ l', ProjEquiv (sProj l' t₁) (sProj l' t₂) := by
        intro l'
        by_cases hl : l' = l
        · subst hl
          rw [hproj]
          exact hrest
        · have h' := hp l'
          have hne : ¬ l = l' := fun hh => hl hh.symm
          simp only [sProj, if_neg hne] at h'
          rw [hothers l' hl]
          exact h'
      have hd' : ProjEquiv (sDProj t₁) (sDProj t₂) := by
        simp only [sDProj] at hd
        rw [hds]; exact hd
      exact (RowEquiv.cat (.sing hty) (char_complete t₁ t₂ hv' hp' hd')).trans
        hequiv.symm

-- ## The characterization
-- ⊢  Char(ρ₁, ρ₂)   ⟹   ρ₁ ≈ᵣ ρ₂
theorem RowEquiv.ofChar {B : Type} {ρ₁ ρ₂ : Row B} (h : Row.Char ρ₁ ρ₂) :
    ρ₁ ≈ᵣ ρ₂ :=
  ρ₁.toSpine_equiv.trans
    ((char_complete _ _ h.1 h.2.1 h.2.2).trans ρ₂.toSpine_equiv.symm)

--   ρ₁ ≈ ρ₂   iff   same barrier sequence ∧ pointwise-≈ l-projections
--                   ∧ pointwise-≈ keyed-field types
-- ⊢  ρ₁ ≈ᵣ ρ₂   ↔   Char(ρ₁, ρ₂)
theorem rowEquiv_iff_char {B : Type} {ρ₁ ρ₂ : Row B} :
    ρ₁ ≈ᵣ ρ₂ ↔ Row.Char ρ₁ ρ₂ :=
  ⟨RowEquiv.char, RowEquiv.ofChar⟩

-- ## Cancellativity of shared end-vars
-- The trace-monoid fact ≐ᵣ's U-var-refl rests on: stripping a shared var off
-- either end is sound (trivially, by ≈-congruence) AND complete (below) —
-- this is what replaces P&X's shared-tail side condition [Δ₂]ρ₁ = [Δ₁]ρ₁.
-- ⊢  (α | ρ₁) ≈ᵣ (α | ρ₂)   ⟹   ρ₁ ≈ᵣ ρ₂
theorem RowEquiv.cancel_var_left {B : Type} {α : TyVar} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat (.var α) ρ₁) (.cat (.var α) ρ₂)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp, hd⟩ := h.char
  simp only [Row.toSpine, List.cons_append, List.nil_append, sBarSeq] at hv
  refine RowEquiv.ofChar ⟨?_, fun l => ?_, ?_⟩
  · injection hv
  · have h' := hp l
    simp only [Row.toSpine, List.cons_append, List.nil_append, sProj] at h'
    exact h'.unshift
  · simpa [Row.toSpine, sDProj] using hd

-- ⊢  (ρ₁ | α) ≈ᵣ (ρ₂ | α)   ⟹   ρ₁ ≈ᵣ ρ₂
theorem RowEquiv.cancel_var_right {B : Type} {α : TyVar} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ₁ (.var α)) (.cat ρ₂ (.var α))) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp, hd⟩ := h.char
  simp only [Row.toSpine, sBarSeq_append, sBarSeq] at hv
  have hveq : sBarSeq ρ₁.toSpine = sBarSeq ρ₂.toSpine :=
    (List.append_inj' hv rfl).1
  refine RowEquiv.ofChar ⟨hveq, fun l => ?_, ?_⟩
  · have h' := hp l
    simp only [Row.toSpine, sProj_append, sProj, List.map_nil,
               List.append_nil] at h'
    exact h'
  · simpa [Row.toSpine, sDProj_append, sDProj] using hd

-- ## Full cancellativity (the trace-monoid theorem proper)
-- Not just end-vars: ANY shared prefix or suffix row cancels. This is Levi's
-- lemma territory — a unifier is a factorization, and factorizations against
-- a shared block are unique — and the ingredient the trichotomy's
-- stuck ⟹ no-unique-mgu direction builds on.

-- ⊢  ps ≈ₚ qs   ⟹   |ps| = |qs|
theorem ProjEquiv.length {B : Type} :
    {ps qs : List (Nat × Ty B)} → ProjEquiv ps qs → ps.length = qs.length
  | _, _, .nil => rfl
  | _, _, .cons _ _ h => congrArg (· + 1) h.length

-- ⊢  (a ++ c) ≈ₚ (b ++ d),  |a| = |b|   ⟹   a ≈ₚ b  ∧  c ≈ₚ d
theorem ProjEquiv.split {B : Type} :
    {a b c d : List (Nat × Ty B)} → ProjEquiv (a ++ c) (b ++ d) →
    a.length = b.length → ProjEquiv a b ∧ ProjEquiv c d
  | [], [], _, _, h, _ => ⟨.nil, h⟩
  | [], _ :: _, _, _, _, hl => nomatch hl
  | _ :: _, [], _, _, _, hl => nomatch hl
  | _ :: _, _ :: _, _, _, h, hl => by
      cases h with
      | cons hn hty h' =>
          have hsplit := ProjEquiv.split h' (Nat.succ.inj hl)
          exact ⟨.cons hn hty hsplit.1, hsplit.2⟩

-- ⊢  map(·+k) ps ≈ₚ map(·+k) qs   ⟹   ps ≈ₚ qs
theorem ProjEquiv.unshiftK {B : Type} (k : Nat) :
    {ps qs : List (Nat × Ty B)} →
    ProjEquiv (ps.map (fun p => (p.1 + k, p.2))) (qs.map (fun p => (p.1 + k, p.2))) →
    ProjEquiv ps qs
  | [], [], _ => .nil
  | [], _ :: _, h => nomatch h
  | _ :: _, [], h => nomatch h
  | _ :: _, _ :: _, h => by
      cases h with
      | cons hn hty h' =>
          exact .cons (Nat.add_right_cancel hn) hty (ProjEquiv.unshiftK k h')

-- the keyed-field types of a shared block have equal length on both sides
private theorem sDProj_len_of_bars {B : Type} : (s₁ s₂ : List (Atom B)) →
    sBarSeq s₁ = sBarSeq s₂ → ProjEquiv (sDProj s₁) (sDProj s₂) →
    (sDProj s₁).length = (sDProj s₂).length
  | _, _, _, h => h.length

-- ⊢  (ρ | ρ₁) ≈ᵣ (ρ | ρ₂)   ⟹   ρ₁ ≈ᵣ ρ₂      (shared prefix cancels)
theorem RowEquiv.cancel_cat_left {B : Type} {ρ ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ ρ₁) (.cat ρ ρ₂)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp, hd⟩ := h.char
  simp only [Row.toSpine, sBarSeq_append] at hv
  refine RowEquiv.ofChar ⟨(List.append_inj hv rfl).2, fun l => ?_, ?_⟩
  · have h' := hp l
    simp only [Row.toSpine, sProj_append] at h'
    exact ((ProjEquiv.split h' rfl).2).unshiftK _
  · simp only [Row.toSpine, sDProj_append] at hd
    exact (ProjEquiv.split hd rfl).2

-- ⊢  (ρ₁ | ρ) ≈ᵣ (ρ₂ | ρ)   ⟹   ρ₁ ≈ᵣ ρ₂      (shared suffix cancels)
theorem RowEquiv.cancel_cat_right {B : Type} {ρ ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ₁ ρ) (.cat ρ₂ ρ)) : ρ₁ ≈ᵣ ρ₂ := by
  obtain ⟨hv, hp, hd⟩ := h.char
  simp only [Row.toSpine, sBarSeq_append] at hv
  have hlen : (sBarSeq ρ₁.toSpine).length = (sBarSeq ρ₂.toSpine).length := by
    have := congrArg List.length hv
    simp only [List.length_append] at this
    exact Nat.add_right_cancel this
  have hveq : sBarSeq ρ₁.toSpine = sBarSeq ρ₂.toSpine :=
    (List.append_inj hv hlen).1
  refine RowEquiv.ofChar ⟨hveq, fun l => ?_, ?_⟩
  · have h' := hp l
    simp only [Row.toSpine, sProj_append, hveq] at h'
    have hplen : (sProj l ρ₁.toSpine).length = (sProj l ρ₂.toSpine).length := by
      have := h'.length
      simp only [List.length_append, List.length_map] at this
      exact Nat.add_right_cancel this
    exact (ProjEquiv.split h' hplen).1
  · simp only [Row.toSpine, sDProj_append] at hd
    have hdlen : (sDProj ρ₁.toSpine).length = (sDProj ρ₂.toSpine).length := by
      have := hd.length
      simp only [List.length_append] at this
      exact Nat.add_right_cancel this
    exact (ProjEquiv.split hd hdlen).1

-- ## Leading-field cancellation (the completeness ingredient for U-field)
-- Unlike cancel_cat_left (identical shared prefix), the two leading fields carry
-- DIFFERENT types; cancelling them EXTRACTS the type equivalence. Both fields sit
-- at segment 0 (nothing precedes them), so ≈'s l-projection pins their types
-- pairwise and leaves the tails ≈. This is the forward direction the matchL move
-- needs: a unifier of the original must equate the matched field types.
-- ⊢  (l:τ₁ | R₁) ≈ᵣ (l:τ₂ | R₂)   ⟹   τ₁ ≈ₜ τ₂  ∧  R₁ ≈ᵣ R₂
theorem RowEquiv.field_cancel_left {B : Type} {l : Label} {τ₁ τ₂ : Ty B}
    {R₁ R₂ : Row B}
    (h : RowEquiv (.cat (.sing l τ₁) R₁) (.cat (.sing l τ₂) R₂)) :
    TyEquiv τ₁ τ₂ ∧ RowEquiv R₁ R₂ := by
  obtain ⟨hv, hp, hd⟩ := h.char
  simp only [Row.toSpine, sBarSeq_append, sBarSeq, List.nil_append] at hv
  have hred : ∀ (τ : Ty B) (R : Row B),
      sProj l (Row.toSpine (.cat (.sing l τ) R)) = (0, τ) :: sProj l R.toSpine := by
    intro τ R
    simp only [Row.toSpine, List.cons_append, List.nil_append, sProj]
    split
    · rfl
    · rename_i hne; exact absurd trivial hne
  have hpl := hp l
  rw [hred, hred] at hpl
  obtain ⟨τ', rest, heq, hty, hrest⟩ := hpl.cons_inv
  injection heq with hhd htl
  injection hhd with _ hτ'
  subst hτ'; subst htl
  refine ⟨hty, RowEquiv.ofChar ⟨hv, fun l' => ?_, ?_⟩⟩
  · by_cases hll' : l = l'
    · subst hll'; exact hrest
    · have h' := hp l'
      have hred' : ∀ (τ : Ty B) (R : Row B),
          sProj l' (Row.toSpine (.cat (.sing l τ) R)) = sProj l' R.toSpine := by
        intro τ R
        simp only [Row.toSpine, List.cons_append, List.nil_append, sProj]
        split
        · rename_i heq; exact absurd heq hll'
        · rfl
      rw [hred', hred'] at h'
      exact h'
  · simpa [Row.toSpine, sDProj] using hd

-- A product ≈ ε forces each factor ≈ ε (a cancellative monoid with no inverses:
-- ε has empty barrier sequence and empty projections, and all split over ++).
-- The base case of ≐ᵣ completeness — an exhausted side pins every leftover
-- var to ε.
-- ⊢  (ρ₁ | ρ₂) ≈ᵣ ε   ⟹   ρ₁ ≈ᵣ ε  ∧  ρ₂ ≈ᵣ ε
theorem RowEquiv.cat_empty_split {B : Type} {ρ₁ ρ₂ : Row B}
    (h : RowEquiv (.cat ρ₁ ρ₂) .empty) :
    RowEquiv ρ₁ .empty ∧ RowEquiv ρ₂ .empty := by
  obtain ⟨hv, hp, hd⟩ := h.char
  simp only [Row.toSpine, sBarSeq_append, sBarSeq] at hv
  obtain ⟨hv₁, hv₂⟩ := List.append_eq_nil_iff.mp hv
  have hpe : ∀ l, sProj l ρ₁.toSpine = [] ∧ sProj l ρ₂.toSpine = [] := by
    intro l
    have h' := hp l
    simp only [Row.toSpine, sProj_append, sProj] at h'
    have heq := ProjEquiv.nil_inv h'.symm
    obtain ⟨hx, hym⟩ := List.append_eq_nil_iff.mp heq
    exact ⟨hx, List.map_eq_nil_iff.mp hym⟩
  simp only [Row.toSpine, sDProj_append, sDProj] at hd
  obtain ⟨hd₁, hd₂⟩ := List.append_eq_nil_iff.mp (ProjEquiv.nil_inv hd.symm)
  exact ⟨RowEquiv.ofChar ⟨by rw [hv₁]; rfl,
            fun l => ProjEquiv.of_eq (by rw [(hpe l).1]; rfl),
            ProjEquiv.of_eq (by rw [hd₁]; rfl)⟩,
         RowEquiv.ofChar ⟨by rw [hv₂]; rfl,
            fun l => ProjEquiv.of_eq (by rw [(hpe l).2]; rfl),
            ProjEquiv.of_eq (by rw [hd₂]; rfl)⟩⟩

-- ## Ground rows: the characterization degenerates to the projections
-- (the T-eq workhorse for ≐ᵣ's ground completeness). "Ground" = no barrier:
-- no var and no keyed field on the spine.
def Row.Rigid {B : Type} (ρ : Row B) : Prop := sBarSeq ρ.toSpine = []

-- ⊢  no barrier on the spine   ⟹   ρ.SpineVarFree
theorem spineVarFree_of_barSeq_nil {B : Type} :
    (ρ : Row B) → sBarSeq ρ.toSpine = [] → ρ.SpineVarFree
  | .empty, _ => .empty
  | .var _, h => nomatch h
  | .sing _ _, _ => .sing
  | .cat ρ₁ ρ₂, h => by
      rw [show (Row.cat ρ₁ ρ₂).toSpine = ρ₁.toSpine ++ ρ₂.toSpine from rfl,
          sBarSeq_append, List.append_eq_nil_iff] at h
      exact .cat (spineVarFree_of_barSeq_nil ρ₁ h.1) (spineVarFree_of_barSeq_nil ρ₂ h.2)
  | .dsing q _, h => by
      cases q <;> simp [Row.toSpine, Atom.ofKey, sBarSeq] at h
      exact .dsing

-- ⊢  ρ₁, ρ₂ rigid   ⟹
--       ( ρ₁ ≈ᵣ ρ₂   ↔   ∀ l. proj_l(ρ₁) ≈ₚ proj_l(ρ₂) )
theorem ground_char {B : Type} {ρ₁ ρ₂ : Row B}
    (h₁ : ρ₁.Rigid) (h₂ : ρ₂.Rigid) :
    ρ₁ ≈ᵣ ρ₂ ↔ ∀ l, ProjEquiv (sProj l ρ₁.toSpine) (sProj l ρ₂.toSpine) :=
  ⟨fun h => h.char.2.1,
   fun hp => RowEquiv.ofChar
     ⟨h₁.trans h₂.symm, hp, by
        have e : ∀ s : List (Atom B), sBarSeq s = [] → sDProj s = [] := by
          intro s hs
          induction s with
          | nil => rfl
          | cons a t ih =>
              cases a with
              | field _ _ => exact ih hs
              | var _ => nomatch hs
              | dfield _ _ => nomatch hs
        rw [e _ h₁, e _ h₂]; exact .nil⟩⟩

-- ## The two ≐ᵣ regression examples, mechanized
-- A unifier equates the θ-images up to ≈:
def Unifies {B : Type} (θ : TySubst B) (ρ₁ ρ₂ : Row B) : Prop :=
  RowEquiv (ρ₁.applySubst θ) (ρ₂.applySubst θ)

-- P&X's shared-tail pitfall (l₁: 𝓫 | α) ≐ᵣ (l₂: 𝓫 | α), l₁ ≠ l₂: NO unifier
-- exists — cancel the shared α, then the l₁-projections clash. U-var-refl's
-- right-cancellation rejects this correctly with no side condition.
-- ⊢  l₁ ≠ l₂   ⟹   ¬ θ ⊨ (l₁:𝓫 | α) ≐ᵣ (l₂:𝓫 | α)
theorem shared_tail_no_unifier {B : Type} {b : B} {l₁ l₂ : Label} {α : TyVar}
    (hne : l₁ ≠ l₂) (θ : TySubst B) :
    ¬ Unifies θ (.cat (.sing l₁ (.base b)) (.var α))
                (.cat (.sing l₂ (.base b)) (.var α)) := by
  intro h
  unfold Unifies at h
  simp only [Row.applySubst, Ty.applySubst] at h
  obtain ⟨-, hp, -⟩ := h.cancel_cat_right.char
  have hcontra := hp l₁
  have hne' : ¬ l₂ = l₁ := fun hh => hne hh.symm
  simp only [Row.toSpine, sProj, if_neg hne'] at hcontra
  cases hcontra

-- The LUtail-loses-solutions example (l: 𝓫) ≐ᵣ (α | l: 𝓫): θ is a unifier
-- IFF θ maps α to (something ≈) ε — the unique mgu that P&X's LUtail misses
-- by committing α to contain l. Forced two-sided processing finds it.
-- ⊢  θ ⊨ (l:𝓫) ≐ᵣ (α | l:𝓫)   ↔   θα ≈ᵣ ε
theorem lutail_unifier_iff {B : Type} {b : B} {l : Label} {α : TyVar}
    (θ : TySubst B) :
    Unifies θ (.sing l (.base b)) (.cat (.var α) (.sing l (.base b))) ↔
    RowEquiv (θ.row α) .empty := by
  constructor
  · intro h
    unfold Unifies at h
    simp only [Row.applySubst, Ty.applySubst] at h
    have h' : RowEquiv (.cat (θ.row α) (.sing l (.base b)))
                       (.cat .empty (.sing l (.base b))) :=
      h.symm.trans RowEquiv.unitL.symm
    exact h'.cancel_cat_right
  · intro h
    unfold Unifies
    simp only [Row.applySubst, Ty.applySubst]
    exact RowEquiv.unitL.symm.trans (RowEquiv.cat h.symm (.refl _))

-- ## Wand's ambiguity: the stuck class is real
-- (β | α) ≐ᵣ (l: 𝓫) — Wand's non-principality example in unification
-- clothing. The l-field can come from either side, so solutions EXIST but
-- are incomparable: no mgu. This is trichotomy case (c) — U-stuck fails on
-- exactly this configuration, and correctly so.

/-- θ' factors through θ:  θ' = σ ∘ θ  (mod ≈). An mgu would have every
unifier factor through it. -/
def InstanceOf {B : Type} (θ' θ : TySubst B) : Prop :=
  ∃ σ : TySubst B,
    (∀ x, RowEquiv (θ'.row x) ((θ.row x).applySubst σ)) ∧
    (∀ x, TyEquiv (θ'.ty x) ((θ.ty x).applySubst σ)) ∧
    (∀ x, θ'.lab x = (θ.lab x).applySubst σ)

-- A ProjEquiv into a singleton splits: the entry comes from exactly one of
-- the two appended sides (per-label counting across a concatenation).
-- ⊢  (p₁ ++ p₂) ≈ₚ [(n,τ)]
--        ⟹  (p₁ = [] ∧ p₂ ≈ₚ [(n,τ)])  ∨  (p₁ ≈ₚ [(n,τ)] ∧ p₂ = [])
theorem ProjEquiv.append_singleton {B : Type} {p₁ p₂ : List (Nat × Ty B)}
    {n : Nat} {τ : Ty B} (h : ProjEquiv (p₁ ++ p₂) [(n, τ)]) :
    (p₁ = [] ∧ ProjEquiv p₂ [(n, τ)]) ∨ (ProjEquiv p₁ [(n, τ)] ∧ p₂ = []) := by
  cases p₁ with
  | nil => exact .inl ⟨rfl, h⟩
  | cons x rest =>
      cases h with
      | cons hn hty h' =>
          obtain ⟨hr, hp₂⟩ := List.append_eq_nil_iff.mp h'.symm.nil_inv
          subst hr
          exact .inr ⟨.cons hn hty .nil, hp₂⟩

-- A ground row whose only projection entry is (0, 𝓫) at label l IS the
-- singleton (l: 𝓫), mod ≈ (assembled from the characterization).
-- ⊢  vars(spine ρ) = [],  proj_l(ρ) ≈ₚ [(0,𝓫)],  (∀ l'≠l. proj_l'(ρ) = [])
--        ⟹   ρ ≈ᵣ (l:𝓫)
theorem row_equiv_sing_of_char {B : Type} {ρ : Row B} {l : Label} {b : B}
    (hv : sBarSeq ρ.toSpine = [])
    (hl : ProjEquiv (sProj l ρ.toSpine) [(0, .base b)])
    (hothers : ∀ l', l' ≠ l → sProj l' ρ.toSpine = [])
    (hd : sDProj ρ.toSpine = []) :
    RowEquiv ρ (.sing l (.base b)) := by
  refine RowEquiv.ofChar ⟨?_, fun l' => ?_, ?_⟩
  · simpa [Row.toSpine, sBarSeq] using hv
  · by_cases h : l' = l
    · subst h
      simpa [Row.toSpine, sProj] using hl
    · have hne : ¬ l = l' := fun hh => h hh.symm
      rw [hothers l' h,
          show sProj l' (Row.sing l (.base b)).toSpine = [] from by
            simp [Row.toSpine, sProj, if_neg hne]]
      exact .nil
  · rw [hd]; exact .nil

-- Solutions exist …
-- ⊢  ∃ θ.  θ ⊨ (β | α) ≐ᵣ (l:𝓫)
theorem wand_unifiable {B : Type} (b : B) (l : Label) :
    ∃ θ : TySubst B,
      Unifies θ (.cat (.var "β") (.var "α")) (.sing l (.base b)) :=
  ⟨⟨fun x => .var x,
    fun x => if x = "β" then .sing l (.base b)
             else if x = "α" then .empty else .var x,
    fun x => .var x⟩,
   by unfold Unifies
      simp [Row.applySubst, Ty.applySubst]
      exact RowEquiv.unitR⟩

-- … but no mgu does: a would-be mgu θ must put the l-field into θβ or θα;
-- either way, one of the two witness unifiers (field-from-β / field-from-α)
-- cannot factor through θ, because a ground singleton can never be
-- substituted into ε (⟦concrete-atom pairings are substitution-stable⟧).
-- ⊢  ¬ ∃ θ.  θ ⊨ (β | α) ≐ᵣ (l:𝓫)
--             ∧  (∀ θ'. θ' ⊨ (β | α) ≐ᵣ (l:𝓫) ⟹ θ' ⊑ θ)      (no mgu)
theorem wand_no_mgu {B : Type} (b : B) (l : Label) :
    ¬ ∃ θ : TySubst B,
        Unifies θ (.cat (.var "β") (.var "α")) (.sing l (.base b)) ∧
        ∀ θ' : TySubst B,
          Unifies θ' (.cat (.var "β") (.var "α")) (.sing l (.base b)) →
          InstanceOf θ' θ := by
  rintro ⟨θ, hu, hmgu⟩
  unfold Unifies at hu
  simp only [Row.applySubst, Ty.applySubst] at hu
  obtain ⟨hv, hp, hd⟩ := hu.char
  simp only [Row.toSpine, sBarSeq_append, sBarSeq] at hv
  obtain ⟨hvβ, hvα⟩ := List.append_eq_nil_iff.mp hv
  simp only [Row.toSpine, sDProj_append, sDProj] at hd
  obtain ⟨hdβ, hdα⟩ := List.append_eq_nil_iff.mp (ProjEquiv.nil_inv hd.symm)
  have hpl := hp l
  simp [Row.toSpine, sProj_append, hvβ, sProj] at hpl
  have hothers : ∀ l', l' ≠ l →
      sProj l' (θ.row "β").toSpine = [] ∧ sProj l' (θ.row "α").toSpine = [] := by
    intro l' hl'
    have h' := hp l'
    have hne : ¬ l = l' := fun hh => hl' hh.symm
    simp [Row.toSpine, sProj_append, hvβ, sProj, if_neg hne] at h'
    exact List.append_eq_nil_iff.mp h'.symm.nil_inv
  rcases hpl.append_singleton with ⟨hβnil, hαl⟩ | ⟨hβl, hαnil⟩
  · -- the l-field lives in θα: the field-from-β unifier cannot factor
    have hα : RowEquiv (θ.row "α") (.sing l (.base b)) :=
      row_equiv_sing_of_char hvα hαl (fun l' hl' => (hothers l' hl').2) hdα
    obtain ⟨σ, hrow, -⟩ := hmgu
      ⟨fun x => .var x,
       fun x => if x = "β" then .sing l (.base b)
                else if x = "α" then .empty else .var x,
       fun x => .var x⟩
      (by unfold Unifies
          simp [Row.applySubst, Ty.applySubst]
          exact RowEquiv.unitR)
    have hcl := hrow "α"
    simp at hcl
    have hcontra : RowEquiv (.empty : Row B) (.sing l (.base b)) :=
      hcl.trans (RowEquiv.applySubst σ hα)
    obtain ⟨-, hpc, -⟩ := hcontra.char
    have hfalse := hpc l
    simp [Row.toSpine, sProj] at hfalse
    cases hfalse
  · -- the l-field lives in θβ: the field-from-α unifier cannot factor
    have hβ : RowEquiv (θ.row "β") (.sing l (.base b)) :=
      row_equiv_sing_of_char hvβ hβl (fun l' hl' => (hothers l' hl').1) hdβ
    obtain ⟨σ, hrow, -⟩ := hmgu
      ⟨fun x => .var x,
       fun x => if x = "β" then .empty
                else if x = "α" then .sing l (.base b) else .var x,
       fun x => .var x⟩
      (by unfold Unifies
          simp [Row.applySubst, Ty.applySubst]
          exact RowEquiv.unitL)
    have hcl := hrow "β"
    simp at hcl
    have hcontra : RowEquiv (.empty : Row B) (.sing l (.base b)) :=
      hcl.trans (RowEquiv.applySubst σ hβ)
    obtain ⟨-, hpc, -⟩ := hcontra.char
    have hfalse := hpc l
    simp [Row.toSpine, sProj] at hfalse
    cases hfalse

end MinimalCalculus
