-- FC-labels: the lookup QUERY is a key.
--
-- A selection `e₁.(e₂)` looks up whatever label e₂ has; its type is ⌊k⌋ for a
-- KEY k — a literal l (and the lookup is the old `Lookup`) or a LABEL variable
-- α (a label nobody has chosen yet). Against a literal field, a label variable
-- is UNDECIDED — α might be instantiated to that very label, or to another — so
-- the lookup answers `?` there (L-?-lab).
--
-- Label variables are a sort of their own (`TySubst.lab`): a substitution sends
-- one to a key, never to `int` or ★. So there is no junk key: a selection whose
-- argument is not label-typed is simply ill-typed.
--
-- Phase B (dynamic construction) adds keyed fields `${k}: τ`, and then α ≡ α
-- fires: `(${α}: τ).α ↓ τ`, stable under every substitution because α goes to
-- one key.
--
-- One structural relation serves every key; `Lookup` (label-keyed, in
-- minimal.lean) is its literal instance (`LookupQ.lab_iff`), and `LookupV` its
-- label-variable instance.
import minimal

namespace MinimalCalculus

variable {B : Type}

/-- `ρ.k ↓ r` — the lookup a dynamic selection performs, keyed by the key of its
label argument. Every field is compared with the key by `Key.cmp`. -/
inductive LookupQ : Row B → Key → LookupRes B → Prop where
  -- L-ε
  | emp {k : Key} : LookupQ .empty k .absent
  -- L-hit / L-miss / L-?-lab against a literal field
  | hit {k : Key} {l : Label} {τ : Ty B} :
      Key.cmp k (.lit l) = .eq → LookupQ (.sing l τ) k (.found τ)
  | miss {k : Key} {l : Label} {τ : Ty B} :
      Key.cmp k (.lit l) = .apart → LookupQ (.sing l τ) k .absent
  | sunk {k : Key} {l : Label} {τ : Ty B} :
      Key.cmp k (.lit l) = .undec → LookupQ (.sing l τ) k .unknown
  -- L-α-free
  | varFree {k : Key} {β : TyVar} : LookupQ (.var β) k .unknown
  -- L-conc-hit
  | catHit {k : Key} {ρ₁ ρ₂ : Row B} {τ : Ty B} :
      LookupQ ρ₁ k (.found τ) → LookupQ (.cat ρ₁ ρ₂) k (.found τ)
  -- L-conc-skip
  | catSkip {k : Key} {ρ₁ ρ₂ : Row B} {r : LookupRes B} :
      LookupQ ρ₁ k .absent → LookupQ ρ₂ k r → LookupQ (.cat ρ₁ ρ₂) k r
  -- L-conc-★
  | catUnk {k : Key} {ρ₁ ρ₂ : Row B} :
      LookupQ ρ₁ k .unknown → LookupQ (.cat ρ₁ ρ₂) k .unknown
  -- the same three against a keyed field
  | dhit {k q : Key} {τ : Ty B} : Key.cmp k q = .eq → LookupQ (.dsing q τ) k (.found τ)
  | dmiss {k q : Key} {τ : Ty B} : Key.cmp k q = .apart → LookupQ (.dsing q τ) k .absent
  | dunk {k q : Key} {τ : Ty B} : Key.cmp k q = .undec → LookupQ (.dsing q τ) k .unknown

/-- the label-variable instance. -/
abbrev LookupV (ρ : Row B) (α : TyVar) (r : LookupRes B) : Prop :=
  LookupQ ρ (.var α) r


------------------------------ KEY COMPARISON ----------------------------------

theorem Key.cmp_symm_eq : (c d : Key) → c.cmp d = .eq → d.cmp c = .eq
  | .lit l, .lit l', h => by
      simp only [Key.cmp] at h ⊢; split at h
      · subst l; simp
      · cases h
  | .var α, .var β, h => by
      simp only [Key.cmp] at h ⊢; split at h
      · subst α; simp
      · cases h
  | .lit _, .var _, h => nomatch h
  | .var _, .lit _, h => nomatch h

-- a definite comparison is between one key and itself
theorem Key.cmp_eq_cases {k q : Key} (h : Key.cmp k q = .eq) : k = q := by
  cases k <;> cases q <;> simp only [Key.cmp] at h <;> (try split at h) <;>
    first | (subst_vars; rfl) | cases h

-- a key undecided against a literal is a label variable
theorem Key.cmp_undec_lit {k : Key} {l : Label}
    (h : Key.cmp k (.lit l) = .undec) : ∃ α, k = .var α := by
  cases k with
  | var α => exact ⟨α, rfl⟩
  | lit l' => simp only [Key.cmp_lit_lit] at h; split at h <;> cases h

-- a label variable is undecided against everything but itself
theorem Key.cmp_var_ne {x : Key} {β : TyVar} (h : x ≠ .var β) :
    Key.cmp (.var β) x = .undec := by
  cases x with
  | var γ =>
      simp only [Key.cmp]
      rw [if_neg (fun he => h (by rw [he]))]
  | lit _ => rfl


------------------------------ BASIC METATHEORY --------------------------------

@[simp] theorem LookupQ.lab_iff {ρ : Row B} {l : Label} {r : LookupRes B} :
    LookupQ ρ (.lit l) r ↔ Lookup ρ l r := by
  constructor
  · intro h
    generalize hk : (Key.lit l) = k at h
    induction h with
    | emp => exact .emp
    | hit h =>
        subst hk; simp only [Key.cmp_lit_lit] at h; split at h
        · subst l; exact .hit
        · cases h
    | miss h =>
        subst hk; simp only [Key.cmp_lit_lit] at h; split at h
        · cases h
        · exact .miss (Ne.symm ‹_›)
    | sunk h => subst hk; simp only [Key.cmp_lit_lit] at h; split at h <;> cases h
    | varFree => exact .varFree
    | catHit _ ih => exact .catHit (ih hk)
    | catSkip _ _ ih₁ ih₂ => exact .catSkip (ih₁ hk) (ih₂ hk)
    | catUnk _ ih => exact .catUnk (ih hk)
    | dhit h => subst hk; exact .dhit h
    | dmiss h => subst hk; exact .dmiss h
    | dunk h => subst hk; exact .dunk h
  · intro h
    induction h with
    | emp => exact .emp
    | hit => exact .hit (by simp)
    | miss hne => exact .miss (by simp [Ne.symm hne])
    | varFree => exact .varFree
    | catHit _ ih => exact .catHit ih
    | catSkip _ _ ih₁ ih₂ => exact .catSkip ih₁ ih₂
    | catUnk _ ih => exact .catUnk ih
    | dhit h => exact .dhit h
    | dmiss h => exact .dmiss h
    | dunk h => exact .dunk h

theorem LookupQ.var_iff {ρ : Row B} {α : TyVar} {r : LookupRes B} :
    LookupQ ρ (.var α) r ↔ LookupV ρ α r := Iff.rfl

theorem LookupQ.det {ρ : Row B} {q : Key} {r₁ r₂ : LookupRes B}
    (h₁ : LookupQ ρ q r₁) (h₂ : LookupQ ρ q r₂) : r₁ = r₂ := by
  induction h₁ generalizing r₂ with
  | emp => cases h₂; rfl
  | hit h => cases h₂ <;> simp_all
  | miss h => cases h₂ <;> simp_all
  | sunk h => cases h₂ <;> simp_all
  | varFree => cases h₂; rfl
  | catHit _ ih =>
      cases h₂ with
      | catHit h' => exact ih h'
      | catSkip h' _ => cases ih h'
      | catUnk h' => cases ih h'
  | catSkip _ _ ih₁ ih₂ =>
      cases h₂ with
      | catHit h' => cases ih₁ h'
      | catSkip _ h' => exact ih₂ h'
      | catUnk h' => cases ih₁ h'
  | catUnk _ ih =>
      cases h₂ with
      | catHit h' => cases ih h'
      | catSkip h' _ => cases ih h'
      | catUnk _ => rfl
  | dhit h => cases h₂ <;> simp_all
  | dmiss h => cases h₂ <;> simp_all
  | dunk h => cases h₂ <;> simp_all

theorem LookupV.det {ρ : Row B} {α : TyVar} {r₁ r₂ : LookupRes B}
    (h₁ : LookupV ρ α r₁) (h₂ : LookupV ρ α r₂) : r₁ = r₂ := LookupQ.det h₁ h₂

-- ⊢  totality: bare structural recursion, as for `lookup_total`
theorem LookupQ.total : (ρ : Row B) → (q : Key) → ∃ r, LookupQ ρ q r
  | .empty, _ => ⟨_, .emp⟩
  | .var _, _ => ⟨_, .varFree⟩
  | .sing l _, k =>
      match h : Key.cmp k (.lit l) with
      | .eq => ⟨_, .hit h⟩
      | .apart => ⟨_, .miss h⟩
      | .undec => ⟨_, .sunk h⟩
  | .dsing q _, k =>
      match h : Key.cmp k q with
      | .eq => ⟨_, .dhit h⟩
      | .apart => ⟨_, .dmiss h⟩
      | .undec => ⟨_, .dunk h⟩
  | .cat ρ₁ ρ₂, k =>
      match LookupQ.total ρ₁ k with
      | ⟨.found _, hr₁⟩ => ⟨_, .catHit hr₁⟩
      | ⟨.absent, hr₁⟩ =>
          match LookupQ.total ρ₂ k with
          | ⟨_, hr₂⟩ => ⟨_, .catSkip hr₁ hr₂⟩
      | ⟨.unknown, hr₁⟩ => ⟨_, .catUnk hr₁⟩

theorem LookupV.total (ρ : Row B) (α : TyVar) : ∃ r, LookupV ρ α r :=
  LookupQ.total ρ (.var α)


------------------------------ ROW EQUIVALENCE ---------------------------------

section EquivHelpers
variable {ρ ρ₁ ρ₂ ρ₃ : Row B} {k : Key} {r : LookupRes B}

private theorem lookupQ_assoc_fwd :
    LookupQ (.cat (.cat ρ₁ ρ₂) ρ₃) k r → LookupQ (.cat ρ₁ (.cat ρ₂ ρ₃)) k r := by
  intro h
  cases h with
  | catHit h₁₂ =>
      cases h₁₂ with
      | catHit h₁     => exact .catHit h₁
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catHit h₂)
  | catSkip h₁₂ h₃ =>
      cases h₁₂ with
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catSkip h₂ h₃)
  | catUnk h₁₂ =>
      cases h₁₂ with
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catUnk h₂)
      | catUnk h₁     => exact .catUnk h₁

private theorem lookupQ_assoc_bwd :
    LookupQ (.cat ρ₁ (.cat ρ₂ ρ₃)) k r → LookupQ (.cat (.cat ρ₁ ρ₂) ρ₃) k r := by
  intro h
  cases h with
  | catHit h₁ => exact .catHit (.catHit h₁)
  | catSkip h₁ h₂₃ =>
      cases h₂₃ with
      | catHit h₂     => exact .catHit (.catSkip h₁ h₂)
      | catSkip h₂ h₃ => exact .catSkip (.catSkip h₁ h₂) h₃
      | catUnk h₂     => exact .catUnk (.catSkip h₁ h₂)
  | catUnk h₁ => exact .catUnk (.catUnk h₁)

private theorem lookupQ_unitL_fwd : LookupQ (.cat .empty ρ) k r → LookupQ ρ k r := by
  intro h
  cases h with
  | catHit hε => cases hε
  | catSkip _ h₂ => exact h₂
  | catUnk hε => cases hε

private theorem lookupQ_unitR_fwd : LookupQ (.cat ρ .empty) k r → LookupQ ρ k r := by
  intro h
  cases h with
  | catHit h₁ => exact h₁
  | catSkip h₁ hε => cases hε; exact h₁
  | catUnk h₁ => exact h₁

private theorem lookupQ_unitR_bwd (h : LookupQ ρ k r) : LookupQ (.cat ρ .empty) k r := by
  cases r with
  | found τ => exact .catHit h
  | absent => exact .catSkip h .emp
  | unknown => exact .catUnk h

-- two distinct literal fields: a key is eq to at most one of them, and
-- undecided against both or neither
private theorem keyCmp_two_labs {k : Key} {l₁ l₂ : Label} (hne : l₁ ≠ l₂) :
    (Key.cmp k (.lit l₁) = .eq → Key.cmp k (.lit l₂) = .apart) ∧
    (Key.cmp k (.lit l₁) = .undec ↔ Key.cmp k (.lit l₂) = .undec) := by
  cases k with
  | lit l =>
      simp only [Key.cmp_lit_lit]
      refine ⟨fun h => ?_, ?_⟩
      · split at h
        · subst l; simp [hne]
        · cases h
      · constructor <;> intro h <;> split at h <;> cases h
  | var α => simp [Key.cmp]

private theorem lookupQ_comm_fwd {l₁ l₂ : Label} {τ₁ τ₂ : Ty B} (hne : l₁ ≠ l₂) :
    LookupQ (.cat (.sing l₁ τ₁) (.sing l₂ τ₂)) k r →
    LookupQ (.cat (.sing l₂ τ₂) (.sing l₁ τ₁)) k r := by
  have F := keyCmp_two_labs (k := k) hne
  intro h
  cases h with
  | catHit h₁ =>
      cases h₁ with
      | hit e₁ => exact .catSkip (.miss (F.1 e₁)) (.hit e₁)
  | catSkip h₁ h₂ =>
      cases h₁ with
      | miss e₁ =>
          cases h₂ with
          | hit e₂ => exact .catHit (.hit e₂)
          | miss e₂ => exact .catSkip (.miss e₂) (.miss e₁)
          | sunk e₂ => rw [F.2.2 e₂] at e₁; cases e₁
  | catUnk h₁ =>
      cases h₁ with
      | sunk e₁ => exact .catUnk (.sunk (F.2.1 e₁))

end EquivHelpers

-- ⊢  the query lookup respects ≈, with found types up to ≈
theorem LookupQ.equiv_both :
    {ρ₁ ρ₂ : Row B} → RowEquiv ρ₁ ρ₂ →
    (∀ {k r}, LookupQ ρ₁ k r → ∃ r', LookupQ ρ₂ k r' ∧ ResEquiv r r') ∧
    (∀ {k r}, LookupQ ρ₂ k r → ∃ r', LookupQ ρ₁ k r' ∧ ResEquiv r r')
  | _, _, .refl _ => ⟨fun h => ⟨_, h, .refl _⟩, fun h => ⟨_, h, .refl _⟩⟩
  | _, _, .symm h => (LookupQ.equiv_both h).symm
  | _, _, .trans h₁ h₂ =>
      have ih₁ := LookupQ.equiv_both h₁
      have ih₂ := LookupQ.equiv_both h₂
      ⟨fun hl => match ih₁.1 hl with
        | ⟨_, hm, e₁⟩ => match ih₂.1 hm with
          | ⟨_, hr, e₂⟩ => ⟨_, hr, e₁.trans e₂⟩,
       fun hl => match ih₂.2 hl with
        | ⟨_, hm, e₁⟩ => match ih₁.2 hm with
          | ⟨_, hr, e₂⟩ => ⟨_, hr, e₁.trans e₂⟩⟩
  | _, _, .sing hty =>
      ⟨fun hl => match hl with
        | .hit h => ⟨_, .hit h, .found hty⟩
        | .miss h => ⟨_, .miss h, .absent⟩
        | .sunk h => ⟨_, .sunk h, .unknown⟩,
       fun hl => match hl with
        | .hit h => ⟨_, .hit h, .found hty.symm⟩
        | .miss h => ⟨_, .miss h, .absent⟩
        | .sunk h => ⟨_, .sunk h, .unknown⟩⟩
  | _, _, .dsing hty =>
      ⟨fun hl => match hl with
        | .dhit h => ⟨_, .dhit h, .found hty⟩
        | .dmiss h => ⟨_, .dmiss h, .absent⟩
        | .dunk h => ⟨_, .dunk h, .unknown⟩,
       fun hl => match hl with
        | .dhit h => ⟨_, .dhit h, .found hty.symm⟩
        | .dmiss h => ⟨_, .dmiss h, .absent⟩
        | .dunk h => ⟨_, .dunk h, .unknown⟩⟩
  | _, _, .dsingLab =>
      ⟨fun hl => match hl with
        | .dhit h => ⟨_, .hit h, .refl _⟩
        | .dmiss h => ⟨_, .miss h, .refl _⟩
        | .dunk h => ⟨_, .sunk h, .refl _⟩,
       fun hl => match hl with
        | .hit h => ⟨_, .dhit h, .refl _⟩
        | .miss h => ⟨_, .dmiss h, .refl _⟩
        | .sunk h => ⟨_, .dunk h, .refl _⟩⟩
  | _, _, .cat h₁ h₂ =>
      have ih₁ := LookupQ.equiv_both h₁
      have ih₂ := LookupQ.equiv_both h₂
      ⟨fun hl => match hl with
        | .catHit hf => match ih₁.1 hf with
          | ⟨_, h', .found te⟩ => ⟨_, .catHit h', .found te⟩
        | .catSkip ha hr => match ih₁.1 ha with
          | ⟨_, h', .absent⟩ => match ih₂.1 hr with
            | ⟨_, h'', e⟩ => ⟨_, .catSkip h' h'', e⟩
        | .catUnk hu => match ih₁.1 hu with
          | ⟨_, h', .unknown⟩ => ⟨_, .catUnk h', .unknown⟩,
       fun hl => match hl with
        | .catHit hf => match ih₁.2 hf with
          | ⟨_, h', .found te⟩ => ⟨_, .catHit h', .found te⟩
        | .catSkip ha hr => match ih₁.2 ha with
          | ⟨_, h', .absent⟩ => match ih₂.2 hr with
            | ⟨_, h'', e⟩ => ⟨_, .catSkip h' h'', e⟩
        | .catUnk hu => match ih₁.2 hu with
          | ⟨_, h', .unknown⟩ => ⟨_, .catUnk h', .unknown⟩⟩
  | _, _, .assoc =>
      ⟨fun hl => ⟨_, lookupQ_assoc_fwd hl, .refl _⟩,
       fun hl => ⟨_, lookupQ_assoc_bwd hl, .refl _⟩⟩
  | _, _, .unitL =>
      ⟨fun hl => ⟨_, lookupQ_unitL_fwd hl, .refl _⟩,
       fun hl => ⟨_, .catSkip .emp hl, .refl _⟩⟩
  | _, _, .unitR =>
      ⟨fun hl => ⟨_, lookupQ_unitR_fwd hl, .refl _⟩,
       fun hl => ⟨_, lookupQ_unitR_bwd hl, .refl _⟩⟩
  | _, _, .comm hne =>
      ⟨fun hl => ⟨_, lookupQ_comm_fwd hne hl, .refl _⟩,
       fun hl => ⟨_, lookupQ_comm_fwd (Ne.symm hne) hl, .refl _⟩⟩

theorem LookupQ.equiv {ρ₁ ρ₂ : Row B} {q : Key} {r₁ : LookupRes B}
    (heq : RowEquiv ρ₁ ρ₂) (h : LookupQ ρ₁ q r₁) :
    ∃ r₂, LookupQ ρ₂ q r₂ ∧ ResEquiv r₁ r₂ :=
  (LookupQ.equiv_both heq).1 h


------------------------------ SUBSTITUTION ------------------------------------
-- A definite answer is stable under substitution, the key included: eq and
-- apart comparisons survive (`Key.cmp_applySubst`), and a definite derivation
-- never looked at a row variable.

/-- no field and no variable on the spine: ε's, concatenated. -/
inductive Row.Hollow : Row B → Prop where
  | empty : Row.Hollow .empty
  | cat {ρ₁ ρ₂ : Row B} : Row.Hollow ρ₁ → Row.Hollow ρ₂ → Row.Hollow (.cat ρ₁ ρ₂)

-- a label variable is never apart from a field key, so its ⊥ means HOLLOW
theorem LookupV.hollow {ρ : Row B} {α : TyVar}
    {r : LookupRes B} (h : LookupV ρ α r) (hr : r = .absent) : ρ.Hollow := by
  unfold LookupV at h
  generalize hk : (Key.var α) = k at h
  induction h with
  | emp => exact .empty
  | miss h => subst hk; simp [Key.cmp] at h
  | dmiss h =>
      subst hk
      rename_i q _
      cases q <;> simp [Key.cmp] at h
      split at h <;> cases h
  | catSkip _ _ ih₁ ih₂ => exact .cat (ih₁ rfl hk) (ih₂ hr hk)
  | _ => cases hr

theorem Row.Hollow.applySubst {ρ : Row B} (h : ρ.Hollow) (θ : TySubst B) :
    (ρ.applySubst θ).Hollow := by
  induction h with
  | empty => exact .empty
  | cat _ _ ih₁ ih₂ => exact .cat ih₁ ih₂

theorem Row.Hollow.lookup {ρ : Row B} (h : ρ.Hollow) (l : Label) :
    Lookup ρ l .absent := by
  induction h with
  | empty => exact .emp
  | cat _ _ ih₁ ih₂ => exact .catSkip ih₁ ih₂

theorem Row.Hollow.lookupQ {ρ : Row B} (h : ρ.Hollow) (q : Key) :
    LookupQ ρ q .absent := by
  induction h with
  | empty => exact .emp
  | cat _ _ ih₁ ih₂ => exact .catSkip ih₁ ih₂

theorem Row.Hollow.lookupV {ρ : Row B} (h : ρ.Hollow) (α : TyVar) :
    LookupV ρ α .absent := h.lookupQ _

-- ⊢  the twin of `lookup_applySubst`, with the key substituted too
theorem LookupQ.applySubst {ρ : Row B} {q : Key} {r : LookupRes B}
    (θ : TySubst B) (h : LookupQ ρ q r) (hr : r ≠ .unknown) :
    LookupQ (ρ.applySubst θ) (q.applySubst θ) (r.applySubst θ) := by
  induction h with
  | emp => exact .emp
  | hit h => exact .hit (Key.cmp_applySubst θ h (by simp))
  | miss h => exact .miss (Key.cmp_applySubst θ h (by simp))
  | sunk _ => exact absurd rfl hr
  | varFree => exact absurd rfl hr
  | catHit _ ih =>
      simp only [Row.applySubst]
      exact .catHit (ih (by intro h; cases h))
  | catSkip _ _ ih₁ ih₂ =>
      simp only [Row.applySubst]
      exact .catSkip (ih₁ (by intro h; cases h)) (ih₂ hr)
  | catUnk _ _ => exact absurd rfl hr
  | dhit h => exact .dhit (Key.cmp_applySubst θ h (by simp))
  | dmiss h => exact .dmiss (Key.cmp_applySubst θ h (by simp))
  | dunk _ => exact absurd rfl hr

end MinimalCalculus
