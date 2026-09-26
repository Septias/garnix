-- FC-labels, phase A: the lookup QUERY is a type.
--
-- A selection `e₁.(e₂)` looks up whatever label e₂ has, and its type is either
-- ⌊l⌋ (a literal, and the lookup is the old `Lookup`) or a label VARIABLE α
-- (a type variable standing for a label nobody has chosen yet). Against a
-- literal field, a label variable is UNDECIDED — α might be instantiated to that
-- very label, or to another — so the lookup answers `?` there (L-?-lab). It can
-- never answer a type: ⊥ when the row provably has no field at all, ? otherwise.
--
-- Label variables are ordinary type variables, so nothing stops a substitution
-- from sending one to `int`. Rather than a kind discipline, the lookup says what
-- that means: a key that is not a label names no field, so the answer is ⊥
-- (L-junk). This is the soft-typing reading — `r.${1}` types at ★ with a flag
-- and is a ↯ lookup error at run time — and it is what keeps every stability
-- lemma unconditional: a variable query only ever answers ? (improvable) or ⊥
-- (the row is hollow, and a hollow row answers ⊥ to ANY key).
--
-- Phase A has no var-labeled fields (only `{ ${e} = v }` would create them), so
-- `α ≡ α` never gets a chance to fire here; `LookupV` carries α for phase B.
import minimal

namespace MinimalCalculus

variable {B : Type}

/-- `Γ ⊢ ρ.α ↓ r` for a label VARIABLE α. The spine rules are L-ε, L-α,
L-α-free and the three concatenation rules, unchanged; the field rule is
L-?-lab: a literal field is undecided against a variable. -/
inductive LookupV (Γ : Ctx B) : Row B → TyVar → LookupRes B → Prop where
  -- L-ε
  | emp {α : TyVar} : LookupV Γ .empty α .absent
  -- L-?-lab: `l` and `α` may or may not be the same label
  | sing {α : TyVar} {l : Label} {τ : Ty B} : LookupV Γ (.sing l τ) α .unknown
  -- L-α
  | var {α β : TyVar} {ρ : Row B} {r : LookupRes B} :
      Γ.lookupRow β = some ρ → LookupV Γ ρ α r → LookupV Γ (.var β) α r
  -- L-α-free
  | varFree {α β : TyVar} : Γ.lookupRow β = none → LookupV Γ (.var β) α .unknown
  -- L-conc-skip
  | catSkip {α : TyVar} {ρ₁ ρ₂ : Row B} {r : LookupRes B} :
      LookupV Γ ρ₁ α .absent → LookupV Γ ρ₂ α r → LookupV Γ (.cat ρ₁ ρ₂) α r
  -- L-conc-★
  | catUnk {α : TyVar} {ρ₁ ρ₂ : Row B} :
      LookupV Γ ρ₁ α .unknown → LookupV Γ (.cat ρ₁ ρ₂) α .unknown

/-- a type that can stand in label position: ⌊l⌋ or a (label) variable. -/
def Ty.IsQuery : Ty B → Prop
  | .lab _  => True
  | .var _  => True
  | .base _ => False
  | .unk    => False
  | .fn _ _ => False
  | .rcd _  => False
-- (spelled out: a wildcard arm here compiles through `propext`, and every
-- declarative statement would inherit it)

/-- `Γ ⊢ ρ.q ↓ r` — the lookup a dynamic selection performs, keyed by the TYPE
of its label argument: ⌊l⌋ is the ordinary lookup, a label variable is
`LookupV`, and any other type is not a label and names no field (L-junk). -/
inductive LookupQ (Γ : Ctx B) : Row B → Ty B → LookupRes B → Prop where
  | lit {ρ : Row B} {l : Label} {r : LookupRes B} :
      Lookup Γ ρ l r → LookupQ Γ ρ (.lab l) r
  | var {ρ : Row B} {α : TyVar} {r : LookupRes B} :
      LookupV Γ ρ α r → LookupQ Γ ρ (.var α) r
  -- L-junk
  | junk {ρ : Row B} {q : Ty B} : ¬ q.IsQuery → LookupQ Γ ρ q .absent

@[simp] theorem LookupQ.lab_iff {Γ : Ctx B} {ρ : Row B} {l : Label} {r : LookupRes B} :
    LookupQ Γ ρ (.lab l) r ↔ Lookup Γ ρ l r :=
  ⟨fun | .lit h => h | .junk h => absurd trivial h, .lit⟩

@[simp] theorem LookupQ.var_iff {Γ : Ctx B} {ρ : Row B} {α : TyVar} {r : LookupRes B} :
    LookupQ Γ ρ (.var α) r ↔ LookupV Γ ρ α r :=
  ⟨fun | .var h => h | .junk h => absurd trivial h, .var⟩


------------------------------ LookupV METATHEORY ------------------------------

-- ⊢  a label variable never FINDS a type in a literal row
theorem LookupV.not_found {Γ : Ctx B} {ρ : Row B} {α : TyVar} {τ : Ty B} :
    ¬ LookupV Γ ρ α (.found τ) := by
  intro h
  generalize hr : LookupRes.found τ = r at h
  induction h with
  | emp => cases hr
  | sing => cases hr
  | var _ _ ih => exact ih hr
  | varFree => cases hr
  | catSkip _ _ _ ih₂ => exact ih₂ hr
  | catUnk => cases hr

theorem LookupV.det {Γ : Ctx B} {ρ : Row B} {α : TyVar} {r₁ r₂ : LookupRes B}
    (h₁ : LookupV Γ ρ α r₁) (h₂ : LookupV Γ ρ α r₂) : r₁ = r₂ := by
  induction h₁ generalizing r₂ with
  | emp => cases h₂; rfl
  | sing => cases h₂; rfl
  | var hΓ _ ih =>
      cases h₂ with
      | var hΓ' h' =>
          rw [hΓ] at hΓ'; injection hΓ' with heq; exact ih (heq ▸ h')
      | varFree hΓ' => rw [hΓ] at hΓ'; cases hΓ'
  | varFree hΓ =>
      cases h₂ with
      | var hΓ' _ => rw [hΓ] at hΓ'; cases hΓ'
      | varFree _ => rfl
  | catSkip _ _ ih₁ ih₂ =>
      cases h₂ with
      | catSkip _ h' => exact ih₂ h'
      | catUnk h' => cases ih₁ h'
  | catUnk _ ih =>
      cases h₂ with
      | catSkip h' _ => cases ih h'
      | catUnk _ => rfl

theorem LookupQ.det {Γ : Ctx B} {ρ : Row B} {q : Ty B} {r₁ r₂ : LookupRes B}
    (h₁ : LookupQ Γ ρ q r₁) (h₂ : LookupQ Γ ρ q r₂) : r₁ = r₂ := by
  cases h₁ with
  | lit h =>
      cases h₂ with
      | lit h' => exact lookup_det h h'
      | junk h' => exact absurd trivial h'
  | var h =>
      cases h₂ with
      | var h' => exact h.det h'
      | junk h' => exact absurd trivial h'
  | junk h =>
      cases h₂ with
      | lit _ => exact absurd trivial h
      | var _ => exact absurd trivial h
      | junk _ => rfl

-- ⊢  definite results survive extending the row-solutions
theorem LookupV.mono {Γ Γ' : Ctx B} {ρ : Row B} {α : TyVar} {r : LookupRes B}
    (hext : Ctx.RowExt Γ Γ') (h : LookupV Γ ρ α r) (hr : r ≠ .unknown) :
    LookupV Γ' ρ α r := by
  induction h with
  | emp => exact .emp
  | sing => exact absurd rfl hr
  | var hΓ _ ih => exact .var (hext _ _ hΓ) (ih hr)
  | varFree _ => exact absurd rfl hr
  | catSkip _ _ ih₁ ih₂ => exact .catSkip (ih₁ (by intro h; cases h)) (ih₂ hr)
  | catUnk _ _ => exact absurd rfl hr

theorem LookupQ.mono {Γ Γ' : Ctx B} {ρ : Row B} {q : Ty B} {r : LookupRes B}
    (hext : Ctx.RowExt Γ Γ') (h : LookupQ Γ ρ q r) (hr : r ≠ .unknown) :
    LookupQ Γ' ρ q r := by
  cases h with
  | lit h => exact .lit (lookup_mono hext h hr)
  | var h => exact .var (h.mono hext hr)
  | junk h => exact .junk h

private theorem lookupV_total_go {Γ : Ctx B} (rank : TyVar → Nat)
    (hrank : ∀ α ρ, Γ.lookupRow α = some ρ → ρ.rankUnder rank < rank α)
    (ρ : Row B) (α : TyVar) : ∃ r, LookupV Γ ρ α r :=
  match ρ with
  | .empty => ⟨_, .emp⟩
  | .sing _ _ => ⟨_, .sing⟩
  | .cat ρ₁ ρ₂ =>
      match lookupV_total_go rank hrank ρ₁ α with
      | ⟨.found _, hr₁⟩ => absurd hr₁ LookupV.not_found
      | ⟨.absent, hr₁⟩ =>
          match lookupV_total_go rank hrank ρ₂ α with
          | ⟨_, hr₂⟩ => ⟨_, .catSkip hr₁ hr₂⟩
      | ⟨.unknown, hr₁⟩ => ⟨_, .catUnk hr₁⟩
  | .var β =>
      match hβ : Γ.lookupRow β with
      | none => ⟨_, .varFree hβ⟩
      | some ρ' =>
          match lookupV_total_go rank hrank ρ' α with
          | ⟨_, hr⟩ => ⟨_, .var hβ hr⟩
termination_by (ρ.rankUnder rank, sizeOf ρ)
decreasing_by
  · simp only [Prod.lex_def, Row.rankUnder]; simp; omega
  · simp only [Prod.lex_def, Row.rankUnder]; simp; omega
  · have := hrank _ _ hβ
    simp only [Prod.lex_def, Row.rankUnder]
    omega

theorem LookupV.total {Γ : Ctx B} (hwf : Γ.RowWF) (ρ : Row B) (α : TyVar) :
    ∃ r, LookupV Γ ρ α r :=
  match hwf with
  | ⟨rank, hrank⟩ => lookupV_total_go rank hrank ρ α

-- ⊢  totality: every key has a lookup (under acyclic row-solutions)
theorem LookupQ.total {Γ : Ctx B} (hwf : Γ.RowWF) (ρ : Row B) (q : Ty B) :
    ∃ r, LookupQ Γ ρ q r := by
  cases q with
  | lab l => obtain ⟨r, h⟩ := lookup_total hwf ρ l; exact ⟨r, .lit h⟩
  | var α => obtain ⟨r, h⟩ := LookupV.total hwf ρ α; exact ⟨r, .var h⟩
  | _ => exact ⟨_, .junk (by simp [Ty.IsQuery])⟩

-- ⊢  only the row-solutions are consulted
theorem LookupV.congr_rowEnv {Γ₁ Γ₂ : Ctx B}
    (hrow : ∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α) :
    {ρ : Row B} → {α : TyVar} → {r : LookupRes B} →
    LookupV Γ₁ ρ α r → LookupV Γ₂ ρ α r
  | _, _, _, .emp => .emp
  | _, _, _, .sing => .sing
  | _, _, _, .var hΓ h => .var (hrow _ ▸ hΓ) (LookupV.congr_rowEnv hrow h)
  | _, _, _, .varFree hΓ => .varFree (hrow _ ▸ hΓ)
  | _, _, _, .catSkip h₁ h₂ =>
      .catSkip (LookupV.congr_rowEnv hrow h₁) (LookupV.congr_rowEnv hrow h₂)
  | _, _, _, .catUnk h => .catUnk (LookupV.congr_rowEnv hrow h)

theorem LookupQ.congr_rowEnv {Γ₁ Γ₂ : Ctx B}
    (hrow : ∀ α, Γ₁.lookupRow α = Γ₂.lookupRow α) {ρ : Row B} {q : Ty B}
    {r : LookupRes B} : LookupQ Γ₁ ρ q r → LookupQ Γ₂ ρ q r
  | .lit h => .lit (Lookup.congr_rowEnv hrow h)
  | .var h => .var (LookupV.congr_rowEnv hrow h)
  | .junk h => .junk h


------------------------------ ROW EQUIVALENCE ---------------------------------
-- A label variable's lookup never finds, so ≈ preserves it ON THE NOSE: the
-- answer is ⊥ exactly when the spine has no field and every variable resolves
-- to such a spine, and no ≈-axiom creates or destroys a field or a variable.

section EquivHelpers
variable {Γ : Ctx B} {ρ ρ₁ ρ₂ ρ₃ : Row B} {α : TyVar} {r : LookupRes B}

private theorem lookupV_assoc_fwd :
    LookupV Γ (.cat (.cat ρ₁ ρ₂) ρ₃) α r → LookupV Γ (.cat ρ₁ (.cat ρ₂ ρ₃)) α r := by
  intro h
  cases h with
  | catSkip h₁₂ h₃ =>
      cases h₁₂ with
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catSkip h₂ h₃)
  | catUnk h₁₂ =>
      cases h₁₂ with
      | catSkip h₁ h₂ => exact .catSkip h₁ (.catUnk h₂)
      | catUnk h₁ => exact .catUnk h₁

private theorem lookupV_assoc_bwd :
    LookupV Γ (.cat ρ₁ (.cat ρ₂ ρ₃)) α r → LookupV Γ (.cat (.cat ρ₁ ρ₂) ρ₃) α r := by
  intro h
  cases h with
  | catSkip h₁ h₂₃ =>
      cases h₂₃ with
      | catSkip h₂ h₃ => exact .catSkip (.catSkip h₁ h₂) h₃
      | catUnk h₂ => exact .catUnk (.catSkip h₁ h₂)
  | catUnk h₁ => exact .catUnk (.catUnk h₁)

private theorem lookupV_unitL_fwd : LookupV Γ (.cat .empty ρ) α r → LookupV Γ ρ α r := by
  intro h
  cases h with
  | catSkip _ h₂ => exact h₂
  | catUnk hε => cases hε

private theorem lookupV_unitR_fwd : LookupV Γ (.cat ρ .empty) α r → LookupV Γ ρ α r := by
  intro h
  cases h with
  | catSkip h₁ hε => cases hε; exact h₁
  | catUnk h₁ => exact h₁

private theorem lookupV_unitR_bwd (h : LookupV Γ ρ α r) : LookupV Γ (.cat ρ .empty) α r := by
  cases r with
  | found τ => exact absurd h LookupV.not_found
  | absent => exact .catSkip h .emp
  | unknown => exact .catUnk h

end EquivHelpers

theorem LookupV.equiv_both {Γ : Ctx B} :
    {ρ₁ ρ₂ : Row B} → RowEquiv ρ₁ ρ₂ →
    (∀ {α r}, LookupV Γ ρ₁ α r → LookupV Γ ρ₂ α r) ∧
    (∀ {α r}, LookupV Γ ρ₂ α r → LookupV Γ ρ₁ α r)
  | _, _, .refl _ => ⟨id, id⟩
  | _, _, .symm h => (LookupV.equiv_both h).symm
  | _, _, .trans h₁ h₂ =>
      have ih₁ := LookupV.equiv_both h₁
      have ih₂ := LookupV.equiv_both h₂
      ⟨fun h => ih₂.1 (ih₁.1 h), fun h => ih₁.2 (ih₂.2 h)⟩
  | _, _, .sing _ => ⟨fun | .sing => .sing, fun | .sing => .sing⟩
  | _, _, .cat h₁ h₂ =>
      have ih₁ := LookupV.equiv_both h₁
      have ih₂ := LookupV.equiv_both h₂
      ⟨fun | .catSkip ha hr => .catSkip (ih₁.1 ha) (ih₂.1 hr)
           | .catUnk hu => .catUnk (ih₁.1 hu),
       fun | .catSkip ha hr => .catSkip (ih₁.2 ha) (ih₂.2 hr)
           | .catUnk hu => .catUnk (ih₁.2 hu)⟩
  | _, _, .assoc => ⟨lookupV_assoc_fwd, lookupV_assoc_bwd⟩
  | _, _, .unitL => ⟨lookupV_unitL_fwd, fun h => .catSkip .emp h⟩
  | _, _, .unitR => ⟨lookupV_unitR_fwd, lookupV_unitR_bwd⟩
  | _, _, .comm _ =>
      ⟨fun | .catUnk .sing => .catUnk .sing, fun | .catUnk .sing => .catUnk .sing⟩

-- ⊢  the query lookup respects ≈, with found types up to ≈
theorem LookupQ.equiv {Γ : Ctx B} {ρ₁ ρ₂ : Row B} {q : Ty B} {r₁ : LookupRes B}
    (heq : RowEquiv ρ₁ ρ₂) (h : LookupQ Γ ρ₁ q r₁) :
    ∃ r₂, LookupQ Γ ρ₂ q r₂ ∧ ResEquiv r₁ r₂ := by
  cases h with
  | lit h =>
      obtain ⟨r₂, h₂, he⟩ := lookup_equiv heq h
      exact ⟨r₂, .lit h₂, he⟩
  | var h => exact ⟨_, .var ((LookupV.equiv_both heq).1 h), .refl _⟩
  | junk h => exact ⟨_, .junk h, .refl _⟩


------------------------------ SUBSTITUTION ------------------------------------
-- A definite answer at an empty row environment is stable under substitution,
-- the query included — provided the query stays a query. For a variable query
-- the only definite answer is ⊥, which says the row is HOLLOW (no field, no
-- variable on its spine), and a hollow row answers ⊥ to every label.

/-- no field and no variable on the spine: ε's, concatenated. -/
inductive Row.Hollow : Row B → Prop where
  | empty : Row.Hollow .empty
  | cat {ρ₁ ρ₂ : Row B} : Row.Hollow ρ₁ → Row.Hollow ρ₂ → Row.Hollow (.cat ρ₁ ρ₂)

theorem LookupV.hollow {Γ : Ctx B} (hrow : Γ.rowEnv = []) {ρ : Row B} {α : TyVar}
    {r : LookupRes B} (h : LookupV Γ ρ α r) (hr : r = .absent) : ρ.Hollow := by
  induction h with
  | emp => exact .empty
  | sing => cases hr
  | var hΓ _ _ =>
      rw [show Γ.lookupRow _ = none by simp [Ctx.lookupRow, hrow]] at hΓ; cases hΓ
  | varFree _ => cases hr
  | catSkip _ _ ih₁ ih₂ => exact .cat (ih₁ rfl) (ih₂ hr)
  | catUnk => cases hr

theorem Row.Hollow.applySubst {ρ : Row B} (h : ρ.Hollow) (θ : TySubst B) :
    (ρ.applySubst θ).Hollow := by
  induction h with
  | empty => exact .empty
  | cat _ _ ih₁ ih₂ => exact .cat ih₁ ih₂

theorem Row.Hollow.lookup {Γ : Ctx B} {ρ : Row B} (h : ρ.Hollow) (l : Label) :
    Lookup Γ ρ l .absent := by
  induction h with
  | empty => exact .emp
  | cat _ _ ih₁ ih₂ => exact .catSkip ih₁ ih₂

theorem Row.Hollow.lookupV {Γ : Ctx B} {ρ : Row B} (h : ρ.Hollow) (α : TyVar) :
    LookupV Γ ρ α .absent := by
  induction h with
  | empty => exact .emp
  | cat _ _ ih₁ ih₂ => exact .catSkip ih₁ ih₂

theorem Row.Hollow.lookupQ {Γ : Ctx B} {ρ : Row B} (h : ρ.Hollow) (q : Ty B) :
    LookupQ Γ ρ q .absent := by
  cases q with
  | lab l => exact .lit (h.lookup l)
  | var α => exact .var (h.lookupV α)
  | _ => exact .junk (by simp [Ty.IsQuery])

-- ⊢  the twin of `lookup_applySubst`, with the key substituted too
theorem LookupQ.applySubst {Γ Γ' : Ctx B} {ρ : Row B} {q : Ty B} {r : LookupRes B}
    (hrow : Γ.rowEnv = []) (θ : TySubst B) (h : LookupQ Γ ρ q r) (hr : r ≠ .unknown) :
    LookupQ Γ' (ρ.applySubst θ) (q.applySubst θ) (r.applySubst θ) := by
  cases h with
  | lit h => exact .lit (lookup_applySubst hrow θ h hr)
  | var h =>
      cases r with
      | found τ => exact absurd h LookupV.not_found
      | unknown => exact absurd rfl hr
      | absent => exact ((h.hollow hrow rfl).applySubst θ).lookupQ _
  | junk hq =>
      refine .junk ?_
      revert hq
      cases q <;> simp [Ty.applySubst, Ty.IsQuery]


------------------------------ ≈ ON THE KEY ------------------------------------
-- The algorithm reads a key under ⟦S⟧, the declarative side under σ, and under a
-- σ satisfying S the two are only ≈-equal. That costs nothing: a query type —
-- ⌊l⌋ or a variable — is ≈-rigid, so an ≈-image of a key is the same key, and a
-- non-query stays a non-query.

theorem TyEquiv.query_inv_both :
    {τ σ : Ty B} → TyEquiv τ σ → (τ.IsQuery → σ = τ) ∧ (σ.IsQuery → τ = σ)
  | _, _, .refl _ => ⟨fun _ => rfl, fun _ => rfl⟩
  | _, _, .symm h => (TyEquiv.query_inv_both h).symm
  | _, _, .trans h₁ h₂ =>
      have ih₁ := TyEquiv.query_inv_both h₁
      have ih₂ := TyEquiv.query_inv_both h₂
      ⟨fun hq => by
          have e₁ := ih₁.1 hq
          have e₂ := ih₂.1 (e₁ ▸ hq)
          rw [e₂, e₁],
       fun hq => by
          have e₂ := ih₂.2 hq
          have e₁ := ih₁.2 (e₂ ▸ hq)
          rw [e₁, e₂]⟩
  | _, _, .fn _ _ => ⟨(fun h => nomatch h), (fun h => nomatch h)⟩
  | _, _, .rcd _ => ⟨(fun h => nomatch h), (fun h => nomatch h)⟩

-- ⊢  the keyed lookup does not see ≈ on its key
theorem LookupQ.key_equiv {Γ : Ctx B} {ρ : Row B} {q q' : Ty B} {r : LookupRes B}
    (he : TyEquiv q q') (h : LookupQ Γ ρ q r) : LookupQ Γ ρ q' r := by
  have hi := TyEquiv.query_inv_both he
  by_cases hq : q.IsQuery
  · rw [hi.1 hq]; exact h
  · have hq' : ¬ q'.IsQuery := fun h' => hq (hi.2 h' ▸ h')
    cases h with
    | lit _ => exact absurd trivial hq
    | var _ => exact absurd trivial hq
    | junk _ => exact .junk hq'

end MinimalCalculus
