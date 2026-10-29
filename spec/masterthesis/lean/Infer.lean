-- ALGORITHMIC INFERENCE  Γ; S ⊢ e ⇒ τ; S′.
--
-- `algorithmic.typ` gives the A-rules on paper; until now there was no Lean
-- definition at all, which is why `plans/inference-gap-analysis.md` lists "the
-- judgement itself" as the first missing ingredient of §B. Every downstream
-- statement — inference soundness, determinism, principality — is unwriteable
-- without it. This module writes it down.
--
-- ## A RELATION, NOT A FUNCTION, and that is deliberate
-- A function would owe three termination arguments the development does not
-- have: unification's (now PROVED, `unifyRowM_terminates`), `A-let`'s Δ-split
-- least fixpoint, and the `↝*` wake-up closure. A relation owes none of them
-- and is exactly what "inference is sound w.r.t. the declarative system" has to
-- be stated over. Determinism and totality then become theorems ABOUT the
-- relation rather than things smuggled into its definition.
--
-- ## What is filled in here that the paper leaves as prose
--  * `LookupBlocked` — the paper writes `⟦S⟧ ⊢ ρ.l ↓ ? on α`, but `Lookup`
--    records only `.unknown` and not WHICH variable blocked. A-sel-? and
--    K-repark both need the blocker.
--  * the failure policy. Every non-success verdict — clash, stuck, occurs — has
--    NO derivation, so the program is rejected. For clash that is soundness
--    (clash is proved to mean no unifier exists); for stuck/occurs it is the
--    price of a rigid ★: degrading to ★ would need a declarative ★-elimination
--    rule, and ★ has none by design.

import Qualified
import RowUnify.State
import QSubst

namespace MinimalCalculus

--------------------- ? ON α: THE BLOCKER OF AN UNKNOWN LOOKUP ----------------
-- `Lookup ρ l .unknown` says the lookup gave up; it does not say where. The
-- algorithm needs the variable, because that is what the stump is blocked on
-- and what wake-up watches. This refines the `unknown`-producing rules of
-- `Lookup` (L-α-free, L-conc-skip / L-conc-★) with the blocker threaded
-- through.

/-- `LookupBlocked ρ l α` — looking up `l` in `ρ` gets stuck at the row-variable
`α`. This is the paper's `ρ.l ↓ ? on α`.

CONTEXT-FREE, like `Lookup` itself. The `var` constructor — "chase a solved
variable; the blocker is whatever the solution blocks on" — went with `L-α`, and
`varFree`'s premise `Γ.lookupRow α = none` went with it: the rows these rules
look up have already had the solution applied, so a row variable still standing
in one is unsolved by construction. -/
inductive LookupBlocked {B : Type} : Row B → Label → TyVar → Prop where
  -- L-α-free: the lookup dies here, on α itself
  | varFree {α : TyVar} {l : Label} :
      LookupBlocked (.var α) l α
  -- L-conc-skip: the left component is definitely absent, so the right decides
  | catSkip {ρ₁ ρ₂ : Row B} {l : Label} {β : TyVar} :
      Lookup ρ₁ l .absent → LookupBlocked ρ₂ l β →
      LookupBlocked (.cat ρ₁ ρ₂) l β
  -- L-conc-★: the left component already blocks, and ‖ is left-biased
  | catUnk {ρ₁ ρ₂ : Row B} {l : Label} {β : TyVar} :
      LookupBlocked ρ₁ l β → LookupBlocked (.cat ρ₁ ρ₂) l β
  -- L-?-lab against a keyed field: a label meets `${γ}`, and only γ decides
  | dunk {γ : TyVar} {l : Label} {τ : Ty B} :
      LookupBlocked (.dsing (.var γ) τ) l γ

-- ⊢  the refinement is SOUND: a blocked lookup is an unknown lookup
theorem LookupBlocked.toLookup {B : Type} {ρ : Row B} {l : Label}
    {α : TyVar} : LookupBlocked ρ l α → Lookup ρ l .unknown
  | .varFree         => .varFree
  | .catSkip ha hb   => .catSkip ha hb.toLookup
  | .catUnk hb       => .catUnk hb.toLookup
  | .dunk            => .dunk rfl

-- ⊢  …and COMPLETE: an unknown lookup always has a blocker to name
-- Together these say `? on α` is a faithful reading of `?` — the algorithm
-- never has to invent a blocker, and never fails to find one.
theorem Lookup.unknown_blocked {B : Type} {ρ : Row B} {l : Label}
    (h : Lookup ρ l .unknown) : ∃ α, LookupBlocked ρ l α := by
  generalize hr : (LookupRes.unknown : LookupRes B) = r at h
  induction h with
  | emp => exact absurd hr (by simp)
  | hit => exact absurd hr (by simp)
  | miss _ => exact absurd hr (by simp)
  | varFree => exact ⟨_, .varFree⟩
  | catHit _ => exact absurd hr (by simp)
  | catSkip ha _ _ ihb => obtain ⟨β, hb⟩ := ihb hr; exact ⟨β, .catSkip ha hb⟩
  | catUnk _ ih => obtain ⟨β, hb⟩ := ih rfl; exact ⟨β, .catUnk hb⟩
  | dhit _ => exact absurd hr (by simp)
  | dmiss _ => exact absurd hr (by simp)
  | dunk h => obtain ⟨γ, rfl⟩ := Key.cmp_lit_undec h; exact ⟨γ, .dunk⟩

-- ⊢  the blocker is UNIQUE — `lookup_det`'s image for `? on α`, and what makes
--    "the stump is blocked on α" well defined rather than a choice
theorem LookupBlocked.det {B : Type} {ρ : Row B} {l : Label}
    {α β : TyVar} (h₁ : LookupBlocked ρ l α) (h₂ : LookupBlocked ρ l β) :
    α = β := by
  induction h₁ generalizing β with
  | varFree => cases h₂; rfl
  | catSkip ha _ ih =>
      cases h₂ with
      | catSkip _ hb' => exact ih hb'
      | catUnk hb' => exact absurd (lookup_det ha hb'.toLookup) (by simp)
  | catUnk hb ih =>
      cases h₂ with
      | catSkip ha' _ => exact absurd (lookup_det hb.toLookup ha') (by simp)
      | catUnk hb' => exact ih hb'
  | dunk => cases h₂; rfl

-- `LookupBlocked.unsolved` used to live here: "the blocker is genuinely
-- UNSOLVED in Γ", which is what made wake-up's trigger ("a solution α ≔ ρ is
-- written") the right one. It has no content left to state — a blocker is a row
-- variable of the row as given, and the row the rules look up has already had
-- the solution applied, so being unsolved is not a fact about a context but the
-- reason the variable is still there at all.


--------------------- ? ON β FOR A KEYED LOOKUP -------------------------------
-- A keyed lookup's `?` has a second source besides a row variable: a label
-- variable on one side of a key comparison (L-?-lab, and its keyed-field twin).
-- When the lookup key is a label variable α, α is the blocker (it is on every
-- undecided comparison); against a literal key the field's label variable is.

/-- `ρ.k ↓ ? on β` for a keyed lookup. -/
inductive LookupBlockedQ {B : Type} : Row B → Key → TyVar → Prop where
  | varFree {k : Key} {β : TyVar} : LookupBlockedQ (.var β) k β
  -- L-?-lab: a label-variable key meets a literal field
  | sunk {α : TyVar} {l : Label} {τ : Ty B} :
      LookupBlockedQ (.sing l τ) (.var α) α
  -- a label-variable key meets a keyed field it cannot decide against
  | dunkQ {α : TyVar} {q : Key} {τ : Ty B} :
      Key.cmp (.var α) q = .undec → LookupBlockedQ (.dsing q τ) (.var α) α
  -- a literal key meets `${γ}`
  | dunkF {l : Label} {τ : Ty B} {γ : TyVar} :
      LookupBlockedQ (.dsing (.var γ) τ) (.lit l) γ
  | catSkip {ρ₁ ρ₂ : Row B} {k : Key} {β : TyVar} :
      LookupQ ρ₁ k .absent → LookupBlockedQ ρ₂ k β →
      LookupBlockedQ (.cat ρ₁ ρ₂) k β
  | catUnk {ρ₁ ρ₂ : Row B} {k : Key} {β : TyVar} :
      LookupBlockedQ ρ₁ k β → LookupBlockedQ (.cat ρ₁ ρ₂) k β

/-- the label-variable instance. -/
abbrev LookupVBlocked {B : Type} (ρ : Row B) (α β : TyVar) : Prop :=
  LookupBlockedQ ρ (.var α) β

-- ⊢  sound: a blocked keyed lookup is an unknown one
theorem LookupBlockedQ.toLookupQ {B : Type} {ρ : Row B} {q : Key}
    {β : TyVar} : LookupBlockedQ ρ q β → LookupQ ρ q .unknown
  | .varFree => .varFree
  | .sunk => .sunk rfl
  | .dunkQ h => .dunk h
  | .dunkF => .dunk rfl
  | .catSkip ha hb => .catSkip ha hb.toLookupQ
  | .catUnk hb => .catUnk hb.toLookupQ

theorem LookupVBlocked.toLookupV {B : Type} {ρ : Row B} {α β : TyVar}
    (h : LookupVBlocked ρ α β) : LookupV ρ α .unknown := h.toLookupQ

-- ⊢  complete: an unknown keyed lookup always names its blocker
theorem LookupQ.unknown_blocked {B : Type} {ρ : Row B} {q : Key}
    (h : LookupQ ρ q .unknown) : ∃ β, LookupBlockedQ ρ q β := by
  generalize hr : (LookupRes.unknown : LookupRes B) = r at h
  induction h with
  | emp => exact absurd hr (by simp)
  | hit _ => exact absurd hr (by simp)
  | miss _ => exact absurd hr (by simp)
  | sunk h => obtain ⟨α, rfl⟩ := Key.cmp_undec_lit h; exact ⟨α, .sunk⟩
  | varFree => exact ⟨_, .varFree⟩
  | catHit _ => exact absurd hr (by simp)
  | catSkip ha _ _ ihb => obtain ⟨β, hb⟩ := ihb hr; exact ⟨β, .catSkip ha hb⟩
  | catUnk _ ih => obtain ⟨β, hb⟩ := ih rfl; exact ⟨β, .catUnk hb⟩
  | dhit _ => exact absurd hr (by simp)
  | dmiss _ => exact absurd hr (by simp)
  | @dunk k q τ h =>
      cases k with
      | var α => exact ⟨α, .dunkQ h⟩
      | lit l =>
          obtain ⟨γ, rfl⟩ := Key.cmp_lit_undec h
          exact ⟨γ, .dunkF⟩

theorem LookupV.unknown_blocked {B : Type} {ρ : Row B} {α : TyVar}
    (h : LookupV ρ α .unknown) : ∃ β, LookupVBlocked ρ α β :=
  LookupQ.unknown_blocked h

-- ⊢  the blocker is UNIQUE
theorem LookupBlockedQ.det {B : Type} {ρ : Row B} {q : Key}
    {β γ : TyVar} (h₁ : LookupBlockedQ ρ q β) (h₂ : LookupBlockedQ ρ q γ) :
    β = γ := by
  induction h₁ generalizing γ with
  | varFree => cases h₂; rfl
  | sunk => cases h₂; rfl
  | dunkQ _ => cases h₂; rfl
  | dunkF => cases h₂; rfl
  | catSkip ha _ ih =>
      cases h₂ with
      | catSkip _ hb' => exact ih hb'
      | catUnk hb' => exact absurd (ha.det hb'.toLookupQ) (by simp)
  | catUnk hb ih =>
      cases h₂ with
      | catSkip ha' _ => exact absurd (hb.toLookupQ.det ha') (by simp)
      | catUnk hb' => exact ih hb'

theorem LookupVBlocked.det {B : Type} {ρ : Row B} {α β γ : TyVar}
    (h₁ : LookupVBlocked ρ α β) (h₂ : LookupVBlocked ρ α γ) : β = γ :=
  LookupBlockedQ.det h₁ h₂

-- ⊢  a literal key blocks exactly where the label relation does
theorem LookupBlockedQ.lit {B : Type} {ρ : Row B} {l : Label} {β : TyVar} :
    LookupBlocked ρ l β → LookupBlockedQ ρ (.lit l) β
  | .varFree => .varFree
  | .catSkip ha hb => .catSkip (LookupQ.lab_iff.mpr ha) (LookupBlockedQ.lit hb)
  | .catUnk hb => .catUnk (LookupBlockedQ.lit hb)
  | .dunk => .dunkF

theorem LookupBlockedQ.toLit {B : Type} {ρ : Row B} {l : Label} {β : TyVar}
    (h : LookupBlockedQ ρ (.lit l) β) : LookupBlocked ρ l β := by
  generalize hk : (Key.lit l) = k at h
  induction h with
  | varFree => exact .varFree
  | sunk => cases hk
  | dunkQ _ => cases hk
  | dunkF => exact .dunk
  | catSkip ha _ ih => subst hk; exact .catSkip (LookupQ.lab_iff.mp ha) (ih rfl)
  | catUnk _ ih => exact .catUnk (ih hk)

/-- the name a W-flag records for a key: the label itself when it is known. -/
def Key.keyName : Key → Label
  | .lit l => l
  | .var α => "${" ++ α ++ "}"

/-- the name a W-flag records for a base-typed (non-label) key. -/
def Key.baseName : Label := "${𝓫}"

/-- a type's base, when it is one: the test A-sel-dyn-𝓫 / A-rcd-dyn-𝓫 run. -/
def Ty.asBase {B : Type} : Ty B → Option B
  | .base b => some b
  | _       => none

theorem Ty.asBase_eq_some {B : Type} {τ : Ty B} {b : B} (h : τ.asBase = some b) :
    τ = .base b := by
  cases τ <;> simp_all [Ty.asBase]


--------------------- THE SOLVER STATE  S := (θ, Δ, W) ------------------------
-- `algorithmic.typ`: "θ is only ever refined"; Δ holds the parked selections;
-- W collects definite-absence flags and "never affects
-- typing, only diagnostics". The supply is threaded here rather than left as
-- the prose's `fresh α: κ` — that is the gap §B lists as "name supply in the
-- inference rules".
--
--------------------- SORTS OF INVENTED VARIABLES ----------------------------
-- The paper draws `fresh α: κ` and `A-let` reads `κ̄ = Γ(ᾱ)`. Neither was
-- expressible here: `Supply` hands out bare names, and ᾱ are exactly the
-- variables NOT in Γ — so Γ cannot be the source of their kinds, and the paper's
-- side condition is, read literally, about an environment that does not contain
-- them. The mechanization's answer: the kinds come from the DRAW. Every invented
-- name is drawn AT a kind, the state records it, and `κ̄ = Γ(ᾱ)` becomes
-- `κ̄ = S(ᾱ)` — `KEnv.Assigns`, the premise `A-let` now carries.
--
-- Nothing downstream is forced to agree with the record yet: that a name drawn
-- at `.row` only ever OCCURS at row positions is `KindsSound` below, stated
-- against `sortedFtv`'s tags and left open.

/-- κ — the sort a variable inhabits. `Ty/Row.sortedFtv` (RowUnify/State.lean)
already tags OCCURRENCES with a `Bool`, `true` = row; `Kind.tag` is that same
convention, so a recorded kind and an observed occurrence are comparable. -/
inductive Kind where
  | ty
  | row
  | lab
  deriving DecidableEq, Repr

def Kind.tag : Kind → Srt
  | .ty  => .ty
  | .row => .row
  | .lab => .lab

/-- κ̄ — what each invented name was drawn at. Newest first; `draw` only ever
CONSES, which is what `Infer.kinds_mono` says. -/
abbrev KEnv := List (TyVar × Kind)

def KEnv.lookup (K : KEnv) (α : TyVar) : Option Kind :=
  (K.find? (·.1 == α)).map Prod.snd

/-- the domain: the names the state has committed to a kind. -/
def KEnv.dom (K : KEnv) : List TyVar := K.map Prod.fst

/-- `κ̄ = K(ᾱ)` — the side condition `A-let` could not write. Every binder has a
recorded kind, and κ̄ lists them in the binders' own order. -/
def KEnv.Assigns (K : KEnv) (vs : List TyVar) (ks : List Kind) : Prop :=
  vs.map K.lookup = ks.map some


/-- A parked selection `⟨α ⊳ ρ.l ↓ δ⟩`: `Stump` carries `ρ.l ↓ δ`, and the
blocker `α` is what wake-up watches. -/
structure Parked (B : Type) where
  blocker : TyVar
  stump   : Stump B

structure SolverState (B : Type) where
  sol    : Sol B            -- θ
  parked : List (Parked B)  -- Δ
  flags  : List Label       -- W — diagnostics only, never read by typing
  supply : Supply
  kinds  : KEnv := []       -- κ̄ — the sort each drawn name was drawn at

namespace SolverState

-- `SolverState.ctx` used to live here: "⟦S⟧ read as a CONTEXT — the coercion
-- `RowUnify/State.lean` supplies". There is no such reading any more. Every
-- rule below reads ⟦S⟧ as a SUBSTITUTION, which is what it already did to the
-- row (`ρ.applySubst S.subst`); dropping the context argument removes the
-- second, redundant reading and with it the whole θ ↦ rowEnv bridge.

/-- ⟦S⟧ read as a SUBSTITUTION, one step. Using `Sol.toSubst` rather than the
closure keeps these rules independent of `UnifyWF`; on a well-formed state the
two agree (`Sol.closes_of_wf`). `UnifyWF` is now proved, and every state
`SolveTy`/`SolveRow` reach from a clean one is clean (`SolveTy.clean`,
InferSound.lean), hence applied — so `toSubst` IS the closure there. -/
def subst {B : Type} (S : SolverState B) : TySubst B := S.sol.toSubst

/-- draw one fresh name AT A KIND, record the kind, and advance. The κ argument
is what makes `fresh α: κ` writable; at every call site below it is forced by
the position the name is about to be used at, which is exactly the information
that used to be left implicit. -/
def draw {B : Type} (S : SolverState B) (κ : Kind) : TyVar × SolverState B :=
  (S.supply.fresh.1,
   { S with supply := S.supply.fresh.2,
            kinds  := (S.supply.fresh.1, κ) :: S.kinds })

/-- refine θ with a newly found solution -/
def extend {B : Type} (S : SolverState B) (s : Sol B) (S' : Supply) : SolverState B :=
  { S with sol := s.comp S.sol, supply := S' }

/-- park a stump -/
def park {B : Type} (S : SolverState B) (p : Parked B) : SolverState B :=
  { S with parked := p :: S.parked }

/-- raise a diagnostic flag (W) -/
def flag {B : Type} (S : SolverState B) (l : Label) : SolverState B :=
  { S with flags := l :: S.flags }

end SolverState

--------------------- SOLVING AN EQUATION, AND THE FAILURE POLICY -------------
-- Only a SUCCESS has a rule. A clash is a hard error, and it is PROVED to mean
-- no unifier exists, so rejecting is soundness, not choice. stuck and occurs
-- are rejected as well. They used to degrade to ★ with a W-flag (A-app-degrade,
-- A-sel-degrade), but those rules had no declarative counterpart: ★ is rigid,
-- so there is no application or selection at ★ for them to be sound against.
-- Rejecting instead is sound by construction and costs only completeness;
-- the price is recorded in plans/drop-expand.md.

-- THE SUPPLY IS THE STATE'S, not unification's own. `unifyTyM` / `unifyRowM`
-- start from a LOCAL supply computed from the problem's own ftv
-- (`⟨lenBound … + 1⟩`, `localSupply`), which is right for a standalone call and
-- wrong here: it can hand back a supply BEHIND the state's, and then a later
-- `draw` re-issues a name inference already used. So these go through
-- `unifyTyF` / `unifySpineMF` with `S.supply` threaded in and out. This is the
-- concrete content of §B's "name supply in the inference rules".
--
-- …ADVANCED past the problem and ⟦S⟧'s keys first (`supplyTy`, `supplyRow`).
-- U-host draws a name and needs it NEW: to the problem, or the success is not
-- applied and the run need not terminate (`unifyM_good`, `unifyTyF_terminates`
-- take `Below`), and to ⟦S⟧'s keys, or `extend` can rebind one. Advancing
-- costs nothing and makes both hold by construction, with no invariant on
-- `S.supply`.

/-- The supply `τ ≐ τ′` is solved from: the state's, past the (substituted)
problem and ⟦S⟧'s keys. -/
def SolverState.supplyTy {B : Type} (S : SolverState B) (τ τ' : Ty B) : Supply :=
  S.supply.above (Ty.sortedFtv τ ++ Ty.sortedFtv τ' ++ S.sol.domS)

/-- …and the one `ρ ≐ᵣ ρ′` is solved from. -/
def SolverState.supplyRow {B : Type} (S : SolverState B) (ρ ρ' : Row B) : Supply :=
  S.supply.above (Row.sortedFtv ρ ++ Row.sortedFtv ρ' ++ S.sol.domS)

/-- `S ⊢ τ ≐ τ′ ⇝ S′` — the equation was solved and θ refined. -/
def SolveTy {B : Type} [DecidableEq B]
    (S : SolverState B) (τ τ' : Ty B) (S' : SolverState B) : Prop :=
  ∃ (fuel : Nat) (s : Sol B) (Sup : Supply),
    unifyTyF (S.supplyTy (τ.applySubst S.subst) (τ'.applySubst S.subst)) fuel
      (τ.applySubst S.subst) (τ'.applySubst S.subst) = .success s Sup ∧
    S' = S.extend s Sup

/-- `S ⊢ ρ ≐ᵣ ρ′ ⇝ S′`, for the row equations `A-conc` and `A-rec` emit. -/
def SolveRow {B : Type} [DecidableEq B]
    (S : SolverState B) (ρ ρ' : Row B) (S' : SolverState B) : Prop :=
  ∃ (fuel : Nat) (s : Sol B) (Sup : Supply),
    unifySpineMF (S.supplyRow (ρ.applySubst S.subst) (ρ'.applySubst S.subst))
      fuel (ρ.applySubst S.subst).toSpine (ρ'.applySubst S.subst).toSpine = .success s Sup ∧
    S' = S.extend s Sup

-- ⊢  a solved equation really is solved: the emitted solution unifies
-- The failure policy's justification, mechanized — this is why rejecting a
-- clash is soundness rather than choice.
theorem SolveTy.unifies {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') {θ : TySubst B}
    (hsat : Sol.Sat θ S'.sol) :
    ∃ s, Sol.Sat θ s ∧
      TyUnifies θ (τ.applySubst S.subst) (τ'.applySubst S.subst) := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  exact ⟨s, (Sol.Sat.comp_inv hsat).2,
    unifyM_success_sound fuel |>.1 _ _ _ hu (Sol.Sat.comp_inv hsat).2⟩

--------------------- WAKE-UP  S ⊢ q ↝ S′ ---------------------------------------
-- K-hit / K-⊥ / K-repark are D-hit / D-⊥ / D-? — "the difference is WHEN:
-- discharge fires once per instantiation, wake-up fires each time θ grows".
-- K-repark has no declarative counterpart: declaratively D-? commits to ★ at
-- once, algorithmically the lookup has merely progressed to the next variable.

/-- `S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ ↝ S′` — one wake-up step. -/
inductive Wake {B : Type} [DecidableEq B] :
    SolverState B → Parked B → SolverState B → Prop where
  -- K-hit: the lookup now lands, so δ is pinned to what it found
  | hit {S S' : SolverState B} {p : Parked B} {τ : Ty B} :
      LookupQ (p.stump.row.applySubst S.subst) (p.stump.label.applySubst S.subst) (.found τ) →
      SolveTy S p.stump.res τ S' →
      Wake S p { S' with parked := S'.parked.filter (·.stump != p.stump) }
  -- K-⊥: definite absence, so δ becomes ★ and W records the site
  | abs {S S' : SolverState B} {p : Parked B} :
      LookupQ (p.stump.row.applySubst S.subst) (p.stump.label.applySubst S.subst) .absent →
      SolveTy S p.stump.res .unk S' →
      Wake S p
        ({ S' with parked := S'.parked.filter (·.stump != p.stump) }.flag
          (p.stump.label.applySubst S.subst).keyName)
  -- K-repark: the lookup progressed to a NEW blocker; nothing is committed
  | repark {S : SolverState B} {p : Parked B} {α' : TyVar} :
      LookupBlockedQ (p.stump.row.applySubst S.subst) (p.stump.label.applySubst S.subst) α' →
      Wake S p
        (({ S with parked := S.parked.filter (·.stump != p.stump) }).park
          ⟨α', p.stump⟩)

/-- `↝*` — the reflexive-transitive closure the A-rules submit constraints to. -/
inductive Wakes {B : Type} [DecidableEq B] :
    SolverState B → List (Parked B) → SolverState B → Prop where
  | nil {S : SolverState B} : Wakes S [] S
  | cons {S S₁ S₂ : SolverState B} {p : Parked B} {ps : List (Parked B)} :
      Wake S p S₁ → Wakes S₁ ps S₂ → Wakes S (p :: ps) S₂
  -- a constraint whose lookup is still blocked is simply parked
  | park {S S₁ : SolverState B} {p : Parked B} {ps : List (Parked B)} :
      LookupBlockedQ (p.stump.row.applySubst S.subst) (p.stump.label.applySubst S.subst) p.blocker →
      Wakes (S.park p) ps S₁ → Wakes S (p :: ps) S₁

--------------------- THE STATE INVARIANT, AND SATURATION ---------------------
-- `plans/inference-gap-analysis.md` §B lists "every stump in Δ genuinely blocked
-- on its recorded blocker under ⟦S⟧" among the invariants that nothing yet
-- states, let alone maintains. `fStarEx_stale_blocker` (InferSound.lean) shows
-- what its absence costs: A-app's arrow equation can solve a parked stump's
-- BLOCKER, `Infer.var` is the only rule that runs wake-up, and the stump then
-- sits in Δ with a stale annotation, waiting on a variable that is no longer
-- there.
--
-- So the invariant gets a name, and wake-up gets run wherever a solution is
-- written — which is what `algorithmic.typ` says happens ("wake-up fires when a
-- solution α ≔ ρ is written, and only for stumps blocked on α") and what the
-- rules did not do.
--
-- WHY A RELATION, and why quiescence is a PREMISE rather than a theorem: a
-- saturation FUNCTION would owe a termination measure, and K-repark has none —
-- each repark moves the blocker to a new variable, and "the unsolved row
-- variables reachable from the stump's row" is a plausible measure that nobody
-- has written. Same reason `Infer` is a relation. What saturation can reach is
-- therefore an existence statement, and it stays on the open list:
-- `Wake`'s arms need `lookup_total` (hence `Acyclic`, hence `UnifyAcyclic`) to
-- have a step at all, and the equation a step emits can CLASH — the tension
-- case, which is a hard error by `unifyM_clash_no_unifier`, not a gap.

/-- `Quiescent S` — every parked stump is genuinely blocked on the blocker it
records, read on the row THE STATE HAS SUBSTITUTED — the only reading there is
since ↓ stopped consulting a context, and the form `Wake`, `Wakes.park` and the
A-rules all check it in. -/
def SolverState.Quiescent {B : Type} (S : SolverState B) : Prop :=
  ∀ p ∈ S.parked,
    LookupBlockedQ (p.stump.row.applySubst S.subst) (p.stump.label.applySubst S.subst) p.blocker

/-- an empty Δ is quiescent, vacuously — the state every run starts in. -/
theorem SolverState.Quiescent.nil {B : Type} {S : SolverState B}
    (h : S.parked = []) : S.Quiescent := by
  intro p hp; rw [h] at hp; exact absurd hp List.not_mem_nil

/-- quiescence travels along the updates that leave the SOLUTION alone and only
shrink Δ: `draw`, `flag`, and `A-let`'s restriction of Δ to `Δ_Γ`. -/
theorem SolverState.Quiescent.mono {B : Type} {S S' : SolverState B}
    (hsol : S'.sol = S.sol) (hsub : ∀ p ∈ S'.parked, p ∈ S.parked)
    (h : S.Quiescent) : S'.Quiescent := by
  intro p hp
  have hb := h p (hsub p hp)
  show LookupBlockedQ (p.stump.row.applySubst S'.sol.toSubst)
    (p.stump.label.applySubst S'.sol.toSubst) _
  rw [hsol]; exact hb

-- `SolverState.Quiescent.blocker_unsolved` used to live here: "in a quiescent
-- state no parked stump's blocker is SOLVED", read off `LookupBlockedQ.unsolved`
-- through the state. Both are gone for the same reason. The rules look up
-- `ρ.applySubst ⟦S⟧`, so a blocker IS a variable ⟦S⟧ left standing, and
-- "the blocker is unsolved" is true by the shape of the premise rather than a
-- lemma about a row environment. §B's invariant is discharged, not relocated.

/-- `S ⊢ Δ ↝! S′` — wake-up run to QUIESCENCE. A step is taken only on a stump
that has gone STALE (its recorded blocker no longer blocks its lookup), which is
exactly the configuration a solution write creates; `Wake`'s three arms then
resolve it (K-hit / K-⊥) or move the annotation (K-repark). -/
inductive Saturate {B : Type} [DecidableEq B] :
    SolverState B → SolverState B → Prop where
  | done {S : SolverState B} : S.Quiescent → Saturate S S
  | step {S S₁ S₂ : SolverState B} {p : Parked B} :
      p ∈ S.parked →
      ¬ LookupBlockedQ (p.stump.row.applySubst S.subst) (p.stump.label.applySubst S.subst)
          p.blocker →
      Wake S p S₁ → Saturate S₁ S₂ → Saturate S S₂

/-- ⊢  saturation ends where it says it does. This is what every A-rule below
gets to conclude about its own output state. -/
theorem Saturate.quiescent {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Saturate S S' → S'.Quiescent
  | .done h            => h
  | .step _ _ _ hsat => hsat.quiescent

/-- `S ⊢ τ ≐ τ′ ⇝! S′` — solve the equation, then wake what the solution staled.
This is the form the A-rules take, in place of bare `SolveTy`: writing a solution
and re-running wake-up is ONE step of the algorithm, not two, and packaging it
this way keeps every rule's premise count — and so every proof by recursion over
the rules — unchanged. `Wake` keeps the bare `SolveTy`: its own equation writes
at the TYPE sort only, and it is what `Saturate` is defined in terms of. -/
def SolveTySat {B : Type} [DecidableEq B]
    (S : SolverState B) (τ τ' : Ty B) (S' : SolverState B) : Prop :=
  ∃ S₁, SolveTy S τ τ' S₁ ∧ Saturate S₁ S'

/-- `S ⊢ Q ↝!* S′` — A-var's closure, then saturation. A-var is not exempt: a
`Wake` step whose δ is already solved unifies structurally and CAN write a row
solution, so the closure's own steps can stale a stump it has already parked. -/
def WakesSat {B : Type} [DecidableEq B]
    (S : SolverState B) (ps : List (Parked B)) (S' : SolverState B) : Prop :=
  ∃ S₁, Wakes S ps S₁ ∧ Saturate S₁ S'

theorem SolveTySat.quiescent {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} : SolveTySat S τ τ' S' → S'.Quiescent
  | ⟨_, _, hsat⟩ => hsat.quiescent

theorem WakesSat.quiescent {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S'.Quiescent
  | ⟨_, _, hsat⟩ => hsat.quiescent


/-- ⊢  **at a quiescent state wake-up cannot commit anything**: every parked
stump's lookup is still `?`, and `?` is neither `found` nor `absent`, so the only
step left is K-repark. That is why a run's answer can keep what is parked. -/
theorem SolverState.Quiescent.wake_no_commit {B : Type} [DecidableEq B]
    {S S' : SolverState B} {p : Parked B} (hq : S.Quiescent) (hp : p ∈ S.parked)
    (hw : Wake S p S') : S'.sol = S.sol := by
  have hb := hq p hp
  cases hw with
  | hit hl _  => exact absurd (hl.det hb.toLookupQ) (by simp)
  | abs hl _  => exact absurd (hl.det hb.toLookupQ) (by simp)
  | repark _  => rfl


--------------------- THE SPENT PROMISE ---------------------------------------
-- A-sel-? returns δ so that the position stays WRITABLE, and a use of the
-- selection's value writes to it: `(x.l) y` makes A-app emit `δ ≐ (τ_y → β)`,
-- which succeeds, because δ is an unsolved variable. The promise has then been
-- SPENT on an arrow. No instance answers it with ★, so an instance has to find
-- the field: `LetSpent` (below) asks that one can, by putting the field into
-- the blocker (`LetResults.inhabited`, LetCase.lean). That happens inside a
-- proof only; the answer keeps the stump.


--------------------- THE FORCED MERGE ----------------------------------------
-- Two parked stumps can ask the SAME lookup: `x: {p = (x.l) c; q = x.l}` parks
-- ⟨ρ.l ↓ 𝓫 → β⟩ and ⟨ρ.l ↓ δ⟩. At every instance one lookup has one answer, so
-- every instance that discharges both sends their results to ≈-equal types
-- (`merge_forced`, Finalization.lean). Unifying the results is then a forced
-- move, and it is what `LetSpent` needs: after it, the unspent stump is spent
-- alike (`Parked.spentAlike`).

/-- `S ⊢ Δ ⇝ₘ S′` — M-merge: two parked stumps with one spine and one key at S
have their results unified. Both stay parked; saturation wakes what the equation
staled. -/
inductive Merge {B : Type} [DecidableEq B] : SolverState B → SolverState B → Prop where
  | merge {S S' : SolverState B} {p q : Parked B} :
      p ∈ S.parked → q ∈ S.parked →
      (p.stump.row.applySubst S.subst).toSpine = (q.stump.row.applySubst S.subst).toSpine →
      p.stump.label.applySubst S.subst = q.stump.label.applySubst S.subst →
      SolveTySat S p.stump.res q.stump.res S' →
      Merge S S'

/-- `S ⊢ Δ ⇝ₘ* S′` — any number of merges. A merge whose equation fails is not
taken, so merging never makes a run fail. -/
inductive Merges {B : Type} [DecidableEq B] : SolverState B → SolverState B → Prop where
  | refl (S : SolverState B) : Merges S S
  | step {S S₁ S₂ : SolverState B} : Merge S S₁ → Merges S₁ S₂ → Merges S S₂

theorem Merges.quiescent {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Merges S S' → S.Quiescent → S'.Quiescent
  | .refl _, h => h
  | .step (.merge _ _ _ _ hs) hms, _ => Merges.quiescent hms hs.quiescent

--------------------- THE WITNESS SEARCH --------------------------------------
-- The answer is read at the merged state. Whether it has an instance is checked
-- (`LetSpent`) at a SCRATCH COPY of that state, which may be changed at will:
-- the change never reaches a reported type, so all it owes is that a witness
-- there is a witness of the answer (`inhabited_of_ext`, Finalization.lean). Any
-- equation, solved and saturated, will do. `runF` makes two kinds:
-- * a KEY GUESS κ ≔ ℓ. A spent stump with a variable key, `r: a: (r.${a}) c`,
--   cannot be filled, since no field is named by a variable; after the guess it
--   asks for ℓ, and `LetSpent` fills its row with ℓ.
-- * a WITNESS MERGE of two stumps on one blocker with one key. Unlike M-merge
--   it is not forced (the rows may differ before the blocker), but the filled
--   blocker answers both lookups with one field, so the witness needs it.

/-- one step of the witness search: an equation, solved and saturated. -/
inductive Guess {B : Type} [DecidableEq B] : SolverState B → SolverState B → Prop where
  | solve {S S' : SolverState B} {τ τ' : Ty B} : SolveTySat S τ τ' S' → Guess S S'

/-- the witness search: any number of steps. -/
inductive Guesses {B : Type} [DecidableEq B] : SolverState B → SolverState B → Prop where
  | refl (S : SolverState B) : Guesses S S
  | step {S S₁ S₂ : SolverState B} : Guess S S₁ → Guesses S₁ S₂ → Guesses S S₂

theorem Guesses.quiescent {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Guesses S S' → S.Quiescent → S'.Quiescent
  | .refl _, h => h
  | .step (.solve hs) hgs, _ => Guesses.quiescent hgs hs.quiescent

theorem Guesses.trans {B : Type} [DecidableEq B] {S₁ S₂ S₃ : SolverState B} :
    Guesses S₁ S₂ → Guesses S₂ S₃ → Guesses S₁ S₃
  | .refl _, h => h
  | .step hg hgs, h => .step hg (Guesses.trans hgs h)

theorem Guesses.of_merges {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Merges S S' → Guesses S S'
  | .refl _ => .refl _
  | .step (.merge _ _ _ _ hs) hms => .step (.solve hs) (Guesses.of_merges hms)

--------------------- INSTANTIATION AT A-var ---------------------------------
-- `x: ∀(ᾱ: κ̄). Q ⇒ τ ∈ Γ   fresh β̄: κ̄   S ⊢ Q[β̄/ᾱ] ↝* S′`.
--
-- The instantiation is a RENAMING of the binders — that is what `fresh β̄` says
-- — so a stump's result variable δ is renamed too and stays a variable, which
-- is what keeps the selection's result position writable.

/-- `θ` renames exactly the binders `vs`, via `f`. -/
def IsRenaming {B : Type} (θ : TySubst B) (vs : List TyVar) (f : TyVar → TyVar) : Prop :=
  θ.FixedOutside vs ∧
    ∀ α ∈ vs, θ.ty α = .var (f α) ∧ θ.row α = .var (f α) ∧ θ.lab α = .var (f α)

/-- the freshness the rules need of an instantiation: injective on the binders,
landing on names the state has not already committed to, AND on names Γ does not
mention. The last clause is Γ-freshness, which used to be unstateable for want of
an ftv of a `QCtx`; `QCtx.ftv` (Qualified.lean) supplies it. The other half of
that gap — the SORT of each drawn name, the paper's `κ̄ = Γ(ᾱ)` — is now carried
by `KEnv`/`KEnv.Assigns` and read off the draw. -/
def FreshRenaming {B : Type} (f : TyVar → TyVar) (vs : List TyVar)
    (Γ : QCtx B) (S : SolverState B) : Prop :=
  (∀ α ∈ vs, ∀ β ∈ vs, f α = f β → α = β) ∧
  (∀ α ∈ vs, f α ∉ S.sol.dom) ∧
  (∀ α ∈ vs, ∀ p ∈ S.parked, .var (f α) ≠ p.stump.res) ∧
  (∀ α ∈ vs, f α ∉ Γ.ftv)

/-- the parked images of a scheme's constraints under an instantiation. The
BLOCKERS are left free: `Wakes` determines each one, either by resolving the
constraint or by exhibiting a `LookupBlocked` witness — which is the `K-park`
rule (“compute the initial blocker”) that the paper leaves out. -/
def InstStumps {B : Type} (θ : TySubst B) (f : TyVar → TyVar)
    (Q : List (Stump B)) (ps : List (Parked B)) : Prop :=
  ps.map Parked.stump =
    Q.map (fun st => (⟨st.row.applySubst θ, st.label.applySubst θ, st.res.applySubst θ⟩ : Stump B))

--------------------- ⟦S⟧ APPLIED TO A CONTEXT -------------------------------
-- What the soundness statement needs is the whole context read under the final
-- state: θ pushed through the type environment. Lookups need nothing further —
-- they read rows that have already had θ applied.

-- `QScheme.applySubst` moved to Qualified.lean, where its capture-avoidance
-- side condition (`QScheme.Avoiding`) and what that condition buys
-- (`QCovers.forward_of_avoiding`, QSubst.lean) are stated and proved.

/-- `⟦S⟧Γ` — Γ under the state's substitution. The `rowEnv := S.sol.row` field
this used to carry ("the state's row-solutions installed as the row environment
that `Lookup` and discharge consult") has no counterpart: discharge substitutes
and then looks up, so the solution reaches it through `S.subst` on the schemes,
once, instead of through the context a second time. -/
def SolverState.applyCtx {B : Type} (S : SolverState B) (Γ : QCtx B) : QCtx B :=
  { tyEnv := Γ.tyEnv.map (fun p => (p.1, p.2.applySubst S.subst)) }

/-- the variable a stump's result READS as at S: itself while unsolved, the
variable unification aliased it to otherwise. (A result solved to a non-variable
is a spent promise and has no reading; `LetResults` excludes it.) -/
def SolverState.resVar {B : Type} (S : SolverState B) : Ty B → TyVar
  | .var δ =>
      match S.subst.ty δ with
      | .var β => β
      | _      => δ
  | τ =>
      match τ.applySubst S.subst with
      | .var β => β
      | _      => ""

/-- the scheme A-let builds: body AND constraints read under ⟦S₁⟧. Reading the
constraints raw would keep a stump's row pointing at the variable it was parked
on, not at the generalized tail that variable has since been solved to — and
then an instance's stump would chase the ORIGINAL tail, shared by all
instances. The RESULT is read too: a result aliased to β by unification must be
generalized as β, the name the body mentions (`InferRuns.lean`, the
`h = λy. g y` witness), and a spent result is generalized as the type it was
spent on. -/
def letScheme {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) (Δq : List (Parked B))
    (τ₁ : Ty B) : QScheme B :=
  ⟨ᾱ, Δq.map (fun p => (⟨p.stump.row.applySubst S₁.subst, p.stump.label.applySubst S₁.subst,
      p.stump.res.applySubst S₁.subst⟩ : Stump B)), τ₁.applySubst S₁.subst⟩

/-- a spent stump can be met by filling its blocker as a ROW: its key is a label,
and the blocker is not a key variable of the row — a label lookup blocked on a
`${γ}` field waits on γ, and no row extension decides it. -/
def Key.isLit : Key → Bool
  | .lit _ => true
  | .var _ => false

theorem Key.exists_of_isLit {k : Key} (h : k.isLit = true) : ∃ l, k = .lit l := by
  cases k <;> simp_all [Key.isLit]

def Parked.fillable {B : Type} (S : SolverState B) (p : Parked B) : Bool :=
  (p.stump.label.applySubst S.subst).isLit &&
    !((p.stump.row.applySubst S.subst).sortedFtv.contains (.lab, p.blocker))

theorem Parked.fillable_iff {B : Type} {S : SolverState B} {p : Parked B} :
    p.fillable S = true ↔ (p.stump.label.applySubst S.subst).isLit = true ∧
      (.lab, p.blocker) ∉ (p.stump.row.applySubst S.subst).sortedFtv := by
  simp [Parked.fillable]

/-- every type variable to ★; rows and keys stay -/
def TySubst.starTy {B : Type} : TySubst B := ⟨fun _ => .unk, fun γ => .var γ, fun x => .var x⟩

/-- q demands the same field as the spent p, and agrees with it once every type
variable is sent to ★: q is spent too, and the two results differ only in type
variables. One field then answers both, at the instance that sends the
generalized type variables to ★. -/
def Parked.spentAlike {B : Type} (S₁ : SolverState B) (q p : Parked B) : Prop :=
  (q.stump.res.applySubst S₁.subst).isVar = false ∧
    (q.stump.res.applySubst S₁.subst).applySubst TySubst.starTy =
      (p.stump.res.applySubst S₁.subst).applySubst TySubst.starTy

instance {B : Type} [DecidableEq B] {S₁ : SolverState B} {q p : Parked B} :
    Decidable (q.spentAlike S₁ p) := by
  unfold Parked.spentAlike; infer_instance

theorem Parked.spentAlike_refl {B : Type} {S₁ : SolverState B} {p : Parked B}
    (h : (p.stump.res.applySubst S₁.subst).isVar = false) : p.spentAlike S₁ p :=
  ⟨h, rfl⟩

/-- A spent result can be met at some instance, by extending its blocker row
with the field (`LetResults.inhabited`, LetCase.lean): its key is literal,
every stump on the same blocker is literally keyed and, if it has its key, is
spent alike (`Parked.spentAlike`), and no spent stump's blocker occurs in it. A sufficient, syntactic test for the
inhabitation of the scheme (`∃ τ₁. σ ≥ τ₁`, the T-let premise); unlike the
property itself it is closed under union (`LetAdmissible.union`). -/
def LetSpent {B : Type} (S₁ : SolverState B) (Δq : List (Parked B)) : Prop :=
  ∀ p ∈ Δq, (p.stump.res.applySubst S₁.subst).isVar = false →
      p.fillable S₁ = true ∧
      (∀ q ∈ Δq, q.blocker = p.blocker →
        q.fillable S₁ = true ∧
        (q.stump.label.applySubst S₁.subst = p.stump.label.applySubst S₁.subst →
          q.spentAlike S₁ p)) ∧
      (∀ q ∈ Δq, (q.stump.res.applySubst S₁.subst).isVar = false →
        q.blocker ∉ (p.stump.res.applySubst S₁.subst).ftv)

set_option synthInstance.maxSize 512 in
set_option synthInstance.maxHeartbeats 200000 in
instance {B : Type} [DecidableEq B] {S : SolverState B} {Δ : List (Parked B)} :
    Decidable (LetSpent S Δ) := by
  unfold LetSpent; infer_instance

/-- A-let's premise on the generalized RESULTS, read at S₁ (`QScheme.WF` and
the inhabitation of the scheme): each is a type over ᾱ — a variable, or a type
a promise was spent on — and the spent ones can be met (`LetSpent`). -/
def LetResults {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) (Δq : List (Parked B)) :
    Prop :=
  (∀ p ∈ Δq, ∀ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∈ ᾱ) ∧ LetSpent S₁ Δq

--------------------- A-let'S CHOICE OF ᾱ -------------------------------------

/-- ftv(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁): what A-let chooses ᾱ from. A variable outside it
would be generalized vacuously. -/
def letCand {B : Type} (S₁ : SolverState B) (τ₁ : Ty B) : List TyVar :=
  ((τ₁.applySubst S₁.subst).ftv ++ S₁.parked.flatMap (fun p =>
    (p.stump.row.applySubst S₁.subst).ftv ++ (p.stump.res.applySubst S₁.subst).ftv ++
      (p.stump.label.applySubst S₁.subst).ftv)).eraseDups

theorem mem_letCand_body {B : Type} {S₁ : SolverState B} {τ₁ : Ty B} {α : TyVar}
    (h : α ∈ (τ₁.applySubst S₁.subst).ftv) : α ∈ letCand S₁ τ₁ := by
  unfold letCand; rw [List.mem_eraseDups]; exact List.mem_append_left _ h

theorem mem_letCand_parked {B : Type} {S₁ : SolverState B} {τ₁ : Ty B} {p : Parked B}
    (hp : p ∈ S₁.parked) {α : TyVar}
    (h : α ∈ (p.stump.row.applySubst S₁.subst).ftv ∨ α ∈ (p.stump.res.applySubst S₁.subst).ftv ∨
      α ∈ (p.stump.label.applySubst S₁.subst).ftv) : α ∈ letCand S₁ τ₁ := by
  unfold letCand; rw [List.mem_eraseDups]
  refine List.mem_append_right _ (List.mem_flatMap.mpr ⟨p, hp, ?_⟩)
  rcases h with h | h | h <;> simp [h]

/-- Δ_q for a choice ᾱ: the stumps blocked on a generalized variable. -/
def letQ {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) : List (Parked B) :=
  S₁.parked.filter (fun p => decide (p.blocker ∈ ᾱ))

/-- Δ_Γ for a choice ᾱ: the rest. -/
def letG {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) : List (Parked B) :=
  S₁.parked.filter (fun p => !decide (p.blocker ∈ ᾱ))

theorem mem_letQ {B : Type} {S₁ : SolverState B} {ᾱ : List TyVar} {p : Parked B} :
    p ∈ letQ S₁ ᾱ ↔ p ∈ S₁.parked ∧ p.blocker ∈ ᾱ := by
  simp [letQ]

theorem mem_letG {B : Type} {S₁ : SolverState B} {ᾱ : List TyVar} {p : Parked B} :
    p ∈ letG S₁ ᾱ ↔ p ∈ S₁.parked ∧ p.blocker ∉ ᾱ := by
  simp [letG]

/-- ⊢  Δ₁ ~ Δ_q ⊎ Δ_Γ: a PARTITION, up to order. Δ₁ is ordered by parking
time, and a stump that is generalized may have been parked after one that is
not. -/
theorem letSplit {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) :
    S₁.parked.Perm (letQ S₁ ᾱ ++ letG S₁ ᾱ) :=
  (List.filter_append_perm _ _).symm

/-- ftv(⟦S₁⟧(Γ, Δ)): the environment a let must not generalize over — Γ, and
the stumps Δ that stay parked — read at S₁. Stated per variable and at all
three sorts, since `ftv` does not record which one it saw. -/
def letEnvFtv {B : Type} (Γ : QCtx B) (S₁ : SolverState B) (Δ : List (Parked B)) :
    List TyVar :=
  Γ.ftv.flatMap (fun β =>
    (S₁.subst.ty β).ftv ++ (S₁.subst.row β).ftv ++ (S₁.subst.lab β).ftv) ++
  Δ.flatMap (fun p =>
    (p.stump.row.applySubst S₁.subst).ftv ++ (p.stump.res.applySubst S₁.subst).ftv ++
      (p.stump.label.applySubst S₁.subst).ftv)

/-- ᾱ is an admissible choice at A-let (`gen` in the thesis): Δ_q and Δ_Γ are
determined by ᾱ (`letQ`, `letG`), and four conditions remain.
* `fresh`: ᾱ ∩ ftv(⟦S₁⟧(Γ, Δ_Γ)) = ∅ — the HM(X) side condition. Γ's half was
  once MISSING, and `runSound_false_unguarded_let` (LetSound.lean) is what it
  cost: `λy. let z = y in z` inferred `a → b`. Δ_Γ's half makes what stays
  parked read the same at every instance.
* `own`: Δ_q is e₁'s OWN; no stump parked before the let may be filed under
  the scheme, or nothing ever discharges it (`runSound_false_let_captures`).
* `res`: each generalized result, read at S₁, is generalized with it
  (`QScheme.WF`).
* `spent`: the spent results can be met (`LetSpent`, inhabitation).
Neither the sorts of ᾱ nor "nothing solved is generalized" are premises: the
scheme carries no sorts, and a solved variable does not occur in anything read
at S₁. -/
structure LetAdmissible {B : Type} (Γ : QCtx B) (S S₁ : SolverState B) (ᾱ : List TyVar) :
    Prop where
  fresh : ∀ α ∈ ᾱ, α ∉ letEnvFtv Γ S₁ (letG S₁ ᾱ)
  own   : ∀ p ∈ letQ S₁ ᾱ, ∀ q ∈ S.parked, p.stump ≠ q.stump
  res   : ∀ p ∈ letQ S₁ ᾱ, ∀ δ ∈ (p.stump.res.applySubst S₁.subst).ftv, δ ∈ ᾱ
  spent : LetSpent S₁ (letQ S₁ ᾱ)

theorem mem_letEnvFtv {B : Type} {Γ : QCtx B} {S₁ : SolverState B} {Δ : List (Parked B)}
    {α : TyVar} :
    α ∈ letEnvFtv Γ S₁ Δ ↔
      (∃ β ∈ Γ.ftv, α ∈ (S₁.subst.ty β).ftv ∨ α ∈ (S₁.subst.row β).ftv ∨
        α ∈ (S₁.subst.lab β).ftv) ∨
      (∃ p ∈ Δ, α ∈ (p.stump.row.applySubst S₁.subst).ftv ∨
        α ∈ (p.stump.res.applySubst S₁.subst).ftv ∨
        α ∈ (p.stump.label.applySubst S₁.subst).ftv) := by
  simp [letEnvFtv, or_assoc]

/-- ⊢  `fresh`, Γ's half: ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅. -/
theorem LetAdmissible.gfresh {B : Type} {Γ : QCtx B} {S S₁ : SolverState B}
    {ᾱ : List TyVar} (h : LetAdmissible Γ S S₁ ᾱ) :
    ∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv ∧
      α ∉ (S₁.subst.lab β).ftv := by
  intro α hα β hβ
  have := h.fresh α hα
  rw [mem_letEnvFtv] at this
  exact ⟨fun hm => this (.inl ⟨β, hβ, .inl hm⟩), fun hm => this (.inl ⟨β, hβ, .inr (.inl hm)⟩),
    fun hm => this (.inl ⟨β, hβ, .inr (.inr hm)⟩)⟩

/-- ⊢  `fresh`, Δ_Γ's half: ᾱ ∩ ftv(⟦S₁⟧Δ_Γ) = ∅ — row, key and answer. -/
theorem LetAdmissible.dis {B : Type} {Γ : QCtx B} {S S₁ : SolverState B}
    {ᾱ : List TyVar} (h : LetAdmissible Γ S S₁ ᾱ) :
    ∀ α ∈ ᾱ, ∀ p ∈ letG S₁ ᾱ, α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧
      α ∉ (p.stump.res.applySubst S₁.subst).ftv ∧
      α ∉ (p.stump.label.applySubst S₁.subst).ftv := by
  intro α hα p hp
  have := h.fresh α hα
  rw [mem_letEnvFtv] at this
  exact ⟨fun hm => this (.inr ⟨p, hp, .inl hm⟩), fun hm => this (.inr ⟨p, hp, .inr (.inl hm)⟩),
    fun hm => this (.inr ⟨p, hp, .inr (.inr hm)⟩)⟩

/-- ⊢  `fresh` from its two halves. -/
theorem LetAdmissible.fresh_of {B : Type} {Γ : QCtx B} {S₁ : SolverState B}
    {ᾱ : List TyVar}
    (hg : ∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv ∧
      α ∉ (S₁.subst.lab β).ftv)
    (hd : ∀ α ∈ ᾱ, ∀ p ∈ letG S₁ ᾱ, α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧
      α ∉ (p.stump.res.applySubst S₁.subst).ftv ∧
      α ∉ (p.stump.label.applySubst S₁.subst).ftv) :
    ∀ α ∈ ᾱ, α ∉ letEnvFtv Γ S₁ (letG S₁ ᾱ) := by
  intro α hα hm
  rcases mem_letEnvFtv.mp hm with ⟨β, hβ, h | h | h⟩ | ⟨p, hp, h | h | h⟩
  · exact (hg α hα β hβ).1 h
  · exact (hg α hα β hβ).2.1 h
  · exact (hg α hα β hβ).2.2 h
  · exact (hd α hα p hp).1 h
  · exact (hd α hα p hp).2.1 h
  · exact (hd α hα p hp).2.2 h

/-- ⊢  `res` and `spent` together are `LetResults` at Δ_q. -/
theorem LetAdmissible.results {B : Type} {Γ : QCtx B} {S S₁ : SolverState B}
    {ᾱ : List TyVar} (h : LetAdmissible Γ S S₁ ᾱ) : LetResults S₁ ᾱ (letQ S₁ ᾱ) :=
  ⟨h.res, h.spent⟩

/-- ⊢  an unsolved result reads as itself -/
theorem SolverState.subst_ty_of_not_dom {B : Type} {S : SolverState B} {δ : TyVar}
    (h : δ ∉ S.sol.dom) : S.subst.ty δ = .var δ :=
  tyLookup_not_mem _ (fun hm => h (List.mem_append_left _ (List.mem_append_left _ hm)))

theorem SolverState.resVar_of_not_dom {B : Type} {S : SolverState B} {δ : TyVar}
    (h : δ ∉ S.sol.dom) : S.resVar (.var δ) = δ := by
  simp only [SolverState.resVar, SolverState.subst_ty_of_not_dom h]

theorem SolverState.resVar_of_var {B : Type} {S : SolverState B} {δ β : TyVar}
    (h : S.subst.ty δ = .var β) : S.resVar (.var δ) = β := by
  simp only [SolverState.resVar, h]

--------------------- Γ; S ⊢ e ⇒ τ; S′ ---------------------------------------
-- One rule per term former, plus the DEGRADATION rules.
-- A clash has no rule at all: that is the hard error, and it is sound
-- because `unifyM_clash_no_unifier` proves a clash means no unifier exists.

mutual

inductive Infer {B C : Type} [DecidableEq B] (constTy : Sig B C) :
    QCtx B → SolverState B → Expr C → Ty B → SolverState B → Prop where
  -- A-cons
  | con {Γ : QCtx B} {S : SolverState B} {c : C} :
      Infer constTy Γ S (.con c) (.base (constTy c)) S
  -- A-var — IS I-inst: the instantiated constraints go to wake-up, which
  -- resolves what θ already decides and parks the rest
  | var {Γ : QCtx B} {S S' : SolverState B} {x : Var} {σ : QScheme B}
      {θ : TySubst B} {f : TyVar → TyVar} {ps : List (Parked B)} {Sup : Supply}
      {κs : List Kind} :
      Γ.lookup x = some σ →
      IsRenaming θ σ.vars f → FreshRenaming f σ.vars Γ S →
      -- the renaming is DRAWN: every new name comes from the block the supply
      -- hands out, and the supply moves past it. Without this a later `draw`
      -- could reissue one (`nameReuse_infers_unguarded`, FreshNames.lean).
      (∀ α ∈ σ.vars, ∃ k, S.supply.next ≤ k ∧ k < Sup.next ∧ f α = natName k) →
      S.supply.next ≤ Sup.next →
      -- … and it is drawn AT A KIND: each binder's kind is the one the state
      -- recorded when the binder was itself drawn (a let's ᾱ, `Assigns` in A-let),
      -- and its image inherits it. Without this A-let could never generalize an
      -- instance's names — they would have no recorded kind.
      S.kinds.Assigns σ.vars κs →
      InstStumps θ f σ.constraints ps →
      WakesSat { S with supply := Sup, kinds := (σ.vars.map f).zip κs ++ S.kinds } ps S' →
      Infer constTy Γ S (.var x) (σ.body.applySubst θ) S'
  -- A-lam
  | lam {Γ : QCtx B} {S S' : SolverState B} {x : Var} {e : Expr C} {τ : Ty B}
      {α : TyVar} {S₀ : SolverState B} :
      (α, S₀) = S.draw .ty →
      Infer constTy (Γ.bindTy x (.var α)) S₀ e τ S' →
      Infer constTy Γ S (.lam x e) (.fn (.var α) τ) S'
  -- A-app
  | app {Γ : QCtx B} {S S₁ S₂ S₃ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {β : TyVar} {S₂' : SolverState B} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      (β, S₂') = S₂.draw .ty →
      SolveTySat S₂' τ₁ (.fn τ₂ (.var β)) S₃ →
      Infer constTy Γ S (.app e₁ e₂) (.var β) S₃
  -- A-conc
  | conc {Γ : QCtx B} {S S₁ S₂ S₃ S₄ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {r₁ r₂ : TyVar} {Sa Sb : SolverState B} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      (r₁, Sa) = S₂.draw .row → (r₂, Sb) = Sa.draw .row →
      SolveTySat Sb τ₁ (.rcd (.var r₁)) S₃ →
      SolveTySat S₃ τ₂ (.rcd (.var r₂)) S₄ →
      Infer constTy Γ S (.cat e₁ e₂) (.rcd (.cat (.var r₂) (.var r₁))) S₄
  -- A-sel: the lookup lands
  | sel {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ τ' : Ty B}
      {l : Label} {r : TyVar} {S₁' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ (.rcd (.var r)) S₂ →
      -- the row is SUBSTITUTED before the lookup runs. It always was — A-sel-?
      -- below reads `(Row.var r).applySubst S₂.subst` — but A-sel and A-sel-⊥
      -- used to read the bare `.var r` and let `Lookup S₂.ctx` chase the
      -- solution instead. With ↓ context-free that reading is gone (a bare
      -- variable is always `?`), so all three arms now substitute first.
      Lookup ((Row.var r).applySubst S₂.subst) l (.found τ') →
      Infer constTy Γ S (.sel e l) τ' S₂
  -- A-sel-⊥: definite absence. ★ and a W-flag — this is where T-sel-⊥ lives
  | selAbs {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r : TyVar} {S₁' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ (.rcd (.var r)) S₂ →
      Lookup ((Row.var r).applySubst S₂.subst) l .absent →
      Infer constTy Γ S (.sel e l) .unk (S₂.flag l)
  -- A-sel-?: NOT ★. The stump-var δ keeps the position writable, so a later
  -- refinement can still fill it in — (x: x.l) must not freeze at {β} → ★
  | selUnk {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r α δ : TyVar} {S₁' S₂' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ (.rcd (.var r)) S₂ →
      LookupBlocked ((Row.var r).applySubst S₂.subst) l α →
      (δ, S₂') = S₂.draw .ty →
      Infer constTy Γ S (.sel e l) (.var δ)
        (S₂'.park ⟨α, ⟨.var r, .lit l, .var δ⟩⟩)
  -- A-lab: a label literal is its own singleton
  | lab {Γ : QCtx B} {S : SolverState B} {l : Label} :
      Infer constTy Γ S (.lab l) (.lab (.lit l)) S
  -- A-sel-dyn. The record first (forced to `{r}`), then the key, whose type is
  -- forced to ⌊κ⌋ for a fresh LABEL variable κ (a key whose type is not yet a
  -- base type: there is no junk key; a definite 𝓫 takes A-sel-dyn-𝓫). The lookup is keyed by κ read under ⟦S⟧, and its three
  -- verdicts are A-sel's, A-sel-⊥'s and A-sel-?'s
  | selDyn {Γ : QCtx B} {S S₁ S₁' S₂ S₃ S₃' S₄ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ τ' : Ty B} {r κ : TyVar} :
      Infer constTy Γ S e₁ τ₁ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ₁ (.rcd (.var r)) S₂ →
      Infer constTy Γ S₂ e₂ τ₂ S₃ →
      (κ, S₃') = S₃.draw .lab →
      SolveTySat S₃' τ₂ (.lab (.var κ)) S₄ →
      LookupQ ((Row.var r).applySubst S₄.subst) ((Key.var κ).applySubst S₄.subst)
        (.found τ') →
      Infer constTy Γ S (.selDyn e₁ e₂) τ' S₄
  | selDynAbs {Γ : QCtx B} {S S₁ S₁' S₂ S₃ S₃' S₄ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {r κ : TyVar} :
      Infer constTy Γ S e₁ τ₁ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ₁ (.rcd (.var r)) S₂ →
      Infer constTy Γ S₂ e₂ τ₂ S₃ →
      (κ, S₃') = S₃.draw .lab →
      SolveTySat S₃' τ₂ (.lab (.var κ)) S₄ →
      LookupQ ((Row.var r).applySubst S₄.subst) ((Key.var κ).applySubst S₄.subst) .absent →
      Infer constTy Γ S (.selDyn e₁ e₂) .unk
        (S₄.flag ((Key.var κ).applySubst S₄.subst).keyName)
  | selDynUnk {Γ : QCtx B} {S S₁ S₁' S₂ S₃ S₃' S₄ S₄' : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {r κ α δ : TyVar} :
      Infer constTy Γ S e₁ τ₁ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ₁ (.rcd (.var r)) S₂ →
      Infer constTy Γ S₂ e₂ τ₂ S₃ →
      (κ, S₃') = S₃.draw .lab →
      SolveTySat S₃' τ₂ (.lab (.var κ)) S₄ →
      LookupBlockedQ ((Row.var r).applySubst S₄.subst) ((Key.var κ).applySubst S₄.subst) α →
      (δ, S₄') = S₄.draw .ty →
      Infer constTy Γ S (.selDyn e₁ e₂) (.var δ)
        (S₄'.park ⟨α, ⟨.var r, .var κ, .var δ⟩⟩)
  -- A-rcd-dyn. Both halves first, then the key's type is forced to ⌊κ⌋ for a
  -- fresh LABEL variable κ, which keys the field (unless A-rcd-dyn-𝓫 fired). No
  -- lookup, so nothing parks
  | rcdDyn {Γ : QCtx B} {S S₁ S₂ S₂' S₃ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {κ : TyVar} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      (κ, S₂') = S₂.draw .lab →
      SolveTySat S₂' τ₁ (.lab (.var κ)) S₃ →
      Infer constTy Γ S (.rcdDyn e₁ e₂) (.rcd (.dsing (.var κ) τ₂)) S₃
  -- A-sel-dyn-𝓫 / A-rcd-dyn-𝓫. A key whose type is already a BASE type under ⟦S⟧
  -- answers ★ with a W-flag instead of clashing against ⌊κ⌋. Only a DEFINITE
  -- base type takes this arm: an unsolved key variable is still forced to ⌊κ⌋
  | selDynBase {Γ : QCtx B} {S S₁ S₁' S₂ S₃ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {r : TyVar} {b : B} :
      Infer constTy Γ S e₁ τ₁ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ₁ (.rcd (.var r)) S₂ →
      Infer constTy Γ S₂ e₂ τ₂ S₃ →
      τ₂.applySubst S₃.subst = .base b →
      Infer constTy Γ S (.selDyn e₁ e₂) .unk (S₃.flag Key.baseName)
  | rcdDynBase {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {b : B} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      τ₁.applySubst S₂.subst = .base b →
      Infer constTy Γ S (.rcdDyn e₁ e₂) .unk (S₂.flag Key.baseName)
  -- A-rec
  | rcd {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
      InferRec constTy Γ S ξ ρ S' →
      Infer constTy Γ S (.rcd ξ) (.rcd ρ) S'
  -- A-let. The Δ-split is a least FIXPOINT on paper; as a relation it is enough
  -- to REQUIRE the split's defining equations, which is precisely what a
  -- relation buys over a function here.
  | letE {Γ : QCtx B} {S S₁ S₁' S₂ : SolverState B} {x : Var} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {ᾱ : List TyVar} :
      Infer constTy Γ S e₁ τ₁ S₁ →
      -- M-merge: stumps that ask one lookup get one result. Forced
      -- (`merge_forced`), and what `LetSpent` needs of a field selected once
      -- spent and once unspent.
      Merges S₁ S₁' →
      -- ᾱ = gen_{Γ,S}(S₁′, τ₁): any admissible choice. Which one a function
      -- takes, the greatest, is `greatestAlpha` (LetChoice.lean).
      LetAdmissible Γ S S₁' ᾱ →
      Infer constTy (Γ.bindScheme x (letScheme S₁' ᾱ (letQ S₁' ᾱ) τ₁))
        { S₁' with parked := letG S₁' ᾱ } e₂ τ₂ S₂ →
      Infer constTy Γ S (.letE x e₁ e₂) τ₂ S₂
  -- MacroNix.
  -- A-prim: an instance of the builtin's type, the arguments left to right,
  -- then τp θ ≐ τ₁ → … → τₙ → β. The instance θ is free here: soundness needs
  -- no more. The function draws θ's names fresh, which is why the supply may
  -- advance (to `Sup`) and kinds may be recorded (`K`) before the arguments.
  | prim {Γ : QCtx B} {S S₁ S₁' S₂ : SolverState B} {p : C} {es : List (Expr C)}
      {τp : Ty B} {θ : TySubst B} {τs : List (Ty B)} {β : TyVar} {Sup : Supply}
      {K : KEnv} :
      constTy.prim p = some τp →
      S.supply.next ≤ Sup.next →
      InferArgs constTy Γ { S with supply := Sup, kinds := K ++ S.kinds } es τs S₁ →
      (β, S₁') = S₁.draw .ty →
      SolveTySat S₁' (τp.applySubst θ) (Ty.arrows τs (.var β)) S₂ →
      Infer constTy Γ S (.prim p es) (.var β) S₂
  -- A-bool
  | bool {Γ : QCtx B} {S : SolverState B} {b : Bool} {bb : B} :
      constTy.boolB = some bb →
      Infer constTy Γ S (.bool b) (.base bb) S
  -- A-if: the condition is a boolean, the branches agree
  | ite {Γ : QCtx B} {S S₁ S₂ S₃ S₄ S₅ : SolverState B} {e e₁ e₂ : Expr C}
      {τ τ₁ τ₂ : Ty B} {bb : B} :
      constTy.boolB = some bb →
      Infer constTy Γ S e τ S₁ → SolveTySat S₁ τ (.base bb) S₂ →
      Infer constTy Γ S₂ e₁ τ₁ S₃ → Infer constTy Γ S₃ e₂ τ₂ S₄ →
      SolveTySat S₄ τ₁ τ₂ S₅ →
      Infer constTy Γ S (.ite e e₁ e₂) τ₁ S₅
  -- A-assert
  | asrt {Γ : QCtx B} {S S₁ S₂ S₃ : SolverState B} {e e₂ : Expr C}
      {τ τ₂ : Ty B} {bb : B} :
      constTy.boolB = some bb →
      Infer constTy Γ S e τ S₁ → SolveTySat S₁ τ (.base bb) S₂ →
      Infer constTy Γ S₂ e₂ τ₂ S₃ →
      Infer constTy Γ S (.asrt e e₂) τ₂ S₃
  -- A-nil: a list of a fresh element type
  | nil {Γ : QCtx B} {S S' : SolverState B} {α : TyVar} :
      (α, S') = S.draw .ty →
      Infer constTy Γ S .nil (.list (.var α)) S'
  -- A-cons: the tail is a list of the head's type
  | cons {Γ : QCtx B} {S S₁ S₂ S₃ : SolverState B} {e₁ e₂ : Expr C} {τ₁ τ₂ : Ty B} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      SolveTySat S₂ τ₂ (.list τ₁) S₃ →
      Infer constTy Γ S (.cons e₁ e₂) (.list τ₁) S₃
  -- A-has: the subject is a record, the answer a boolean
  | has {Γ : QCtx B} {S S₁ S₁' S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r : TyVar} {bb : B} :
      constTy.boolB = some bb →
      Infer constTy Γ S e τ S₁ → (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ (.rcd (.var r)) S₂ →
      Infer constTy Γ S (.has e l) (.base bb) S₂

inductive InferRec {B C : Type} [DecidableEq B] (constTy : Sig B C) :
    QCtx B → SolverState B → RecBody (Expr C) → Row B → SolverState B → Prop where
  -- A-ξ-empty
  | empty {Γ : QCtx B} {S : SolverState B} :
      InferRec constTy Γ S .empty .empty S
  -- A-ξ-field
  | field {Γ : QCtx B} {S S' : SolverState B} {l : Label} {e : Expr C} {τ : Ty B} :
      Infer constTy Γ S e τ S' →
      InferRec constTy Γ S (.field l e) (.sing l τ) S'
  -- A-ξ-conc. Literal rows are spine-var-free by construction.
  | cat {Γ : QCtx B} {S S₁ S₂ : SolverState B} {ξ₁ ξ₂ : RecBody (Expr C)}
      {ρ₁ ρ₂ : Row B} :
      InferRec constTy Γ S ξ₁ ρ₁ S₁ → InferRec constTy Γ S₁ ξ₂ ρ₂ S₂ →
      InferRec constTy Γ S (.cat ξ₁ ξ₂) (.cat ρ₁ ρ₂) S₂

-- the arguments of a builtin, left to right
inductive InferArgs {B C : Type} [DecidableEq B] (constTy : Sig B C) :
    QCtx B → SolverState B → List (Expr C) → List (Ty B) → SolverState B → Prop where
  | nil {Γ : QCtx B} {S : SolverState B} : InferArgs constTy Γ S [] [] S
  | cons {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {es : List (Expr C)}
      {τ : Ty B} {τs : List (Ty B)} :
      Infer constTy Γ S e τ S₁ → InferArgs constTy Γ S₁ es τs S₂ →
      InferArgs constTy Γ S (e :: es) (τ :: τs) S₂

end


/-- ⊢  A-var at a MONOTYPE: no binders, so nothing is drawn and the instance is
the body itself. Every example derivation's variable use is this one. -/
theorem Infer.var_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {x : Var} {τ : Ty B}
    (hl : Γ.lookup x = some ⟨[], [], τ⟩) (hw : WakesSat S [] S') :
    Infer constTy Γ S (.var x) τ S' := by
  have h := Infer.var (constTy := constTy) (σ := ⟨[], [], τ⟩) (θ := TySubst.id B)
    (f := id) (ps := []) (Sup := S.supply) hl
    ⟨⟨fun _ _ => rfl, fun _ _ => rfl, fun _ _ => rfl⟩, fun _ h => absurd h List.not_mem_nil⟩
    ⟨fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil,
     fun _ h => absurd h List.not_mem_nil, fun _ h => absurd h List.not_mem_nil⟩
    (fun _ h => absurd h List.not_mem_nil) (Nat.le_refl _) (κs := []) rfl rfl hw
  rwa [Ty.applySubst_id] at h

--------------------- THE JUDGEMENT IS NOT VACUOUS ----------------------------
-- A relation is cheap to write and easy to write EMPTY. This derives the
-- motivating program through it end to end.
--
--     λx. x.l    with    α fresh for the binder, r for the record row,
--                        δ for the selection's result
--
-- and the result is `α → δ` with EXACTLY ONE stump parked, blocked on `r` and
-- writing its answer into `δ`. Compare `selQ = ∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ`
-- (Qualified.lean): the algorithm produces the shape the declarative side
-- already proved instance-closed (`selQ_instance_closed`).
--
-- Note which rule fires: A-sel-? , not A-sel-⊥. `r` is
-- unsolved at the row sort — `α ≐ {r}` binds α at the TYPE sort — so the lookup
-- blocks on `r` and the position stays writable. That is the whole point of
-- returning δ rather than ★.

private def idSubst : TySubst Unit := ⟨fun x => .var x, fun x => .var x, fun x => .var x⟩

-- The kinds the run records, newest first: δ at the type sort, the record's row
-- variable at the row sort, the λ-binder at the type sort. This is `fresh α: κ`
-- made concrete — the sorts that used to be readable only off the use sites.
private def selExKinds : KEnv :=
  [(natName 3, .ty), (natName 2, .row), (natName 1, .ty)]

theorem selEx_infers :
    Infer (B := Unit) (C := Unit) (Sig.plain fun _ => ()) QCtx.empty
      ⟨Sol.nil, [], [], ⟨1⟩, []⟩ (selEx Unit)
      (.fn (.var (natName 1)) (.var (natName 3)))
      ⟨⟨[(natName 1, .rcd (.var (natName 2)))], [], []⟩,
       [⟨natName 2, ⟨.var (natName 2), .lit "l", .var (natName 3)⟩⟩], [], ⟨4⟩, selExKinds⟩ := by
  refine Infer.lam (S₀ := ⟨Sol.nil, [], [], ⟨2⟩, [(natName 1, .ty)]⟩) rfl ?_
  refine Infer.selUnk (τ := .var (natName 1))
    (S₁ := ⟨Sol.nil, [], [], ⟨2⟩, [(natName 1, .ty)]⟩)
    (S₁' := ⟨Sol.nil, [], [], ⟨3⟩, [(natName 2, .row), (natName 1, .ty)]⟩)
    (S₂ := ⟨⟨[(natName 1, .rcd (.var (natName 2)))], [], []⟩, [], [], ⟨3⟩,
            [(natName 2, .row), (natName 1, .ty)]⟩)
    (r := natName 2) (α := natName 2) (δ := natName 3)
    ?_ rfl ?_ ?_ rfl
  · exact Infer.var_mono rfl
      ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩
  · exact ⟨_, ⟨5, ⟨[(natName 1, .rcd (.var (natName 2)))], [], []⟩, ⟨3⟩, rfl, rfl⟩,
      .done (SolverState.Quiescent.nil rfl)⟩
  · exact .varFree


-- `SolverState.applyCtx` (⟦S⟧Γ) is defined before `Infer`: A-let's
-- generalization premise reads it.

--------------------- THE STATEMENT THIS MODULE EXISTS FOR --------------------
-- Inference soundness — "the algorithm never infers a type the declarative
-- system rejects" — is `InferSound` (InferSoundA.lean), PROVED as
-- `inferSound` (LetCase.lean). The first statement of it lived here: it read
-- the conclusion at ⟦S′⟧ under a `S′.parked = []` hypothesis, which speaks
-- about the wrong state at an inner A-sel-? and so could not be carried by the
-- induction. It was removed on 2026-09-26; `InferSound.lean`'s header says
-- what replaced each piece.

--------------------- THE TOP-LEVEL ENTRY JUDGEMENT ---------------------------
-- Run inference from the empty state, merge the stumps that ask one lookup, and
-- answer the scheme A-let would build with Γ = ∅ (plans/top-level-schemes.md).
-- The entry used to decide at this point: F-hit put a spent field into its
-- blocker, F-★ sent every undecided result to ★. Both picked ONE instance of
-- this scheme, and neither pick was forced. With Γ = ∅ and a start state that
-- parks nothing, A-let's admissibility is free except for `LetSpent`, the
-- inhabitation test.
--
-- The initial supply is ⟨1⟩, not ⟨0⟩: `natName 0` is the empty string, so
-- starting at 1 keeps every invented name non-empty.

/-- ⊢  a blocker is a variable of the row or of the key it blocks. -/
theorem LookupBlockedQ.mem_ftv {B : Type} {ρ : Row B} {k : Key} {β : TyVar} :
    LookupBlockedQ ρ k β → β ∈ ρ.ftv ∨ β ∈ k.ftv
  | .varFree => .inl (by simp [Row.ftv])
  | .sunk => .inr (by simp [Key.ftv])
  | .dunkQ _ => .inr (by simp [Key.ftv])
  | .dunkF => .inl (by simp [Row.ftv, Key.ftv])
  | .catSkip _ h => (LookupBlockedQ.mem_ftv h).imp (fun h => by simp [Row.ftv, h]) id
  | .catUnk h => (LookupBlockedQ.mem_ftv h).imp (fun h => by simp [Row.ftv, h]) id

/-- `⊢ e ⇒ σ` — inference from nothing, the forced merge, and the scheme read at
the merged state with every variable bound. `LetSpent` is checked at S₃, a
scratch copy of S₂ after the witness search (`Guesses`); with nothing to guess,
S₃ = S₂. -/
def Run {B C : Type} [DecidableEq B] (constTy : Sig B C) (e : Expr C) (σ : QScheme B) : Prop :=
  ∃ τ S₁ S₂ S₃, Infer constTy QCtx.empty ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁ ∧
    Merges S₁ S₂ ∧ σ = letScheme S₂ (letCand S₂ τ) S₂.parked τ ∧
    Guesses S₂ S₃ ∧ LetSpent S₃ S₃.parked

/-- **Algorithm soundness, top-level schemes**: every instance of the answer
types the program, and the answer has one (conjuncts 1 and 2 of
`QScheme.Principal`). PROVED: `runSound` (Finalization.lean). -/
def RunSound (B C : Type) [DecidableEq B] (constTy : Sig B C) : Prop :=
  ∀ (e : Expr C) (σ : QScheme B), Run constTy e σ →
    (∀ τ, σ.Inst τ → QTyped constTy QCtx.empty e τ) ∧ ∃ τ, σ.Inst τ


--------------------- …AND IT RUNS -------------------------------------------

private def selExS1 : SolverState Unit :=
  ⟨⟨[(natName 1, .rcd (.var (natName 2)))], [], []⟩,
   [⟨natName 2, ⟨.var (natName 2), .lit "l", .var (natName 3)⟩⟩], [], ⟨4⟩, selExKinds⟩

/-- ⊢  **the entry judgement is not empty.** `λx. x.l` runs end to end: A-sel-?
parks the stump, nothing ever resolves it, and the answer keeps it as its
constraint: `∀β δ. ⟨β.l ↓ δ⟩ ⇒ {β} → δ`, which is `selQ` up to the names of
its binders (`runF_selEx_principal`, InferRuns.lean). -/
theorem selEx_runs :
    Run (B := Unit) (C := Unit) (Sig.plain fun _ => ()) (selEx Unit)
      ⟨[natName 2, natName 3], [⟨.var (natName 2), .lit "l", .var (natName 3)⟩],
       .fn (.rcd (.var (natName 2))) (.var (natName 3))⟩ :=
  ⟨_, selExS1, selExS1, selExS1, selEx_infers, .refl _, rfl, .refl _, by decide⟩


--------------------- THE SUPPLY ONLY ADVANCES -------------------------------
-- `unifyM_supply_mono` (Soundness.lean) says a successful unification never
-- hands back a supply behind the one it was given. Everything here lifts that
-- through the solver steps and then through inference itself. This is the
-- invariant "inferred variables are fresh" rests on: an A-rule that takes a
-- supply from a sub-derivation and then draws from it must not re-issue a name
-- an earlier step already used.

theorem SolverState.draw_supply {B : Type} (S : SolverState B) (κ : Kind) :
    (S.draw κ).2.supply.next = S.supply.next + 1 := rfl

theorem SolveTy.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') : S.supply.next ≤ S'.supply.next := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  exact Nat.le_trans (Supply.le_above _ _) ((unifyM_supply_mono fuel).1 _ _ _ hu)

theorem SolveRow.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ρ ρ' : Row B} (h : SolveRow S ρ ρ' S') : S.supply.next ≤ S'.supply.next := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  exact Nat.le_trans (Supply.le_above _ _) ((unifyM_supply_mono fuel).2 _ _ _ hu)

theorem Wake.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Wake S p S' → S.supply.next ≤ S'.supply.next
  | .hit _ hs    => hs.supply
  | .abs _ hs    => hs.supply
  | .repark _    => Nat.le_refl _

theorem Wakes.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : Wakes S ps S' → S.supply.next ≤ S'.supply.next
  | .nil            => Nat.le_refl _
  | .cons hw hws    => Nat.le_trans hw.supply hws.supply
  | .park _ hws     => hws.supply

theorem Saturate.supply {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Saturate S S' → S.supply.next ≤ S'.supply.next
  | .done _           => Nat.le_refl _
  | .step _ _ hw hsat => Nat.le_trans hw.supply hsat.supply

theorem SolveTySat.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} : SolveTySat S τ τ' S' → S.supply.next ≤ S'.supply.next
  | ⟨_, hs, hsat⟩ => Nat.le_trans hs.supply hsat.supply

theorem Merges.supply {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Merges S S' → S.supply.next ≤ S'.supply.next
  | .refl _ => Nat.le_refl _
  | .step (.merge _ _ _ _ hs) hms => Nat.le_trans hs.supply hms.supply

theorem WakesSat.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S.supply.next ≤ S'.supply.next
  | ⟨_, hw, hsat⟩ => Nat.le_trans hw.supply hsat.supply

--------------------- THE KIND RECORD ONLY GROWS -----------------------------
-- The `κ̄` counterpart of the supply invariant above. Only `draw` writes to
-- `kinds`, and it CONSES, so every solver step leaves the record a suffix of
-- what it becomes: nothing already recorded is dropped or rewritten. This is
-- what makes A-var's `κ̄` mean what it should — the kind a binder is
-- instantiated at is the kind it was DRAWN at, not one a later step could have
-- overwritten.

theorem SolverState.draw_kinds {B : Type} (S : SolverState B) (κ : Kind) :
    (S.draw κ).2.kinds = ((S.draw κ).1, κ) :: S.kinds := rfl

-- Solving and waking never invent a name, so they never touch κ̄.
theorem SolveTy.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') : S'.kinds = S.kinds := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h; rfl

theorem SolveRow.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ρ ρ' : Row B} (h : SolveRow S ρ ρ' S') : S'.kinds = S.kinds := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h; rfl

theorem Wake.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Wake S p S' → S'.kinds = S.kinds
  | .hit _ hs    => hs.kinds
  | .abs _ hs    => hs.kinds
  | .repark _    => rfl

theorem Wakes.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : Wakes S ps S' → S'.kinds = S.kinds
  | .nil         => rfl
  | .cons hw hws => (hws.kinds).trans hw.kinds
  | .park _ hws  => hws.kinds

theorem Saturate.kinds {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Saturate S S' → S'.kinds = S.kinds
  | .done _           => rfl
  | .step _ _ hw hsat => (hsat.kinds).trans hw.kinds

theorem SolveTySat.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} : SolveTySat S τ τ' S' → S'.kinds = S.kinds
  | ⟨_, hs, hsat⟩ => (hsat.kinds).trans hs.kinds

theorem Merges.kinds {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Merges S S' → S'.kinds = S.kinds
  | .refl _ => rfl
  | .step (.merge _ _ _ _ hs) hms => hms.kinds.trans hs.kinds

theorem WakesSat.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S'.kinds = S.kinds
  | ⟨_, hw, hsat⟩ => (hsat.kinds).trans hw.kinds

-- from `(α, S₀) = S.draw κ`, recover the extended record
private theorem draw_kind_eq {B : Type} {S S₀ : SolverState B} {α : TyVar}
    {κ : Kind} (h : (α, S₀) = S.draw κ) : S₀.kinds = (α, κ) :: S.kinds := by
  have h1 : α = (S.draw κ).1 := congrArg Prod.fst h
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  rw [h2, h1]; rfl

mutual

/-- ⊢  inference never drops or rewrites a recorded kind. -/
theorem Infer.kinds_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B} :
    Infer constTy Γ S e τ S' → S.kinds <:+ S'.kinds
  | .con => List.suffix_refl _
  | .var _ _ _ _ _ _ _ hw => hw.kinds ▸ List.suffix_append _ _
  | .lam hd hb => by
      refine List.IsSuffix.trans ?_ (Infer.kinds_mono hb)
      rw [draw_kind_eq hd]; exact List.suffix_cons _ _
  | .app h₁ h₂ hd hs => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      refine List.IsSuffix.trans (Infer.kinds_mono h₂) ?_
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .rcdDyn h₁ h₂ hd hs => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      refine List.IsSuffix.trans (Infer.kinds_mono h₂) ?_
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      refine List.IsSuffix.trans (Infer.kinds_mono h₂) ?_
      rw [hs₂.kinds, hs₁.kinds, draw_kind_eq hd₂, draw_kind_eq hd₁]
      exact List.IsSuffix.trans (List.suffix_cons _ _) (List.suffix_cons _ _)
  | .sel h₁ hd hs _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .selAbs h₁ hd hs _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.flag _ _).kinds
      simp only [SolverState.flag]
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .selUnk h₁ hd hs _ hd₂ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.park _ _).kinds
      simp only [SolverState.park]
      rw [draw_kind_eq hd₂, hs.kinds, draw_kind_eq hd]
      exact List.IsSuffix.trans (List.suffix_cons _ _) (List.suffix_cons _ _)
  | .lab => List.suffix_refl _
  | .selDyn h₁ hd hs h₂ hdk hsk _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      rw [hsk.kinds, draw_kind_eq hdk]
      refine List.IsSuffix.trans ?_ (List.suffix_cons _ _)
      refine List.IsSuffix.trans ?_ (Infer.kinds_mono h₂)
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .selDynAbs h₁ hd hs h₂ hdk hsk _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.flag _ _).kinds
      simp only [SolverState.flag]
      rw [hsk.kinds, draw_kind_eq hdk]
      refine List.IsSuffix.trans ?_ (List.suffix_cons _ _)
      refine List.IsSuffix.trans ?_ (Infer.kinds_mono h₂)
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .selDynUnk h₁ hd hs h₂ hdk hsk _ hd₂ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.park _ _).kinds
      simp only [SolverState.park]
      rw [draw_kind_eq hd₂]
      refine List.IsSuffix.trans ?_ (List.suffix_cons _ _)
      rw [hsk.kinds, draw_kind_eq hdk]
      refine List.IsSuffix.trans ?_ (List.suffix_cons _ _)
      refine List.IsSuffix.trans ?_ (Infer.kinds_mono h₂)
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .selDynBase h₁ hd hs h₂ _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.flag _ _).kinds
      simp only [SolverState.flag]
      refine List.IsSuffix.trans ?_ (Infer.kinds_mono h₂)
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .rcdDynBase h₁ h₂ _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.flag _ _).kinds
      simp only [SolverState.flag]
      exact Infer.kinds_mono h₂
  | .rcd hb => InferRec.kinds_mono hb
  | .letE (S₁ := S₁) (S₁' := S₁') h₁ hm _ h₂ => by
      have i₁ := Infer.kinds_mono h₁
      have i₂ := Infer.kinds_mono h₂
      have i₃ : S₁.kinds <:+ S₁'.kinds := hm.kinds ▸ List.suffix_refl _
      exact List.IsSuffix.trans i₁ (List.IsSuffix.trans i₃ i₂)
  | .prim (K := K) _ _ ha hd hs => by
      refine List.IsSuffix.trans (List.suffix_append K _) ?_
      refine List.IsSuffix.trans (InferArgs.kinds_mono ha) ?_
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .bool _ => List.suffix_refl _
  | .ite _ h hs₁ h₁ h₂ hs₂ => by
      have a := Infer.kinds_mono h; have b := Infer.kinds_mono h₁
      have c := Infer.kinds_mono h₂
      rw [hs₂.kinds]; rw [hs₁.kinds] at b; exact a.trans (b.trans c)
  | .asrt _ h hs h₂ => by
      have a := Infer.kinds_mono h; have b := Infer.kinds_mono h₂
      rw [hs.kinds] at b; exact a.trans b
  | .nil hd => by rw [draw_kind_eq hd]; exact List.suffix_cons _ _
  | .cons h₁ h₂ hs => by
      have a := Infer.kinds_mono h₁; have b := Infer.kinds_mono h₂
      rw [hs.kinds]; exact a.trans b
  | .has _ h hd hs => by
      refine List.IsSuffix.trans (Infer.kinds_mono h) ?_
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _

theorem InferRec.kinds_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.kinds <:+ S'.kinds
  | .empty      => List.suffix_refl _
  | .field h    => Infer.kinds_mono h
  | .cat h₁ h₂  => by
      have i₁ := InferRec.kinds_mono h₁
      have i₂ := InferRec.kinds_mono h₂
      exact List.IsSuffix.trans i₁ i₂

theorem InferArgs.kinds_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {es : List (Expr C)} {τs : List (Ty B)} :
    InferArgs constTy Γ S es τs S' → S.kinds <:+ S'.kinds
  | .nil       => List.suffix_refl _
  | .cons h ha => (Infer.kinds_mono h).trans (InferArgs.kinds_mono ha)

end

--------------------- WHAT THE RECORD DOES NOT YET BUY ------------------------
/-- **Kind soundness** — a name drawn at κ only ever OCCURS at κ-tagged
positions, read against `Ty/Row.sortedFtv`'s tags (`Kind.tag`). This is the
statement that would turn `kinds` from a bookkeeping record into a typing
discipline, and it is the sorted counterpart of the `NoCapture` leak: `a ≐ᵣ (l:a)`
is legal precisely because the row-sort `a` and the type-sort `a` are different
variables, and this says inference never confuses the two.

Named, not proved, so the target has a name.
It needs the Γ-freshness invariant (`FreshRenaming`, still not an invariant) to
rule out a drawn name colliding with one already live at the other sort. -/
def KindsSound (B C : Type) [DecidableEq B] (constTy : Sig B C) : Prop :=
  ∀ (Γ : QCtx B) (S S' : SolverState B) (e : Expr C) (τ : Ty B),
    Infer constTy Γ S e τ S' →
    ∀ α κ, S'.kinds.lookup α = some κ →
      ∀ t, (t, α) ∈ Ty.sortedFtv (τ.applySubst S'.subst) → t = κ.tag

-- from `(α, S₀) = S.draw`, recover the advanced supply
private theorem draw_eq {B : Type} {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.supply.next = S.supply.next + 1 := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  rw [h2]; rfl

mutual

/-- ⊢  inference never re-issues a name: the supply only advances. -/
theorem Infer.supply_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B} :
    Infer constTy Γ S e τ S' → S.supply.next ≤ S'.supply.next
  | .con => Nat.le_refl _
  | .var _ _ _ _ hle _ _ hw => Nat.le_trans hle hw.supply
  | .lam hd hb => by
      have := Infer.supply_mono hb
      have hd' := draw_eq hd
      omega
  | .app h₁ h₂ hd hs => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      have i₃ := hs.supply
      omega
  | .rcdDyn h₁ h₂ hd hs => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      have i₃ := hs.supply
      omega
  | .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have e₁ := draw_eq hd₁
      have e₂ := draw_eq hd₂
      have j₁ := hs₁.supply
      have j₂ := hs₂.supply
      omega
  | .sel h₁ hd hs _ => by
      have i₁ := Infer.supply_mono h₁
      have hd' := draw_eq hd
      have j := hs.supply
      omega
  | .selAbs h₁ hd hs _ => by
      have i₁ := Infer.supply_mono h₁
      have hd' := draw_eq hd
      have j := hs.supply
      show _ ≤ (SolverState.flag _ _).supply.next
      simp only [SolverState.flag]
      omega
  | .selUnk h₁ hd hs _ hd₂ => by
      have i₁ := Infer.supply_mono h₁
      have hd' := draw_eq hd
      have j := hs.supply
      have hd₂' := draw_eq hd₂
      show _ ≤ (SolverState.park _ _).supply.next
      simp only [SolverState.park]
      omega
  | .lab => Nat.le_refl _
  | .selDyn h₁ hd hs h₂ hdk hsk _ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      have j := hs.supply
      have hdk' := draw_eq hdk
      have jk := hsk.supply
      omega
  | .selDynAbs h₁ hd hs h₂ hdk hsk _ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      have j := hs.supply
      have hdk' := draw_eq hdk
      have jk := hsk.supply
      show _ ≤ (SolverState.flag _ _).supply.next
      simp only [SolverState.flag]
      omega
  | .selDynUnk h₁ hd hs h₂ hdk hsk _ hd₂ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      have j := hs.supply
      have hdk' := draw_eq hdk
      have jk := hsk.supply
      have hd₂' := draw_eq hd₂
      show _ ≤ (SolverState.park _ _).supply.next
      simp only [SolverState.park]
      omega
  | .selDynBase h₁ hd hs h₂ _ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      have j := hs.supply
      show _ ≤ (SolverState.flag _ _).supply.next
      simp only [SolverState.flag]
      omega
  | .rcdDynBase h₁ h₂ _ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      show _ ≤ (SolverState.flag _ _).supply.next
      simp only [SolverState.flag]
      omega
  | .rcd hb => InferRec.supply_mono hb
  | .letE h₁ hm _ h₂ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      exact Nat.le_trans i₁ (Nat.le_trans hm.supply i₂)
  | .prim _ hle ha hd hs => by
      have i₁ := InferArgs.supply_mono ha
      have hd' := draw_eq hd
      have i₂ := hs.supply
      simp only at i₁
      omega
  | .bool _ => Nat.le_refl _
  | .ite _ h hs₁ h₁ h₂ hs₂ => by
      have a := Infer.supply_mono h; have b := hs₁.supply
      have c := Infer.supply_mono h₁; have d := Infer.supply_mono h₂
      have e := hs₂.supply
      omega
  | .asrt _ h hs h₂ => by
      have a := Infer.supply_mono h; have b := hs.supply
      have c := Infer.supply_mono h₂
      omega
  | .nil hd => by have := draw_eq hd; omega
  | .cons h₁ h₂ hs => by
      have a := Infer.supply_mono h₁; have b := Infer.supply_mono h₂
      have c := hs.supply
      omega
  | .has _ h hd hs => by
      have a := Infer.supply_mono h; have hd' := draw_eq hd
      have b := hs.supply
      omega

theorem InferRec.supply_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.supply.next ≤ S'.supply.next
  | .empty => Nat.le_refl _
  | .field h => Infer.supply_mono h
  | .cat h₁ h₂ => Nat.le_trans (InferRec.supply_mono h₁) (InferRec.supply_mono h₂)


theorem InferArgs.supply_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {es : List (Expr C)} {τs : List (Ty B)} :
    InferArgs constTy Γ S es τs S' → S.supply.next ≤ S'.supply.next
  | .nil => Nat.le_refl _
  | .cons h ha => Nat.le_trans (Infer.supply_mono h) (InferArgs.supply_mono ha)

end


--------------------- THE SOLUTION IS ONLY EVER REFINED -----------------------
-- `algorithmic.typ`: "θ is only ever refined". Stated SEMANTICALLY — any σ
-- satisfying the later solution satisfies the earlier one — rather than as
-- `∃ t, S'.sol = t.comp S.sol`. The semantic form is transitive on the nose,
-- needs no associativity of `Sol.comp`, and is exactly what the soundness
-- induction consumes: the conclusion is stated at the FINAL state while the
-- premises were solved at intermediate ones, so every sub-derivation's
-- equations have to be replayed under the final σ.

/-- `S ⊑ S′` at the level of solutions. -/
def SolverState.SatMono {B : Type} (S S' : SolverState B) : Prop :=
  ∀ σ : TySubst B, Sol.Sat σ S'.sol → Sol.Sat σ S.sol

theorem SolverState.SatMono.refl {B : Type} (S : SolverState B) : S.SatMono S :=
  fun _ h => h

theorem SolverState.SatMono.trans {B : Type} {S₁ S₂ S₃ : SolverState B}
    (h₁ : S₁.SatMono S₂) (h₂ : S₂.SatMono S₃) : S₁.SatMono S₃ :=
  fun σ h => h₁ σ (h₂ σ h)

-- the three state updates that leave `sol` alone
theorem SolverState.SatMono.of_sol_eq {B : Type} {S S' : SolverState B}
    (h : S'.sol = S.sol) : S.SatMono S' := fun σ hs => h ▸ hs

theorem SolveTy.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') : S.SatMono S' := by
  obtain ⟨fuel, s, Sup, -, rfl⟩ := h
  exact fun σ hs => (Sol.Sat.comp_inv hs).1

theorem SolveRow.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ρ ρ' : Row B} (h : SolveRow S ρ ρ' S') : S.SatMono S' := by
  obtain ⟨fuel, s, Sup, -, rfl⟩ := h
  exact fun σ hs => (Sol.Sat.comp_inv hs).1

theorem Wake.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Wake S p S' → S.SatMono S'
  | .hit _ hs  => hs.satMono
  | .abs _ hs  => hs.satMono
  | .repark _  => SolverState.SatMono.of_sol_eq rfl

theorem Wakes.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : Wakes S ps S' → S.SatMono S'
  | .nil         => SolverState.SatMono.refl _
  | .cons hw hws => hw.satMono.trans hws.satMono
  | .park _ hws  => hws.satMono

theorem Saturate.satMono {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Saturate S S' → S.SatMono S'
  | .done _           => SolverState.SatMono.refl _
  | .step _ _ hw hsat => hw.satMono.trans hsat.satMono

theorem SolveTySat.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} : SolveTySat S τ τ' S' → S.SatMono S'
  | ⟨_, hs, hsat⟩ => hs.satMono.trans hsat.satMono

theorem Merges.satMono {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Merges S S' → S.SatMono S'
  | .refl _ => SolverState.SatMono.refl _
  | .step (.merge _ _ _ _ hs) hms => hs.satMono.trans hms.satMono

theorem WakesSat.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S.SatMono S'
  | ⟨_, hw, hsat⟩ => hw.satMono.trans hsat.satMono

private theorem draw_sol {B : Type} {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.sol = S.sol := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  rw [h2]; rfl

mutual

/-- ⊢  inference only refines the solution. -/
theorem Infer.sat_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B} :
    Infer constTy Γ S e τ S' → S.SatMono S'
  | .con => SolverState.SatMono.refl _
  | .var _ _ _ _ _ _ _ hw => hw.satMono
  | .lam hd hb =>
      (SolverState.SatMono.of_sol_eq (draw_sol hd)).trans (Infer.sat_mono hb)
  | .app h₁ h₂ hd hs =>
      ((Infer.sat_mono h₁).trans (Infer.sat_mono h₂)).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans hs.satMono)
  | .rcdDyn h₁ h₂ hd hs =>
      ((Infer.sat_mono h₁).trans (Infer.sat_mono h₂)).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans hs.satMono)
  | .conc h₁ h₂ hd₁ hd₂ hs₁ hs₂ =>
      ((Infer.sat_mono h₁).trans (Infer.sat_mono h₂)).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd₁)).trans
          ((SolverState.SatMono.of_sol_eq (draw_sol hd₂)).trans
            (hs₁.satMono.trans hs₂.satMono)))
  | .sel h₁ hd hs _ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans hs.satMono)
  | .selAbs h₁ hd hs _ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (hs.satMono.trans (SolverState.SatMono.of_sol_eq rfl)))
  | .selUnk h₁ hd hs _ hd₂ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (hs.satMono.trans
            ((SolverState.SatMono.of_sol_eq (draw_sol hd₂)).trans
              (SolverState.SatMono.of_sol_eq rfl))))
  | .lab => SolverState.SatMono.refl _
  | .selDyn h₁ hd hs h₂ hdk hsk _ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (hs.satMono.trans ((Infer.sat_mono h₂).trans
            ((SolverState.SatMono.of_sol_eq (draw_sol hdk)).trans hsk.satMono))))
  | .selDynAbs h₁ hd hs h₂ hdk hsk _ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (hs.satMono.trans ((Infer.sat_mono h₂).trans
            ((SolverState.SatMono.of_sol_eq (draw_sol hdk)).trans
              (hsk.satMono.trans (SolverState.SatMono.of_sol_eq rfl))))))
  | .selDynUnk h₁ hd hs h₂ hdk hsk _ hd₂ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (hs.satMono.trans ((Infer.sat_mono h₂).trans
            ((SolverState.SatMono.of_sol_eq (draw_sol hdk)).trans
              (hsk.satMono.trans
                ((SolverState.SatMono.of_sol_eq (draw_sol hd₂)).trans
                  (SolverState.SatMono.of_sol_eq rfl)))))))
  | .selDynBase h₁ hd hs h₂ _ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (hs.satMono.trans ((Infer.sat_mono h₂).trans
            (SolverState.SatMono.of_sol_eq rfl))))
  | .rcdDynBase h₁ h₂ _ =>
      ((Infer.sat_mono h₁).trans (Infer.sat_mono h₂)).trans
        (SolverState.SatMono.of_sol_eq rfl)
  | .rcd hb => InferRec.sat_mono hb
  | .letE h₁ hm _ h₂ => by
      refine (Infer.sat_mono h₁).trans (hm.satMono.trans ?_)
      intro σ hσ
      exact Infer.sat_mono h₂ σ hσ
  | .prim _ _ ha hd hs =>
      (SolverState.SatMono.of_sol_eq rfl).trans ((InferArgs.sat_mono ha).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans hs.satMono))
  | .bool _ => SolverState.SatMono.refl _
  | .ite _ h hs₁ h₁ h₂ hs₂ =>
      (Infer.sat_mono h).trans (hs₁.satMono.trans ((Infer.sat_mono h₁).trans
        ((Infer.sat_mono h₂).trans hs₂.satMono)))
  | .asrt _ h hs h₂ =>
      (Infer.sat_mono h).trans (hs.satMono.trans (Infer.sat_mono h₂))
  | .nil hd => SolverState.SatMono.of_sol_eq (draw_sol hd)
  | .cons h₁ h₂ hs =>
      (Infer.sat_mono h₁).trans ((Infer.sat_mono h₂).trans hs.satMono)
  | .has _ h hd hs =>
      (Infer.sat_mono h).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans hs.satMono)

theorem InferRec.sat_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.SatMono S'
  | .empty      => SolverState.SatMono.refl _
  | .field h    => Infer.sat_mono h
  | .cat h₁ h₂  => (InferRec.sat_mono h₁).trans (InferRec.sat_mono h₂)


theorem InferArgs.sat_mono {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {es : List (Expr C)} {τs : List (Ty B)} :
    InferArgs constTy Γ S es τs S' → S.SatMono S'
  | .nil => SolverState.SatMono.refl _
  | .cons h ha => (Infer.sat_mono h).trans (InferArgs.sat_mono ha)

end

--------------------- EVERY REACHABLE STATE IS QUIESCENT ----------------------
-- The invariant, earned. Before the A-rules took `SolveTySat` / `WakesSat` this
-- was FALSE, and `fStarEx_stale_blocker` (InferSound.lean) is the counterexample:
-- A-app solved a parked stump's blocker and nothing re-checked. Now every rule
-- that writes a solution ends in a saturation, so the conclusion is
-- unconditional at exactly those rules — `SolveTySat.quiescent` — and the
-- congruence rules carry it from the induction hypothesis.
--
-- WHAT THIS BUYS, beyond tidiness:
--   * no parked blocker is solved — the §B invariant, and what makes a stump's
--     annotation meaningful at all (true by the shape of the premise, which
--     reads the row under ⟦S⟧);
--   * every stump a run's answer keeps is `?` at the state it is read at
--     (`SolverState.Quiescent.wake_no_commit`), and its blocker is a variable
--     of its row or key (`LookupBlockedQ.mem_ftv`), so the scheme binds it;
--   * `A-let`'s Δ-split dispatches on `p.blocker`, so it now reads annotations
--     that are true of the state rather than possibly stale ones.

/-- a draw leaves Δ alone. -/
private theorem draw_parked {B : Type} {S S₀ : SolverState B} {α : TyVar}
    {κ : Kind} (h : (α, S₀) = S.draw κ) : S₀.parked = S.parked := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  rw [h2]; rfl

/-- `draw`, `flag` and `A-let`'s restriction of Δ, each as its own corollary:
stated at the shape the rule produces, so nothing has to unify a bare `rfl`
against two different states. -/
theorem SolverState.Quiescent.draw {B : Type} {S S₀ : SolverState B} {α : TyVar}
    {κ : Kind} (h : S.Quiescent) (hd : (α, S₀) = S.draw κ) : S₀.Quiescent :=
  SolverState.Quiescent.mono (draw_sol hd) (fun p hp => (draw_parked hd) ▸ hp) h

theorem SolverState.Quiescent.flag {B : Type} {S : SolverState B} (l : Label)
    (h : S.Quiescent) : (S.flag l).Quiescent :=
  SolverState.Quiescent.mono rfl (fun _ hp => hp) h

theorem SolverState.Quiescent.restrict {B : Type} {S : SolverState B}
    {Δ : List (Parked B)} (hsub : ∀ p ∈ Δ, p ∈ S.parked) (h : S.Quiescent) :
    ({ S with parked := Δ } : SolverState B).Quiescent :=
  SolverState.Quiescent.mono (S := S) rfl hsub h

/-- A-sel-?'s shape: park a stump whose blockedness was established at a state
with the same solution (the draw of δ sits in between). -/
theorem SolverState.Quiescent.park_draw {B : Type} {S S' : SolverState B}
    {q : Parked B} (hsol : S'.sol = S.sol) (hpar : S'.parked = S.parked)
    (hq : S.Quiescent)
    (hb : LookupBlockedQ (q.stump.row.applySubst S.subst) (q.stump.label.applySubst S.subst)
            q.blocker) :
    (S'.park q).Quiescent := by
  intro p hp
  show LookupBlockedQ (p.stump.row.applySubst S'.sol.toSubst)
    (p.stump.label.applySubst S'.sol.toSubst) _
  rw [hsol]
  rcases List.mem_cons.mp hp with rfl | hp'
  · exact hb
  · rw [hpar] at hp'; exact hq p hp'

mutual

/-- ⊢  **inference maintains the state invariant.** Every parked stump in the
state a run ends in is genuinely blocked on the blocker it records. -/
theorem Infer.quiescent {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B} :
    Infer constTy Γ S e τ S' → S.Quiescent → S'.Quiescent
  | .con, hq => hq
  -- the rules that WRITE end in a saturation, so they conclude it outright
  | .var _ _ _ _ _ _ _ hw, _ => hw.quiescent
  | .app _ _ _ hs, _ => hs.quiescent
  | .rcdDyn _ _ _ hs, _ => hs.quiescent
  | .conc _ _ _ _ _ hs₂, _ => hs₂.quiescent
  | .sel _ _ hs _, _ => hs.quiescent
  | .selAbs _ _ hs _, _ => hs.quiescent.flag _
  | .selUnk _ hd hs hb hd₂, _ =>
      SolverState.Quiescent.park_draw (draw_sol hd₂) (draw_parked hd₂)
        hs.quiescent (.lit hb)
  -- the key's inference runs from the saturated state, and carries it
  | .lab, hq => hq
  | .selDyn _ _ _ _ _ hsk _, _ => hsk.quiescent
  | .selDynAbs _ _ _ _ _ hsk _, _ => hsk.quiescent.flag _
  | .selDynUnk _ _ _ _ _ hsk hb hd₂, _ =>
      SolverState.Quiescent.park_draw (draw_sol hd₂) (draw_parked hd₂) hsk.quiescent hb
  | .selDynBase _ _ hs h₂ _, _ => (Infer.quiescent h₂ hs.quiescent).flag _
  | .rcdDynBase h₁ h₂ _, hq => (Infer.quiescent h₂ (Infer.quiescent h₁ hq)).flag _
  -- …and the rules that do not write carry it through
  | .lam hd hb, hq => Infer.quiescent hb (hq.draw hd)
  | .rcd hb, hq => InferRec.quiescent hb hq
  -- A-let: the body runs at Δ_Γ, a SUBLIST of the quiescent Δ₁
  | .letE h₁ hm _ h₂, hq => by
      refine Infer.quiescent h₂ ((hm.quiescent (Infer.quiescent h₁ hq)).restrict ?_)
      intro p hp; exact (mem_letG.mp hp).1
  | .prim _ _ _ _ hs, _ => hs.quiescent
  | .bool _, hq => hq
  | .ite _ _ _ _ _ hs₂, _ => hs₂.quiescent
  | .asrt _ _ hs h₂, _ => Infer.quiescent h₂ hs.quiescent
  | .nil hd, hq => hq.draw hd
  | .cons _ _ hs, _ => hs.quiescent
  | .has _ _ _ hs, _ => hs.quiescent

theorem InferRec.quiescent {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.Quiescent → S'.Quiescent
  | .empty, hq     => hq
  | .field h, hq   => Infer.quiescent h hq
  | .cat h₁ h₂, hq => InferRec.quiescent h₂ (InferRec.quiescent h₁ hq)


theorem InferArgs.quiescent {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {es : List (Expr C)} {τs : List (Ty B)} :
    InferArgs constTy Γ S es τs S' → S.Quiescent → S'.Quiescent
  | .nil, hq => hq
  | .cons h ha, hq => InferArgs.quiescent ha (Infer.quiescent h hq)

end

/-- ⊢  and a run starts with an empty Δ, so this is unconditional. -/
theorem Infer.quiescent_of_nil {B C : Type} [DecidableEq B] {constTy : Sig B C}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B}
    (h : Infer constTy Γ S e τ S') (hnil : S.parked = []) : S'.Quiescent :=
  h.quiescent (SolverState.Quiescent.nil hnil)


end MinimalCalculus
