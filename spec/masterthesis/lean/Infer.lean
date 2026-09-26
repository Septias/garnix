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
-- `Lookup Γ ρ l .unknown` says the lookup gave up; it does not say where. The
-- algorithm needs the variable, because that is what the stump is blocked on
-- and what wake-up watches. This refines the three `unknown`-producing rules of
-- `Lookup` (L-α-free, L-α through a solved var, L-conc-skip / L-conc-★) with
-- the blocker threaded through.

/-- `LookupBlocked Γ ρ l α` — looking up `l` in `ρ` under `Γ` gets stuck at the
unsolved row-variable `α`. This is the paper's `Γ ⊢ ρ.l ↓ ? on α`. -/
inductive LookupBlocked {B : Type} (Γ : Ctx B) : Row B → Label → TyVar → Prop where
  -- L-α-free: the lookup dies here, on α itself
  | varFree {α : TyVar} {l : Label} :
      Γ.lookupRow α = none → LookupBlocked Γ (.var α) l α
  -- L-α: chase a solved variable; the blocker is whatever the solution blocks on
  | var {α β : TyVar} {ρ : Row B} {l : Label} :
      Γ.lookupRow α = some ρ → LookupBlocked Γ ρ l β → LookupBlocked Γ (.var α) l β
  -- L-conc-skip: the left component is definitely absent, so the right decides
  | catSkip {ρ₁ ρ₂ : Row B} {l : Label} {β : TyVar} :
      Lookup Γ ρ₁ l .absent → LookupBlocked Γ ρ₂ l β →
      LookupBlocked Γ (.cat ρ₁ ρ₂) l β
  -- L-conc-★: the left component already blocks, and ‖ is left-biased
  | catUnk {ρ₁ ρ₂ : Row B} {l : Label} {β : TyVar} :
      LookupBlocked Γ ρ₁ l β → LookupBlocked Γ (.cat ρ₁ ρ₂) l β

-- ⊢  the refinement is SOUND: a blocked lookup is an unknown lookup
theorem LookupBlocked.toLookup {B : Type} {Γ : Ctx B} {ρ : Row B} {l : Label}
    {α : TyVar} : LookupBlocked Γ ρ l α → Lookup Γ ρ l .unknown
  | .varFree h        => .varFree h
  | .var h hb         => .var h hb.toLookup
  | .catSkip ha hb    => .catSkip ha hb.toLookup
  | .catUnk hb        => .catUnk hb.toLookup

-- ⊢  …and COMPLETE: an unknown lookup always has a blocker to name
-- Together these say `? on α` is a faithful reading of `?` — the algorithm
-- never has to invent a blocker, and never fails to find one.
theorem Lookup.unknown_blocked {B : Type} {Γ : Ctx B} {ρ : Row B} {l : Label}
    (h : Lookup Γ ρ l .unknown) : ∃ α, LookupBlocked Γ ρ l α := by
  generalize hr : (LookupRes.unknown : LookupRes B) = r at h
  induction h with
  | emp => exact absurd hr (by simp)
  | hit => exact absurd hr (by simp)
  | miss _ => exact absurd hr (by simp)
  | var hΓ _ ih => obtain ⟨β, hb⟩ := ih hr; exact ⟨β, .var hΓ hb⟩
  | varFree hΓ => exact ⟨_, .varFree hΓ⟩
  | catHit _ => exact absurd hr (by simp)
  | catSkip ha _ _ ihb => obtain ⟨β, hb⟩ := ihb hr; exact ⟨β, .catSkip ha hb⟩
  | catUnk _ ih => obtain ⟨β, hb⟩ := ih rfl; exact ⟨β, .catUnk hb⟩

-- ⊢  the blocker is UNIQUE — `lookup_det`'s image for `? on α`, and what makes
--    "the stump is blocked on α" well defined rather than a choice
theorem LookupBlocked.det {B : Type} {Γ : Ctx B} {ρ : Row B} {l : Label}
    {α β : TyVar} (h₁ : LookupBlocked Γ ρ l α) (h₂ : LookupBlocked Γ ρ l β) :
    α = β := by
  induction h₁ generalizing β with
  | varFree h =>
      cases h₂ with
      | varFree _ => rfl
      | var h' _ => rw [h] at h'; cases h'
  | var h _ ih =>
      cases h₂ with
      | varFree h' => rw [h] at h'; cases h'
      | var h' hb' => rw [h] at h'; injection h' with he; exact ih (he ▸ hb')
  | catSkip ha _ ih =>
      cases h₂ with
      | catSkip _ hb' => exact ih hb'
      | catUnk hb' => exact absurd (lookup_det ha hb'.toLookup) (by simp)
  | catUnk hb ih =>
      cases h₂ with
      | catSkip ha' _ => exact absurd (lookup_det hb.toLookup ha') (by simp)
      | catUnk hb' => exact ih hb'

-- ⊢  the blocker is genuinely UNSOLVED in Γ — what makes wake-up's trigger
--    ("a solution α ≔ ρ is written") the right one
theorem LookupBlocked.unsolved {B : Type} {Γ : Ctx B} {ρ : Row B} {l : Label}
    {α : TyVar} : LookupBlocked Γ ρ l α → Γ.lookupRow α = none
  | .varFree h     => h
  | .var _ hb      => hb.unsolved
  | .catSkip _ hb  => hb.unsolved
  | .catUnk hb     => hb.unsolved


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
  deriving DecidableEq, Repr

def Kind.tag : Kind → Bool
  | .ty  => false
  | .row => true

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


/-- A parked selection `⟨α ▷ ρ.l ↓ δ⟩`: `Stump` carries `ρ.l ↓ δ`, and the
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

/-- ⟦S⟧ read as a CONTEXT — the coercion `RowUnify/State.lean` supplies. -/
def ctx {B : Type} (S : SolverState B) : Ctx B := S.sol.toCtx

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

/-- `S ⊢ τ ≐ τ′ ⇝ S′` — the equation was solved and θ refined. -/
def SolveTy {B : Type} [DecidableEq B]
    (S : SolverState B) (τ τ' : Ty B) (S' : SolverState B) : Prop :=
  ∃ (fuel : Nat) (s : Sol B) (Sup : Supply),
    unifyTyF S.supply fuel (τ.applySubst S.subst) (τ'.applySubst S.subst)
      = .success s Sup ∧
    S' = S.extend s Sup

/-- `S ⊢ ρ ≐ᵣ ρ′ ⇝ S′`, for the row equations `A-conc` and `A-rec` emit. -/
def SolveRow {B : Type} [DecidableEq B]
    (S : SolverState B) (ρ ρ' : Row B) (S' : SolverState B) : Prop :=
  ∃ (fuel : Nat) (s : Sol B) (Sup : Supply),
    unifySpineMF S.supply fuel (ρ.applySubst S.subst).toSpine
      (ρ'.applySubst S.subst).toSpine = .success s Sup ∧
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
    unifyM_success_sound fuel |>.1 S.supply _ _ hu (Sol.Sat.comp_inv hsat).2⟩

--------------------- WAKE-UP  S ⊢ q ↝ S′  AND FINALIZATION -------------------
-- K-hit / K-⊥ / K-repark are D-hit / D-⊥ / D-? — "the difference is WHEN:
-- discharge fires once per instantiation, wake-up fires each time θ grows".
-- K-repark has no declarative counterpart: declaratively D-? commits to ★ at
-- once, algorithmically the lookup has merely progressed to the next variable.

/-- `S ⊢ ⟨α ▷ ρ.l ↓ δ⟩ ↝ S′` — one wake-up step. -/
inductive Wake {B : Type} [DecidableEq B] :
    SolverState B → Parked B → SolverState B → Prop where
  -- K-hit: the lookup now lands, so δ is pinned to what it found
  | hit {S S' : SolverState B} {p : Parked B} {τ : Ty B} :
      Lookup S.ctx (p.stump.row.applySubst S.subst) p.stump.label (.found τ) →
      SolveTy S (.var p.stump.res) τ S' →
      Wake S p { S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }
  -- K-⊥: definite absence, so δ becomes ★ and W records the site
  | abs {S S' : SolverState B} {p : Parked B} :
      Lookup S.ctx (p.stump.row.applySubst S.subst) p.stump.label .absent →
      SolveTy S (.var p.stump.res) .unk S' →
      Wake S p
        ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
          p.stump.label)
  -- K-repark: the lookup progressed to a NEW blocker; nothing is committed
  | repark {S : SolverState B} {p : Parked B} {α' : TyVar} :
      LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label α' →
      Wake S p
        (({ S with parked := S.parked.filter (·.stump.res != p.stump.res) }).park
          ⟨α', p.stump⟩)

/-- `↝*` — the reflexive-transitive closure the A-rules submit constraints to. -/
inductive Wakes {B : Type} [DecidableEq B] :
    SolverState B → List (Parked B) → SolverState B → Prop where
  | nil {S : SolverState B} : Wakes S [] S
  | cons {S S₁ S₂ : SolverState B} {p : Parked B} {ps : List (Parked B)} :
      Wake S p S₁ → Wakes S₁ ps S₂ → Wakes S (p :: ps) S₂
  -- a constraint whose lookup is still blocked is simply parked
  | park {S S₁ : SolverState B} {p : Parked B} {ps : List (Parked B)} :
      LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label p.blocker →
      Wakes (S.park p) ps S₁ → Wakes S (p :: ps) S₁

--------------------- THE STATE INVARIANT, AND SATURATION ---------------------
-- `plans/inference-gap-analysis.md` §B lists "every stump in Δ genuinely blocked
-- on its recorded blocker under ⟦S⟧" among the invariants that nothing yet
-- states, let alone maintains. `fStarEx_stale_blocker` (InferSound.lean) shows
-- what its absence costs: A-app's arrow equation can solve a parked stump's
-- BLOCKER, `Infer.var` is the only rule that runs wake-up, and the stump then
-- sits in Δ with a stale annotation until finalization commits it to ★ over a
-- lookup that lands.
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
records, read on the row THE STATE HAS SUBSTITUTED, which is the form `Wake` and
`Wakes.park` check it in. (On a non-idempotent solution that differs from the
ctx-chasing form `Lookup S.ctx (.var r) l` — reconciling the two is `Sol.WF`'s
job, i.e. `UnifyWF`'s; for a stump row that is a bare variable, which is all
`A-sel-?` ever parks, the two coincide.) -/
def SolverState.Quiescent {B : Type} (S : SolverState B) : Prop :=
  ∀ p ∈ S.parked,
    LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label p.blocker

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
  show LookupBlocked S'.sol.toCtx (p.stump.row.applySubst S'.sol.toSubst) _ _
  rw [hsol]; exact hb

/-- ⊢  the invariant §B asks for, in the form it is useful: in a quiescent state
no parked stump's blocker is SOLVED. `LookupBlocked.unsolved` is the content;
this is it read through the state. -/
theorem SolverState.Quiescent.blocker_unsolved {B : Type} {S : SolverState B}
    (h : S.Quiescent) : ∀ p ∈ S.parked, p.blocker ∉ S.sol.row.map Prod.fst :=
  fun p hp => Sol.lookupRow_none (h p hp).unsolved

/-- `S ⊢ Δ ↝! S′` — wake-up run to QUIESCENCE. A step is taken only on a stump
that has gone STALE (its recorded blocker no longer blocks its lookup), which is
exactly the configuration a solution write creates; `Wake`'s three arms then
resolve it (K-hit / K-⊥) or move the annotation (K-repark). -/
inductive Saturate {B : Type} [DecidableEq B] :
    SolverState B → SolverState B → Prop where
  | done {S : SolverState B} : S.Quiescent → Saturate S S
  | step {S S₁ S₂ : SolverState B} {p : Parked B} :
      p ∈ S.parked →
      ¬ LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label
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

/-- `S ⊢ q ⇓ S′` — F-★, the algorithmic moment of T-sel-★. Runs at the end of
inference and at every generalization boundary that does not carry the stump.

THE PREMISE `LookupBlocked` IS THE FIX for what `finalize_star_no_discharge`
(InferSound.lean) refuted: without it the rule had nothing to say about the
lookup, while every sibling that touches a stump's result variable states what
the lookup did first — `Wake.hit` carries its `Lookup … (.found τ)`, `Wake.abs`
its `Lookup … .absent`, `Wake.repark` its `LookupBlocked`. Declaratively
`Stump.Discharge` offers ★ only under `D-⊥` (the lookup is `⊥`, which is K-⊥'s
job) or `D-?` (it is still `?`), so `D-?` is the arm F-★ implements and `? ` is
what it has to check. It is the same condition A-sel-? establishes when it parks
the stump, and by `Finalize.of_quiescent` it is FREE at any state a run produces:
`Infer.quiescent` makes every reachable state quiescent, and quiescence is
exactly this premise for every parked stump.

`p ∈ S.parked` comes with it: finalization discharges a stump the state HOLDS,
and without it the rule would also accept a stump invented on the spot. -/
inductive Finalize {B : Type} [DecidableEq B] :
    SolverState B → Parked B → SolverState B → Prop where
  | star {S S' : SolverState B} {p : Parked B} :
      p ∈ S.parked →
      LookupBlocked S.ctx (p.stump.row.applySubst S.subst) p.stump.label
        p.blocker →
      SolveTy S (.var p.stump.res) .unk S' →
      Finalize S p
        ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
          p.stump.label)

/-- `S ⊢ Δ ⇓* S′` — the ⇓-closure `algorithmic.typ` leaves implicit: F-★ is
stated for ONE stump and the end of a run has a list of them. The counterpart of
`Wakes` for finalization; unlike `Wakes` it has no `park` arm, because
finalization is where parking stops. -/
inductive Finalizes {B : Type} [DecidableEq B] :
    SolverState B → List (Parked B) → SolverState B → Prop where
  | nil {S : SolverState B} : Finalizes S [] S
  | cons {S S₁ S₂ : SolverState B} {p : Parked B} {ps : List (Parked B)} :
      Finalize S p S₁ → Finalizes S₁ ps S₂ → Finalizes S (p :: ps) S₂

/-- ⊢  **the premise costs nothing where it is used.** At a quiescent state — and
every state a run produces is one (`Infer.quiescent`) — blockedness holds of
every parked stump, so F-★ fires on any of them whose equation succeeds. -/
theorem Finalize.of_quiescent {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} (hq : S.Quiescent) (hp : p ∈ S.parked)
    (hs : SolveTy S (.var p.stump.res) .unk S') :
    Finalize S p
      ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
        p.stump.label) :=
  .star hp (hq p hp) hs

/-- ⊢  **F-★ and K-hit / K-⊥ are now exclusive.** Whenever F-★ applies to a stump,
the only wake-up step available on it is K-repark, which commits nothing: the
premise says the lookup is `?`, and `?` is neither `found` nor `absent`. This is
the determinism the `algorithmic.typ` bullet claims, at the one place
`fStar_wake_star_disagree` refuted it. -/
theorem Finalize.wake_no_commit {B : Type} [DecidableEq B] {S S₁ S₂ : SolverState B}
    {p : Parked B} (hf : Finalize S p S₁) (hw : Wake S p S₂) : S₂.sol = S.sol := by
  cases hf with
  | star _ hb _ =>
      cases hw with
      | hit hl _  => exact absurd (lookup_det hl hb.toLookup) (by simp)
      | abs hl _  => exact absurd (lookup_det hl hb.toLookup) (by simp)
      | repark _  => rfl

/-- ⊢  …and the same fact read off the STATE rather than off a finalization step:
at a quiescent state wake-up cannot commit anything, so finalization is the only
progress left. That is what makes it the last step of a run. -/
theorem SolverState.Quiescent.wake_no_commit {B : Type} [DecidableEq B]
    {S S' : SolverState B} {p : Parked B} (hq : S.Quiescent) (hp : p ∈ S.parked)
    (hw : Wake S p S') : S'.sol = S.sol := by
  have hb := hq p hp
  cases hw with
  | hit hl _  => exact absurd (lookup_det hl hb.toLookup) (by simp)
  | abs hl _  => exact absurd (lookup_det hl hb.toLookup) (by simp)
  | repark _  => rfl


--------------------- THE SPENT PROMISE ---------------------------------------
-- The TENSION CASE, as a verdict rather than as an absence. A-sel-? returns δ so
-- that the position stays WRITABLE — and any use of the selection's value writes
-- to it: `(x.l) y` makes A-app emit `δ ≐ (τ_y → β)`, which succeeds, because δ is
-- an unsolved variable and that is exactly what "writable" meant. The promise has
-- then been SPENT on an arrow, and F-★'s own equation `δ ≐ ★` can no longer be
-- solved: ★ is rigid (`U-★`), so it unifies with nothing but itself and with a
-- variable. Finalization has no derivation, and the run cannot be completed.
--
-- That is a hard error by the same discipline as a clash — no rule applies — and
-- it is worth SAYING rather than leaving to be discovered as a stuck derivation.

/-- a type a promise has been spent on: neither a variable (still writable) nor ★
(already the answer F-★ wants). `δ ≐ ★` clashes on exactly these. -/
def Ty.Spent {B : Type} : Ty B → Prop
  | .var _ => False
  | .unk   => False
  | _      => True

/-- ⊢  **a spent promise cannot be finalized.** If the state has already written a
non-variable, non-★ type into a parked stump's result variable, F-★ has no
derivation at that stump — its equation is `⟦S⟧δ ≐ ★` and ★ is rigid. -/
theorem no_finalize_of_spent {B : Type} [DecidableEq B] {S : SolverState B}
    {p : Parked B} (h : (S.subst.ty p.stump.res).Spent) :
    ¬ ∃ S', Finalize S p S' := by
  rintro ⟨S', hf⟩
  cases hf with
  | star _ _ hs =>
      obtain ⟨fuel, s, Sup, hu, -⟩ := hs
      simp only [Ty.applySubst] at hu
      revert h hu
      cases S.subst.ty p.stump.res with
      | var _  => intro h; exact absurd h not_false
      | unk    => intro h; exact absurd h not_false
      | base b => intro _ hu; simp [unifyTyF] at hu
      | fn a b => intro _ hu; simp [unifyTyF] at hu
      | rcd ρ  => intro _ hu; simp [unifyTyF] at hu


--------------------- INSTANTIATION AT A-var ---------------------------------
-- `x: ∀(ᾱ: κ̄). Q ⇒ τ ∈ Γ   fresh β̄: κ̄   S ⊢ Q[β̄/ᾱ] ↝* S′`.
--
-- The instantiation is a RENAMING of the binders — that is what `fresh β̄` says
-- — so a stump's result variable δ is renamed too and stays a variable, which
-- is what keeps the selection's result position writable.

/-- `θ` renames exactly the binders `vs`, via `f`. -/
def IsRenaming {B : Type} (θ : TySubst B) (vs : List TyVar) (f : TyVar → TyVar) : Prop :=
  θ.FixedOutside vs ∧ ∀ α ∈ vs, θ.ty α = .var (f α) ∧ θ.row α = .var (f α)

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
  (∀ α ∈ vs, ∀ p ∈ S.parked, f α ≠ p.stump.res) ∧
  (∀ α ∈ vs, f α ∉ Γ.ftv)

/-- the parked images of a scheme's constraints under an instantiation. The
BLOCKERS are left free: `Wakes` determines each one, either by resolving the
constraint or by exhibiting a `LookupBlocked` witness — which is the `K-park`
rule (“compute the initial blocker”) that the paper leaves out. -/
def InstStumps {B : Type} (θ : TySubst B) (f : TyVar → TyVar)
    (Q : List (Stump B)) (ps : List (Parked B)) : Prop :=
  ps.map Parked.stump =
    Q.map (fun st => (⟨st.row.applySubst θ, st.label, f st.res⟩ : Stump B))

--------------------- ⟦S⟧ APPLIED TO A CONTEXT -------------------------------
-- The declarative side reads row-solutions out of a CONTEXT; the algorithm
-- keeps them in θ. `RowUnify/State.lean` bridges the two for a single lookup;
-- what the soundness statement additionally needs is the whole context read
-- under the final state — θ pushed through the type environment, and θ's row
-- component installed as the row environment discharge will consult.

-- `QScheme.applySubst` moved to Qualified.lean, where its capture-avoidance
-- side condition (`QScheme.Avoiding`) and what that condition buys
-- (`QCovers.forward_of_avoiding`, QSubst.lean) are stated and proved.

/-- `⟦S⟧Γ` — Γ under the state's substitution, with the state's row-solutions
installed as the row environment that `Lookup` and discharge consult. -/
def SolverState.applyCtx {B : Type} (S : SolverState B) (Γ : QCtx B) : QCtx B :=
  { tyEnv  := Γ.tyEnv.map (fun p => (p.1, p.2.applySubst S.subst)),
    rowEnv := S.sol.row }

/-- the variable a stump's result READS as at S: itself while unsolved, the
variable unification aliased it to otherwise. (A result solved to a non-variable
is a spent promise and has no reading; `LetResults` excludes it.) -/
def SolverState.resVar {B : Type} (S : SolverState B) (δ : TyVar) : TyVar :=
  match S.subst.ty δ with
  | .var β => β
  | _      => δ

/-- the scheme A-let builds: body AND constraints read under ⟦S₁⟧. Reading the
constraints raw would keep a stump's row pointing at the variable it was parked
on, not at the generalized tail that variable has since been solved to — and
then an instance's stump would chase the ORIGINAL tail, shared by all
instances. The RESULT is read too (`resVar`): a result aliased to β by
unification must be generalized as β, the name the body mentions
(`InferRuns.lean`, the `h = λy. g y` witness). -/
def letScheme {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) (Δq : List (Parked B))
    (τ₁ : Ty B) : QScheme B :=
  ⟨ᾱ, Δq.map (fun p => (⟨p.stump.row.applySubst S₁.subst, p.stump.label,
      S₁.resVar p.stump.res⟩ : Stump B)), τ₁.applySubst S₁.subst⟩

/-- A-let's premise on the generalized RESULTS: each still reads as a variable at
S₁, that variable is generalized, and distinct generalized stumps read as
distinct variables — one constraint per result (`QScheme.WF`, `Correctable`). -/
def LetResults {B : Type} (S₁ : SolverState B) (ᾱ : List TyVar) (Δq : List (Parked B)) :
    Prop :=
  (∀ p ∈ Δq, S₁.subst.ty p.stump.res = .var (S₁.resVar p.stump.res) ∧
      S₁.resVar p.stump.res ∈ ᾱ) ∧
  (∀ p ∈ Δq, ∀ q ∈ Δq, S₁.resVar p.stump.res = S₁.resVar q.stump.res → p.stump = q.stump)

/-- ⊢  an unsolved result reads as itself -/
theorem SolverState.subst_ty_of_not_dom {B : Type} {S : SolverState B} {δ : TyVar}
    (h : δ ∉ S.sol.dom) : S.subst.ty δ = .var δ :=
  tyLookup_not_mem _ (fun hm => h (List.mem_append_left _ hm))

theorem SolverState.resVar_of_not_dom {B : Type} {S : SolverState B} {δ : TyVar}
    (h : δ ∉ S.sol.dom) : S.resVar δ = δ := by
  unfold SolverState.resVar; rw [SolverState.subst_ty_of_not_dom h]

theorem SolverState.resVar_of_var {B : Type} {S : SolverState B} {δ β : TyVar}
    (h : S.subst.ty δ = .var β) : S.resVar δ = β := by
  unfold SolverState.resVar; rw [h]

--------------------- Γ; S ⊢ e ⇒ τ; S′ ---------------------------------------
-- One rule per term former, plus the DEGRADATION rules.
-- A clash has no rule at all: that is the hard error, and it is sound
-- because `unifyM_clash_no_unifier` proves a clash means no unifier exists.

mutual

inductive Infer {B C : Type} [DecidableEq B] (constTy : C → B) :
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
      Lookup S₂.ctx (.var r) l (.found τ') →
      Infer constTy Γ S (.sel e l) τ' S₂
  -- A-sel-⊥: definite absence. ★ and a W-flag — this is where T-sel-⊥ lives
  | selAbs {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r : TyVar} {S₁' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ (.rcd (.var r)) S₂ →
      Lookup S₂.ctx (.var r) l .absent →
      Infer constTy Γ S (.sel e l) .unk (S₂.flag l)
  -- A-sel-?: NOT ★. The stump-var δ keeps the position writable, so a later
  -- refinement can still fill it in — (x: x.l) must not freeze at {β} → ★
  | selUnk {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r α δ : TyVar} {S₁' S₂' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTySat S₁' τ (.rcd (.var r)) S₂ →
      LookupBlocked S₂.ctx ((Row.var r).applySubst S₂.subst) l α →
      (δ, S₂') = S₂.draw .ty →
      Infer constTy Γ S (.sel e l) (.var δ)
        (S₂'.park ⟨α, ⟨.var r, l, δ⟩⟩)
  -- A-rec
  | rcd {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
      InferRec constTy Γ S ξ ρ S' →
      Infer constTy Γ S (.rcd ξ) (.rcd ρ) S'
  -- A-let. The Δ-split is a least FIXPOINT on paper; as a relation it is enough
  -- to REQUIRE the split's defining equations, which is precisely what a
  -- relation buys over a function here.
  | letE {Γ : QCtx B} {S S₁ S₂ : SolverState B} {x : Var} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {Δq Δγ : List (Parked B)} {ᾱ : List TyVar} {κs : List Kind} :
      Infer constTy Γ S e₁ τ₁ S₁ →
      -- κ̄ = Γ(ᾱ), read off the DRAW rather than off Γ (ᾱ is disjoint from Γ,
      -- which is why the paper's form was never writable here). Forces every
      -- generalized binder to be one inference actually invented, at a known sort.
      S₁.kinds.Assigns ᾱ κs →
      -- Δ₁ = Δ_Γ ⊎ Δ_q — a PARTITION, up to order: Δ₁ is ordered by parking time,
      -- and a prefix split could not generalize a stump parked after a Γ-stump
      S₁.parked.Perm (Δq ++ Δγ) →
      -- Δ_q are exactly the stumps whose blocker lands in ᾱ, Δ_Γ the rest
      (∀ p ∈ Δq, p.blocker ∈ ᾱ) → (∀ p ∈ Δγ, p.blocker ∉ ᾱ) →
      -- ᾱ ∩ ftv(⟦S₁⟧Γ) = ∅ — generalize only what Γ does not mention. This was
      -- MISSING, and `runSound_false_unguarded_let` (LetSound.lean) is what it
      -- cost: `λy. let z = y in z` inferred `a → b`. Stated per variable of Γ
      -- and at BOTH sorts, since `ftv` does not record which one it saw.
      (∀ α ∈ ᾱ, ∀ β ∈ Γ.ftv, α ∉ (S₁.subst.ty β).ftv ∧ α ∉ (S₁.subst.row β).ftv) →
      -- Δ_q is e₁'s OWN: no stump parked before the let may be filed under the
      -- scheme, or nothing ever finalizes it (`runSound_false_let_captures`,
      -- LetSound.lean) …
      (∀ p ∈ Δq, ∀ q ∈ S.parked, p.stump ≠ q.stump) →
      -- … each generalized stump's answer, READ AT S₁, is generalized with it,
      -- one per stump (`QScheme.WF`) …
      LetResults S₁ ᾱ Δq →
      -- … what stays parked does not mention ᾱ, READ AT S₁, so it reads the
      -- same at every instance …
      (∀ α ∈ ᾱ, ∀ p ∈ Δγ, α ∉ (p.stump.row.applySubst S₁.subst).ftv ∧
         α ∉ (S₁.subst.ty p.stump.res).ftv) →
      -- … nothing already solved is generalized …
      (∀ α ∈ ᾱ, α ∉ S₁.sol.dom) →
      -- … and no generalized stump's row, read at S₁, mentions another's answer:
      -- then an instance's constraints can be discharged one at a time
      -- (`instEquivCorrects`, InferSoundA.lean), without a fixpoint
      (∀ p ∈ Δq, ∀ q ∈ Δq, S₁.resVar q.stump.res ∉ (p.stump.row.applySubst S₁.subst).ftv) →
      Infer constTy (Γ.bindScheme x (letScheme S₁ ᾱ Δq τ₁))
        { S₁ with parked := Δγ } e₂ τ₂ S₂ →
      Infer constTy Γ S (.letE x e₁ e₂) τ₂ S₂

inductive InferRec {B C : Type} [DecidableEq B] (constTy : C → B) :
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

end


/-- ⊢  A-var at a MONOTYPE: no binders, so nothing is drawn and the instance is
the body itself. Every example derivation's variable use is this one. -/
theorem Infer.var_mono {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {x : Var} {τ : Ty B}
    (hl : Γ.lookup x = some ⟨[], [], τ⟩) (hw : WakesSat S [] S') :
    Infer constTy Γ S (.var x) τ S' := by
  have h := Infer.var (constTy := constTy) (σ := ⟨[], [], τ⟩) (θ := TySubst.id B)
    (f := id) (ps := []) (Sup := S.supply) hl
    ⟨⟨fun _ _ => rfl, fun _ _ => rfl⟩, fun _ h => absurd h List.not_mem_nil⟩
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

private def idSubst : TySubst Unit := ⟨fun x => .var x, fun x => .var x⟩

-- The kinds the run records, newest first: δ at the type sort, the record's row
-- variable at the row sort, the λ-binder at the type sort. This is `fresh α: κ`
-- made concrete — the sorts that used to be readable only off the use sites.
private def selExKinds : KEnv :=
  [(natName 3, .ty), (natName 2, .row), (natName 1, .ty)]

theorem selEx_infers :
    Infer (B := Unit) (C := Unit) (fun _ => ()) ⟨[], []⟩
      ⟨Sol.nil, [], [], ⟨1⟩, []⟩ (selEx Unit)
      (.fn (.var (natName 1)) (.var (natName 3)))
      ⟨⟨[(natName 1, .rcd (.var (natName 2)))], []⟩,
       [⟨natName 2, ⟨.var (natName 2), "l", natName 3⟩⟩], [], ⟨4⟩, selExKinds⟩ := by
  refine Infer.lam (S₀ := ⟨Sol.nil, [], [], ⟨2⟩, [(natName 1, .ty)]⟩) rfl ?_
  refine Infer.selUnk (τ := .var (natName 1))
    (S₁ := ⟨Sol.nil, [], [], ⟨2⟩, [(natName 1, .ty)]⟩)
    (S₁' := ⟨Sol.nil, [], [], ⟨3⟩, [(natName 2, .row), (natName 1, .ty)]⟩)
    (S₂ := ⟨⟨[(natName 1, .rcd (.var (natName 2)))], []⟩, [], [], ⟨3⟩,
            [(natName 2, .row), (natName 1, .ty)]⟩)
    (r := natName 2) (α := natName 2) (δ := natName 3)
    ?_ rfl ?_ ?_ rfl
  · exact Infer.var_mono rfl
      ⟨_, .nil, .done (SolverState.Quiescent.nil rfl)⟩
  · exact ⟨_, ⟨5, ⟨[(natName 1, .rcd (.var (natName 2)))], []⟩, ⟨3⟩, rfl, rfl⟩,
      .done (SolverState.Quiescent.nil rfl)⟩
  · exact .varFree rfl


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
-- `plans/inference-gap-analysis.md` §B's last ✘ row: "`⇓`/`F-★` finalization
-- exists as a rule, but there is no `Infer(e) = finalize(...)` top-level
-- judgement to state soundness OF THE ALGORITHM about". This is it — run
-- inference from the empty state, then finalize what is still parked.
--
-- `Run` used to also demand `S′.parked = []`. `runSound` never needs it — the
-- conclusion is a plain typing whatever is left parked — so it is gone, and
-- `RunSound` is stronger for it.
--
-- The initial supply is ⟨1⟩, not ⟨0⟩: `natName 0` is the empty string, so
-- starting at 1 keeps every invented name non-empty.

/-- `⊢ e ⇒ τ; S′` — inference, then finalization, from nothing. -/
def Run {B C : Type} [DecidableEq B] (constTy : C → B) (e : Expr C) (τ : Ty B)
    (S' : SolverState B) : Prop :=
  ∃ S₁ : SolverState B,
    Infer constTy ⟨[], []⟩ ⟨Sol.nil, [], [], ⟨1⟩, []⟩ e τ S₁ ∧
    Finalizes S₁ S₁.parked S'

-- The state finalization starts at is QUIESCENT: the run begins with Δ = [], so
-- `Infer.quiescent_of_nil` (below) applies, and `Finalize.of_quiescent` then says
-- F-★'s `LookupBlocked` premise holds of every stump the run hands it. That is
-- the sense in which the premise constrains the RULE without constraining the
-- ALGORITHM.

/-- **Algorithm soundness** — inference soundness at the entry point, with
finalization run on whatever is still parked. Stated at the state's own substitution — NOT at an arbitrary satisfying σ,
because a later refinement can solve a blocker and make a lookup land, which is
precisely why finalization runs last — and in the EMPTY context: the program is
closed, and ⟦S′⟧ is already pushed into τ, so there is no row environment left to
consult. (It used to read `S′.applyCtx ⟨[], []⟩`, whose row environment is
⟦S′⟧'s row solutions; the refutations below never depended on that.)
PROVED: `runSound` (Finalization.lean). -/
def RunSound (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ (e : Expr C) (τ : Ty B) (S' : SolverState B), Run constTy e τ S' →
    QTyped constTy ⟨[], []⟩ e (τ.applySubst S'.subst)


--------------------- …AND IT RUNS -------------------------------------------
-- Non-vacuity for the entry judgement, on the two shapes that matter: one where
-- finalization FIRES, and one where saturation has already left it nothing to do.

private def selExS1 : SolverState Unit :=
  ⟨⟨[(natName 1, .rcd (.var (natName 2)))], []⟩,
   [⟨natName 2, ⟨.var (natName 2), "l", natName 3⟩⟩], [], ⟨4⟩, selExKinds⟩

private def selExStar : Sol Unit := ⟨[(natName 3, .unk)], []⟩

private def selExFinal : SolverState Unit :=
  ({ selExS1.extend selExStar ⟨4⟩ with
       parked := (selExS1.extend selExStar ⟨4⟩).parked.filter
         (·.stump.res != natName 3) }).flag "l"

/-- ⊢  **the entry judgement is not empty, and F-★ fires in it.** `λx. x.l` runs
end to end: A-sel-? parks the stump, nothing ever resolves it — the lookup is
still blocked on the record's row variable at the end — so finalization commits it
to ★ and records the label in W. -/
theorem selEx_runs :
    Run (B := Unit) (C := Unit) (fun _ => ()) (selEx Unit)
      (.fn (.var (natName 1)) (.var (natName 3))) selExFinal :=
  ⟨selExS1, selEx_infers,
   .cons (Finalize.star (by simp [selExS1]) (.varFree rfl)
     ⟨5, selExStar, ⟨4⟩, rfl, rfl⟩) .nil⟩

/-- ⊢  …and the answer is `{β} → ★` with `l` flagged — the L1-finalized type
(`selEx_absent`, minimal.lean), which `finalized_no_blur` says no substitution
can sharpen back. So this is the algorithm reaching the type the declarative
bookend describes, by the route F-★ exists to provide. -/
theorem selEx_runs_star :
    (Ty.fn (.var (natName 1)) (.var (natName 3))).applySubst selExFinal.subst
        = .fn (.rcd (.var (natName 2))) .unk ∧
      selExFinal.flags = ["l"] ∧ selExFinal.parked = [] :=
  ⟨rfl, rfl, rfl⟩


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
  exact (unifyM_supply_mono fuel).1 S.supply _ _ hu

theorem SolveRow.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ρ ρ' : Row B} (h : SolveRow S ρ ρ' S') : S.supply.next ≤ S'.supply.next := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  exact (unifyM_supply_mono fuel).2 S.supply _ _ hu

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

theorem WakesSat.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S.supply.next ≤ S'.supply.next
  | ⟨_, hw, hsat⟩ => Nat.le_trans hw.supply hsat.supply

theorem Finalize.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Finalize S p S' → S.supply.next ≤ S'.supply.next
  | .star _ _ hs => hs.supply

theorem Finalizes.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : Finalizes S ps S' → S.supply.next ≤ S'.supply.next
  | .nil         => Nat.le_refl _
  | .cons hf hfs => Nat.le_trans hf.supply hfs.supply

--------------------- THE KIND RECORD ONLY GROWS -----------------------------
-- The `κ̄` counterpart of the supply invariant above. Only `draw` writes to
-- `kinds`, and it CONSES, so every solver step leaves the record a suffix of
-- what it becomes: nothing already recorded is dropped or rewritten. This is
-- what makes `A-let`'s `KEnv.Assigns ᾱ κ̄` mean what it should — the kind a
-- binder is generalized at is the kind it was DRAWN at, not one a later step
-- could have overwritten.

theorem SolverState.draw_kinds {B : Type} (S : SolverState B) (κ : Kind) :
    (S.draw κ).2.kinds = ((S.draw κ).1, κ) :: S.kinds := rfl

-- Solving, waking and finalizing never invent a name, so they never touch κ̄.
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

theorem WakesSat.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S'.kinds = S.kinds
  | ⟨_, hw, hsat⟩ => (hsat.kinds).trans hw.kinds

theorem Finalize.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Finalize S p S' → S'.kinds = S.kinds
  | .star _ _ hs => hs.kinds

theorem Finalizes.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : Finalizes S ps S' → S'.kinds = S.kinds
  | .nil         => rfl
  | .cons hf hfs => (hfs.kinds).trans hf.kinds

-- from `(α, S₀) = S.draw κ`, recover the extended record
private theorem draw_kind_eq {B : Type} {S S₀ : SolverState B} {α : TyVar}
    {κ : Kind} (h : (α, S₀) = S.draw κ) : S₀.kinds = (α, κ) :: S.kinds := by
  have h1 : α = (S.draw κ).1 := congrArg Prod.fst h
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  rw [h2, h1]; rfl

mutual

/-- ⊢  inference never drops or rewrites a recorded kind. -/
theorem Infer.kinds_mono {B C : Type} [DecidableEq B] {constTy : C → B}
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
  | .rcd hb => InferRec.kinds_mono hb
  | .letE h₁ _ _ _ _ _ _ _ _ _ _ h₂ => by
      have i₁ := Infer.kinds_mono h₁
      have i₂ := Infer.kinds_mono h₂
      exact List.IsSuffix.trans i₁ i₂

theorem InferRec.kinds_mono {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.kinds <:+ S'.kinds
  | .empty      => List.suffix_refl _
  | .field h    => Infer.kinds_mono h
  | .cat h₁ h₂  => by
      have i₁ := InferRec.kinds_mono h₁
      have i₂ := InferRec.kinds_mono h₂
      exact List.IsSuffix.trans i₁ i₂

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
def KindsSound (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
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
theorem Infer.supply_mono {B C : Type} [DecidableEq B] {constTy : C → B}
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
  | .rcd hb => InferRec.supply_mono hb
  | .letE h₁ _ _ _ _ _ _ _ _ _ _ h₂ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      exact Nat.le_trans i₁ i₂

theorem InferRec.supply_mono {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.supply.next ≤ S'.supply.next
  | .empty => Nat.le_refl _
  | .field h => Infer.supply_mono h
  | .cat h₁ h₂ => Nat.le_trans (InferRec.supply_mono h₁) (InferRec.supply_mono h₂)

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

theorem Finalize.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Finalize S p S' → S.SatMono S'
  | .star _ _ hs => hs.satMono

theorem Finalizes.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : Finalizes S ps S' → S.SatMono S'
  | .nil         => SolverState.SatMono.refl _
  | .cons hf hfs => hf.satMono.trans hfs.satMono

theorem Saturate.satMono {B : Type} [DecidableEq B] {S S' : SolverState B} :
    Saturate S S' → S.SatMono S'
  | .done _           => SolverState.SatMono.refl _
  | .step _ _ hw hsat => hw.satMono.trans hsat.satMono

theorem SolveTySat.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} : SolveTySat S τ τ' S' → S.SatMono S'
  | ⟨_, hs, hsat⟩ => hs.satMono.trans hsat.satMono

theorem WakesSat.satMono {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ps : List (Parked B)} : WakesSat S ps S' → S.SatMono S'
  | ⟨_, hw, hsat⟩ => hw.satMono.trans hsat.satMono

private theorem draw_sol {B : Type} {S S₀ : SolverState B} {α : TyVar} {κ : Kind}
    (h : (α, S₀) = S.draw κ) : S₀.sol = S.sol := by
  have h2 : S₀ = (S.draw κ).2 := congrArg Prod.snd h
  rw [h2]; rfl

mutual

/-- ⊢  inference only refines the solution. -/
theorem Infer.sat_mono {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B} :
    Infer constTy Γ S e τ S' → S.SatMono S'
  | .con => SolverState.SatMono.refl _
  | .var _ _ _ _ _ _ _ hw => hw.satMono
  | .lam hd hb =>
      (SolverState.SatMono.of_sol_eq (draw_sol hd)).trans (Infer.sat_mono hb)
  | .app h₁ h₂ hd hs =>
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
  | .rcd hb => InferRec.sat_mono hb
  | .letE h₁ _ _ _ _ _ _ _ _ _ _ h₂ => by
      refine (Infer.sat_mono h₁).trans ?_
      intro σ hσ
      exact Infer.sat_mono h₂ σ hσ

theorem InferRec.sat_mono {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.SatMono S'
  | .empty      => SolverState.SatMono.refl _
  | .field h    => Infer.sat_mono h
  | .cat h₁ h₂  => (InferRec.sat_mono h₁).trans (InferRec.sat_mono h₂)

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
--   * `Quiescent.blocker_unsolved` — no parked blocker is solved, which is the
--     §B invariant and what makes a stump's annotation meaningful at all;
--   * F-★'s missing `LookupBlocked` premise is IMPLIED at any state a run
--     produces, so finalization's soundness lemma can have the fact without the
--     rule being changed — though the rule should still carry it, since nothing
--     stops `Finalize` from being applied at a state no run produced;
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
    (hb : LookupBlocked S.ctx (q.stump.row.applySubst S.subst) q.stump.label
            q.blocker) :
    (S'.park q).Quiescent := by
  intro p hp
  show LookupBlocked S'.sol.toCtx (p.stump.row.applySubst S'.sol.toSubst) _ _
  rw [hsol]
  rcases List.mem_cons.mp hp with rfl | hp'
  · exact hb
  · rw [hpar] at hp'; exact hq p hp'

mutual

/-- ⊢  **inference maintains the state invariant.** Every parked stump in the
state a run ends in is genuinely blocked on the blocker it records. -/
theorem Infer.quiescent {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B} :
    Infer constTy Γ S e τ S' → S.Quiescent → S'.Quiescent
  | .con, hq => hq
  -- the rules that WRITE end in a saturation, so they conclude it outright
  | .var _ _ _ _ _ _ _ hw, _ => hw.quiescent
  | .app _ _ _ hs, _ => hs.quiescent
  | .conc _ _ _ _ _ hs₂, _ => hs₂.quiescent
  | .sel _ _ hs _, _ => hs.quiescent
  | .selAbs _ _ hs _, _ => hs.quiescent.flag _
  | .selUnk _ hd hs hb hd₂, _ =>
      SolverState.Quiescent.park_draw (draw_sol hd₂) (draw_parked hd₂)
        hs.quiescent hb
  -- …and the rules that do not write carry it through
  | .lam hd hb, hq => Infer.quiescent hb (hq.draw hd)
  | .rcd hb, hq => InferRec.quiescent hb hq
  -- A-let: the body runs at Δ_Γ, a SUBLIST of the quiescent Δ₁
  | .letE h₁ _ hsplit _ _ _ _ _ _ _ _ h₂, hq => by
      refine Infer.quiescent h₂ ((Infer.quiescent h₁ hq).restrict ?_)
      intro p hp; exact hsplit.mem_iff.mpr <| List.mem_append_right _ hp

theorem InferRec.quiescent {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {ξ : RecBody (Expr C)} {ρ : Row B} :
    InferRec constTy Γ S ξ ρ S' → S.Quiescent → S'.Quiescent
  | .empty, hq     => hq
  | .field h, hq   => Infer.quiescent h hq
  | .cat h₁ h₂, hq => InferRec.quiescent h₂ (InferRec.quiescent h₁ hq)

end

/-- ⊢  and a run starts with an empty Δ, so this is unconditional. -/
theorem Infer.quiescent_of_nil {B C : Type} [DecidableEq B] {constTy : C → B}
    {Γ : QCtx B} {S S' : SolverState B} {e : Expr C} {τ : Ty B}
    (h : Infer constTy Γ S e τ S') (hnil : S.parked = []) : S'.Quiescent :=
  h.quiescent (SolverState.Quiescent.nil hnil)


end MinimalCalculus
