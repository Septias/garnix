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
-- have: unification's (the Rémy measure does not close), `A-let`'s Δ-split
-- least fixpoint, and the `↝*` wake-up closure. A relation owes none of them
-- and is exactly what "inference is sound w.r.t. the declarative system" has to
-- be stated over. Determinism and totality then become theorems ABOUT the
-- relation rather than things smuggled into its definition.
--
-- ## What is filled in here that the paper leaves as prose
--  * `LookupBlocked` — the paper writes `⟦S⟧ ⊢ ρ.l ↓ ? on α`, but `Lookup`
--    records only `.unknown` and not WHICH variable blocked. A-sel-? and
--    K-repark both need the blocker.
--  * the failure policy. "clash = hard error; stuck/occurs degrade to ★ + W" is
--    prose with no rules, so the algorithm was undefined on those inputs. Here
--    a clash simply has NO derivation (rejecting is soundness — clash is proved
--    to mean no unifier exists) and the degradations are explicit rules.

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
-- W collects definite-absence flags and ★-degradations and "never affects
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
-- against `sortedFtv`'s tags and left open, as `UnifyWF` and `InferSound` are.

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
closure keeps these rules independent of `UnifyWF`, which is still open; on a
well-formed state the two agree (`Sol.closes_of_wf`). -/
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
-- "clash is a hard error (it is PROVED to mean no unifier exists, so rejecting
-- is soundness, not choice). stuck and occurs are conservative and may NOT
-- reject; they degrade to ★ with a W-flag."
--
-- Rendered as a pair of relations. A CLASH satisfies neither, so no A-rule
-- fires and the program is rejected — that IS the hard error. The degradations
-- get their own A-rules below, which is what makes the judgement total on
-- inputs the paper left undefined.

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
    unifyTyF [] S.supply fuel (τ.applySubst S.subst) (τ'.applySubst S.subst)
      = .success s Sup ∧
    S' = S.extend s Sup

/-- …and the conservative verdicts, which may not reject. -/
def SolveTyDegrades {B : Type} [DecidableEq B]
    (S : SolverState B) (τ τ' : Ty B) : Prop :=
  ∃ fuel : Nat,
    unifyTyF [] S.supply fuel (τ.applySubst S.subst) (τ'.applySubst S.subst)
        = .stuck ∨
    unifyTyF [] S.supply fuel (τ.applySubst S.subst) (τ'.applySubst S.subst)
        = .occurs

/-- `S ⊢ ρ ≐ᵣ ρ′ ⇝ S′`, for the row equations `A-conc` and `A-rec` emit. -/
def SolveRow {B : Type} [DecidableEq B]
    (S : SolverState B) (ρ ρ' : Row B) (S' : SolverState B) : Prop :=
  ∃ (fuel : Nat) (s : Sol B) (Sup : Supply),
    unifySpineMF [] S.supply fuel (ρ.applySubst S.subst).toSpine
      (ρ'.applySubst S.subst).toSpine = .success s Sup ∧
    S' = S.extend s Sup

-- ⊢  a solved equation really is solved: the emitted solution unifies
-- The failure policy's justification, mechanized — this is why rejecting a
-- clash is soundness rather than choice, and why the degradations are the only
-- verdicts that need a rule of their own.
theorem SolveTy.unifies {B : Type} [DecidableEq B] {S S' : SolverState B}
    {τ τ' : Ty B} (h : SolveTy S τ τ' S') {θ : TySubst B}
    (hsat : Sol.Sat θ S'.sol) :
    ∃ s, Sol.Sat θ s ∧
      TyUnifies θ (τ.applySubst S.subst) (τ'.applySubst S.subst) := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  exact ⟨s, (Sol.Sat.comp_inv hsat).2,
    unifyM_success_sound fuel |>.1 [] S.supply _ _ hu (Sol.Sat.comp_inv hsat).2⟩

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

/-- `S ⊢ q ⇓ S′` — F-★, the algorithmic moment of T-sel-★. Runs at the end of
inference and at every generalization boundary that does not carry the stump.

**DEFECTIVE AS WRITTEN — see `finalize_star_no_discharge` (InferSound.lean).**
This rule has NO premise about the lookup, while every other rule that touches a
stump's result variable states what the lookup did first: `Wake.hit` carries its
`Lookup … (.found τ)`, `Wake.abs` its `Lookup … .absent`, `Wake.repark` its
`LookupBlocked`. Declaratively `Stump.Discharge` offers ★ only under `D-⊥` (the
lookup is `⊥`) or `D-?` (it is still `?`); there is no rule pinning δ to ★ when
the lookup LANDS. So F-★ can commit a stump to ★ in a configuration the
declarative system cannot read at all, and `finalize_star_no_discharge` exhibits
one: a stump on the literal row `(l: 𝓫)`, where the lookup lands at every
context and under every substitution, and F-★ fires anyway.

THE FIX is the premise its siblings have — `LookupBlocked S.ctx
(p.stump.row.applySubst S.subst) p.stump.label p.blocker`, which is also exactly
what A-sel-? establishes when it parks the stump. Left unchanged for now because
changing it moves `selEx_infers` and the A-sel-? soundness case with it. -/
inductive Finalize {B : Type} [DecidableEq B] :
    SolverState B → Parked B → SolverState B → Prop where
  | star {S S' : SolverState B} {p : Parked B} :
      SolveTy S (.var p.stump.res) .unk S' →
      Finalize S p
        ({ S' with parked := S'.parked.filter (·.stump.res != p.stump.res) }.flag
          p.stump.label)


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

--------------------- Γ; S ⊢ e ⇒ τ; S′ ---------------------------------------
-- One rule per term former, plus the DEGRADATION rules the failure policy calls
-- for and the paper never writes. A clash has no rule at all: that is the hard
-- error, and it is sound because `unifyM_clash_no_unifier` proves a clash means
-- no unifier exists.

mutual

inductive Infer {B C : Type} [DecidableEq B] (constTy : C → B) :
    QCtx B → SolverState B → Expr C → Ty B → SolverState B → Prop where
  -- A-cons
  | con {Γ : QCtx B} {S : SolverState B} {c : C} :
      Infer constTy Γ S (.con c) (.base (constTy c)) S
  -- A-var — IS I-inst: the instantiated constraints go to wake-up, which
  -- resolves what θ already decides and parks the rest
  | var {Γ : QCtx B} {S S' : SolverState B} {x : Var} {σ : QScheme B}
      {θ : TySubst B} {f : TyVar → TyVar} {ps : List (Parked B)} :
      Γ.lookup x = some σ →
      IsRenaming θ σ.vars f → FreshRenaming f σ.vars Γ S →
      InstStumps θ f σ.constraints ps →
      Wakes S ps S' →
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
      SolveTy S₂' τ₁ (.fn τ₂ (.var β)) S₃ →
      Infer constTy Γ S (.app e₁ e₂) (.var β) S₃
  -- A-app-degrade: the arrow equation is stuck or occurs, so the result blurs
  | appDeg {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {β : TyVar} {S₂' : SolverState B} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      (β, S₂') = S₂.draw .ty →
      SolveTyDegrades S₂' τ₁ (.fn τ₂ (.var β)) →
      Infer constTy Γ S (.app e₁ e₂) .unk (S₂'.flag "·app")
  -- A-conc
  | conc {Γ : QCtx B} {S S₁ S₂ S₃ S₄ : SolverState B} {e₁ e₂ : Expr C}
      {τ₁ τ₂ : Ty B} {r₁ r₂ : TyVar} {Sa Sb : SolverState B} :
      Infer constTy Γ S e₁ τ₁ S₁ → Infer constTy Γ S₁ e₂ τ₂ S₂ →
      (r₁, Sa) = S₂.draw .row → (r₂, Sb) = Sa.draw .row →
      SolveTy Sb τ₁ (.rcd (.var r₁)) S₃ →
      SolveTy S₃ τ₂ (.rcd (.var r₂)) S₄ →
      Infer constTy Γ S (.cat e₁ e₂) (.rcd (.cat (.var r₂) (.var r₁))) S₄
  -- A-sel: the lookup lands
  | sel {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ τ' : Ty B}
      {l : Label} {r : TyVar} {S₁' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTy S₁' τ (.rcd (.var r)) S₂ →
      Lookup S₂.ctx (.var r) l (.found τ') →
      Infer constTy Γ S (.sel e l) τ' S₂
  -- A-sel-⊥: definite absence. ★ and a W-flag — this is where T-sel-⊥ lives
  | selAbs {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r : TyVar} {S₁' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTy S₁' τ (.rcd (.var r)) S₂ →
      Lookup S₂.ctx (.var r) l .absent →
      Infer constTy Γ S (.sel e l) .unk (S₂.flag l)
  -- A-sel-?: NOT ★. The stump-var δ keeps the position writable, so a later
  -- refinement can still fill it in — (x: x.l) must not freeze at {β} → ★
  | selUnk {Γ : QCtx B} {S S₁ S₂ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r α δ : TyVar} {S₁' S₂' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTy S₁' τ (.rcd (.var r)) S₂ →
      LookupBlocked S₂.ctx (.var r) l α →
      (δ, S₂') = S₂.draw .ty →
      Infer constTy Γ S (.sel e l) (.var δ)
        (S₂'.park ⟨α, ⟨.var r, l, δ⟩⟩)
  -- A-sel-degrade: the record equation itself gave up
  | selDeg {Γ : QCtx B} {S S₁ : SolverState B} {e : Expr C} {τ : Ty B}
      {l : Label} {r : TyVar} {S₁' : SolverState B} :
      Infer constTy Γ S e τ S₁ →
      (r, S₁') = S₁.draw .row →
      SolveTyDegrades S₁' τ (.rcd (.var r)) →
      Infer constTy Γ S (.sel e l) .unk (S₁'.flag l)
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
      -- Δ₁ = Δ_Γ ⊎ Δ_q
      S₁.parked = Δq ++ Δγ →
      -- Δ_q are exactly the stumps whose blocker lands in ᾱ, Δ_Γ the rest
      (∀ p ∈ Δq, p.blocker ∈ ᾱ) → (∀ p ∈ Δγ, p.blocker ∉ ᾱ) →
      Infer constTy
        (Γ.bindScheme x ⟨ᾱ, Δq.map Parked.stump, τ₁.applySubst S₁.subst⟩)
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
-- Note which rule fires: A-sel-? , not A-sel-⊥ and not a degradation. `r` is
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
  · exact Infer.var (σ := ⟨[], [], .var (natName 1)⟩) (θ := idSubst) (f := id) (ps := []) rfl
      ⟨⟨fun _ _ => rfl, fun _ _ => rfl⟩, by simp⟩ (by simp [FreshRenaming]) rfl .nil
  · exact ⟨5, ⟨[(natName 1, .rcd (.var (natName 2)))], []⟩, ⟨3⟩, rfl, rfl⟩
  · exact .varFree rfl


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

--------------------- THE STATEMENT THIS MODULE EXISTS FOR --------------------
/-- **Inference soundness** — `Γ; S ⊢ e ⇒ τ; S′  ⟹  ⟦S′⟧Γ ⊢ e : ⟦S′⟧τ`.

"The algorithm never infers a type the declarative system rejects." This is the
statement `plans/inference-gap-analysis.md` §B calls unwriteable, and the reason
`⟦S⟧`-as-a-context was built at all. It is now WRITEABLE. It is not proved, and
it is kept as a named `def` for the same reason `UnifyWF` and `TerminalNoMgu`
are: so the thing being aimed at can be named.

THE PROOF LIVES IN `InferSound.lean`, one lemma per A-rule. Eight of the
thirteen rules are proved there, plus A-sel-? modulo `StumpHonest`; the census
at the bottom of that file says what the rest are waiting on. The case lemmas
are stated at an arbitrary σ with `Sol.Sat σ S′.sol`, NOT at `⟦S′⟧` — that is
what lets a premise solved at an intermediate state be replayed under the σ the
conclusion is stated at (`Infer.sat_mono`). Taking σ := S′.subst is the last
step, not the first.

WHAT IT WILL NEED, and none of it is available yet:
  * `UnifyWF` — without it `⟦S′⟧` is `Sol.toSubst`, one unfolding step, rather
    than the closure `algorithmic.typ` specifies. The two agree exactly on a
    well-formed state (`Sol.closes_of_wf`), so this statement is provisional
    until that lands.
  * the parked stumps must DISCHARGE. `A-sel-?` returns a stump-variable δ and
    parks `⟨α ▷ ρ.l ↓ δ⟩`; declaratively `QScheme.Inst` demands
    `Stump.Discharge`, so soundness holds only for states whose Δ is either
    empty or finalized (`Finalize`). The honest form of the theorem probably
    carries `S′.parked = []` as a hypothesis, with `F-★` supplying it.
  * capture-avoidance for `QScheme.applySubst` (see its note).
  * an L2 type-substitution lemma. L1 has `typed_applySubst_aux`; `QTyped` has
    only TERM substitution (`qsubst_preserves_typing`), and transporting a
    derivation along θ is exactly what this proof does. §B lists it. -/
def InferSound (B C : Type) [DecidableEq B] (constTy : C → B) : Prop :=
  ∀ (Γ : QCtx B) (S S' : SolverState B) (e : Expr C) (τ : Ty B),
    Infer constTy Γ S e τ S' → S'.parked = [] →
    QTyped constTy (S'.applyCtx Γ) e (τ.applySubst S'.subst)


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
  exact (unifyM_supply_mono fuel).1 [] S.supply _ _ hu

theorem SolveRow.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {ρ ρ' : Row B} (h : SolveRow S ρ ρ' S') : S.supply.next ≤ S'.supply.next := by
  obtain ⟨fuel, s, Sup, hu, rfl⟩ := h
  exact (unifyM_supply_mono fuel).2 [] S.supply _ _ hu

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

theorem Finalize.supply {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Finalize S p S' → S.supply.next ≤ S'.supply.next
  | .star hs => hs.supply

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

theorem Finalize.kinds {B : Type} [DecidableEq B] {S S' : SolverState B}
    {p : Parked B} : Finalize S p S' → S'.kinds = S.kinds
  | .star hs => hs.kinds

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
  | .var _ _ _ _ hw => hw.kinds ▸ List.suffix_refl _
  | .lam hd hb => by
      refine List.IsSuffix.trans ?_ (Infer.kinds_mono hb)
      rw [draw_kind_eq hd]; exact List.suffix_cons _ _
  | .app h₁ h₂ hd hs => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      refine List.IsSuffix.trans (Infer.kinds_mono h₂) ?_
      rw [hs.kinds, draw_kind_eq hd]; exact List.suffix_cons _ _
  | .appDeg h₁ h₂ hd _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      refine List.IsSuffix.trans (Infer.kinds_mono h₂) ?_
      show _ <:+ (SolverState.flag _ _).kinds
      simp only [SolverState.flag]
      rw [draw_kind_eq hd]; exact List.suffix_cons _ _
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
  | .selDeg h₁ hd _ => by
      refine List.IsSuffix.trans (Infer.kinds_mono h₁) ?_
      show _ <:+ (SolverState.flag _ _).kinds
      simp only [SolverState.flag]
      rw [draw_kind_eq hd]; exact List.suffix_cons _ _
  | .rcd hb => InferRec.kinds_mono hb
  | .letE h₁ _ _ _ _ h₂ => by
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

Named, not proved — like `UnifyWF` and `InferSound`, so the target has a name.
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
  | .var _ _ _ _ hw => hw.supply
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
  | .appDeg h₁ h₂ hd _ => by
      have i₁ := Infer.supply_mono h₁
      have i₂ := Infer.supply_mono h₂
      have hd' := draw_eq hd
      show _ ≤ (SolverState.flag _ _).supply.next
      simp only [SolverState.flag]
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
  | .selDeg h₁ hd _ => by
      have i₁ := Infer.supply_mono h₁
      have hd' := draw_eq hd
      show _ ≤ (SolverState.flag _ _).supply.next
      simp only [SolverState.flag]
      omega
  | .rcd hb => InferRec.supply_mono hb
  | .letE h₁ _ _ _ _ h₂ => by
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
  | .var _ _ _ _ hw => hw.satMono
  | .lam hd hb =>
      (SolverState.SatMono.of_sol_eq (draw_sol hd)).trans (Infer.sat_mono hb)
  | .app h₁ h₂ hd hs =>
      ((Infer.sat_mono h₁).trans (Infer.sat_mono h₂)).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans hs.satMono)
  | .appDeg h₁ h₂ hd _ =>
      ((Infer.sat_mono h₁).trans (Infer.sat_mono h₂)).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (SolverState.SatMono.of_sol_eq rfl))
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
  | .selDeg h₁ hd _ =>
      (Infer.sat_mono h₁).trans
        ((SolverState.SatMono.of_sol_eq (draw_sol hd)).trans
          (SolverState.SatMono.of_sol_eq rfl))
  | .rcd hb => InferRec.sat_mono hb
  | .letE h₁ _ _ _ _ h₂ => by
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


end MinimalCalculus
