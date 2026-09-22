-- FUZZ: an executable search for DIVERGENCE in the ≐ᵣ / ≐ driver.
--
-- Not part of the library (`lake build` does not touch it). Run with
--
--     lake build fuzz && lake exe fuzz
--
-- ## Why
-- Phase D of proof-plan.md: there is no closed-form fuel bound, the naive Rémy
-- measure does not close, and the lexicographic measure was retracted because
-- solve-and-apply grows the spine AND the variable count. Before designing a
-- third measure, settle the prior question — is the driver terminating at all?
-- A divergent input is as valuable a result as a bound, and cheaper to find.
--
-- ## What it does, per pair of spines drawn from a small universe
--  1. DIVERGENCE HUNT. `minFuel` = the least budget at which the pair reaches a
--     verdict, by binary search up to `cap`. A pair still `outOfFuel` at `cap`
--     — with `cap` far above its size — is a divergence CANDIDATE and gets
--     printed. Zero survivors is evidence for termination, not a proof.
--  2. FUEL-MONOTONICITY TRIPWIRE. `unifyM_fuel_mono` says a verdict once
--     REACHED never changes as the budget grows. So the full result (verdict
--     AND solution) at `minFuel` must equal the one at `cap`. Any mismatch is a
--     counterexample to a theorem in the build — print it loudly.
--  3. FUEL PROFILE. The max `minFuel` observed per problem size |s₁|+|s₂|.
--     This is the empirical shape of the bound a measure would have to beat: if
--     it is flat or linear the Rémy-style argument is worth the effort, if it
--     climbs fast the bound is not what one would guess.
--
-- The binary search in (1) and (3) is justified by `unifyM_fuel_mono`:
-- "reaches a verdict" is monotone in the budget. (2) is what checks that.

import RowUnify

namespace MinimalCalculus
namespace Fuzz

abbrev Spine := List (Atom Unit)

---------------------------------- PRINTING -----------------------------------

mutual
def tyStr : Ty Unit → String
  | .var α    => α
  | .base _   => "𝓫"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyStr τ₁ ++ "→" ++ tyStr τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowStr ρ ++ "}"

def rowStr : Row Unit → String
  | .empty     => "ε"
  | .var α     => α
  | .sing l τ  => l ++ ":" ++ tyStr τ
  | .cat ρ₁ ρ₂ => rowStr ρ₁ ++ " | " ++ rowStr ρ₂
end

def atomStr : Atom Unit → String
  | .field l τ => l ++ ":" ++ tyStr τ
  | .var α     => α

def spineStr (s : Spine) : String :=
  "(" ++ String.intercalate " | " (s.map atomStr) ++ ")"

def solStr (s : Sol Unit) : String :=
  "[" ++ String.intercalate ", " (s.ty.map  fun p => p.1 ++ "≔" ++ tyStr p.2) ++
  " ; " ++ String.intercalate ", " (s.row.map fun p => p.1 ++ "≔" ++ rowStr p.2) ++ "]"

-- Structural equality on the syntax, so the semantic half of `Sol.WF` can be
-- decided. (`Ty`/`Row` derive no `DecidableEq`: the algorithm only ever needs
-- `DecidableEq B` at the leaves.)
mutual
def tyEqB : Ty Unit → Ty Unit → Bool
  | .var a,    .var b     => a == b
  | .base _,   .base _    => true
  | .unk,      .unk       => true
  | .fn a b,   .fn c d    => tyEqB a c && tyEqB b d
  | .rcd r,    .rcd r'    => rowEqB r r'
  | _,         _          => false

def rowEqB : Row Unit → Row Unit → Bool
  | .empty,    .empty     => true
  | .var a,    .var b     => a == b
  | .sing l τ, .sing l' τ' => l == l' && tyEqB τ τ'
  | .cat a b,  .cat c d   => rowEqB a c && rowEqB b d
  | _,         _          => false
end

/-- `Sol.WF` (RowUnify/State.lean) as a Bool — the well-formedness ⟦S⟧ needs to
    be a context at all. `Sol.rowWF_toCtx` is conditioned on the acyclic half and
    the θ ↦ rowEnv bridge on the applied half, and `UnifyWF` — the claim that the
    driver RETURNS such a solution — is NOT proved. This is its tripwire.

    Note the applied half is the SEMANTIC one. The syntactic `Sol.NoCapture`
    ("no bound variable occurs in any binding") is refuted here in one move by
    `a ≐ᵣ (l:a)` ⇝ `a ≔ (l:a | ε)`, where the bound `a` is a ROW variable and the
    payload `a` a TYPE variable — different variables that `ftv`, spanning one
    untagged namespace, cannot tell apart. -/
def solAcyclicB (s : Sol Unit) : Bool :=
  let keys := s.row.map Prod.fst
  s.row.all (fun p => (sVarSeq p.2.toSpine).all (fun β => !keys.contains β))

def solAppliedB (s : Sol Unit) : Bool :=
  s.ty.all  (fun p => tyEqB (p.2.applySubst s.toSubst) p.2) &&
  s.row.all (fun p => rowEqB (p.2.applySubst s.toSubst) p.2)

/-- `Sol.Ranked` as a Bool: the sorted dependency graph on the solution's
    bindings is a DAG. A rank exists exactly when it is — take the longest-path
    depth, which is below the number of bindings — so this is the decidable face
    of the ∃-rank in `Sol.Ranked`, and with `Sol.closes_closure` it is what says
    ⟦S⟧ EXISTS for this solution. It replaces the `Applied` half of the old
    tripwire, which was false by design against a triangular solution. -/
def solDeps (s : Sol Unit) : Bool × TyVar → List (Bool × TyVar)
  | (false, α) =>
      ((s.ty.filter  (fun p => p.1 == α)).flatMap (fun p => Ty.sortedFtv  p.2)).filter
        (fun y => s.domS.contains y)
  | (true,  α) =>
      ((s.row.filter (fun p => p.1 == α)).flatMap (fun p => Row.sortedFtv p.2)).filter
        (fun y => s.domS.contains y)

/-- Peel nodes with no surviving dependency; a DAG empties, a cycle stalls. -/
def peelDeps (s : Sol Unit) : Nat → List (Bool × TyVar) → Bool
  | 0,     rem => rem.isEmpty
  | n + 1, rem =>
      let next := rem.filter (fun x => (solDeps s x).any (fun y => rem.contains y))
      if next.length == rem.length then rem.isEmpty else peelDeps s n next

def solRankedB (s : Sol Unit) : Bool := peelDeps s (s.domS.length + 1) s.domS

def solWFB (s : Sol Unit) : Bool := solAcyclicB s && solRankedB s

------------------------- WHICH RANK MEASURE ACTUALLY WORKS -------------------
-- `solRankedB` DECIDES `Sol.Ranked` by peeling, and the sweep says it never
-- fails. That is not a proof: `Sol.Ranked` is an ∃-rank, and to prove it the
-- driver needs a rank it can EXHIBIT and maintain. So the question is not "is the
-- solution rankable" (measured: yes) but "which candidate rank works".
--
-- The candidates, and why these:
--   * |name| — the supply hands out `natName k`, so a name's LENGTH is its draw
--     index, and `unifyM_supply_mono` (already proved) says the draw index only
--     advances. If a rank is monotone in the draw order, that theorem is most of
--     the proof, and nothing new has to be maintained.
--   * position in `domS` — `Sol.comp s₂ s₁` appends: `s₁`'s bindings come first,
--     with their values pushed THROUGH `s₂`. So a binding's value should mention
--     variables bound LATER, i.e. the solution should be triangular in list
--     order. `Sol.Ranked`'s own bound (`rank x < domS.length`) suggests the index
--     was the intended witness.
-- Both are tested in both directions, because the driver both invents fresh names
-- (binding an OLD variable to a value mentioning NEW ones) and solves fresh ones
-- against old payloads.
--
-- NOT MEASURED HERE, and why: the `DepGraph` the driver threads for its occurs
-- guards is an INPUT to `unifyTyF`/`unifySpineMF` and is never returned, so the
-- accumulated Θ is not observable without cloning the driver. If one of the
-- candidates below is clean, Θ is not needed as a measure at all — which is the
-- cheaper question, so it goes first.

/-- Every dependency edge the solution imposes: from a binding's KEY to a
variable in its value that the solution also binds. `Sol.Ranked` is exactly the
existence of a rank that strictly decreases along all of these. -/
def solEdges (s : Sol Unit) : List ((Bool × TyVar) × (Bool × TyVar)) :=
  (s.ty.flatMap  (fun p => ((Ty.sortedFtv  p.2).filter (fun y => s.domS.contains y)).map
                             (fun y => ((false, p.1), y)))) ++
  (s.row.flatMap (fun p => ((Row.sortedFtv p.2).filter (fun y => s.domS.contains y)).map
                             (fun y => ((true,  p.1), y))))

def domIdx (s : Sol Unit) (x : Bool × TyVar) : Nat := s.domS.findIdx (· == x)

/-- keep the FIRST binding per key — which is the only one `rowLookup`/`tyLookup`
ever reads, every later one being dead. `Sol.Ranked` quantifies over ALL of them,
so it constrains bindings ⟦S⟧ cannot see; this is the same solution with the dead
weight dropped, and the measurement below is what it is for. -/
def dedupKeys {α : Type} (l : List (TyVar × α)) : List (TyVar × α) :=
  (l.foldl (fun acc p => if acc.any (fun q => q.1 == p.1) then acc else p :: acc) []).reverse

def solDedup (s : Sol Unit) : Sol Unit := ⟨dedupKeys s.ty, dedupKeys s.row⟩

/-- a key bound TWICE with values that are not syntactically the same row/type.
`Sol.toSubst` reads only the FIRST binding, while `Sol.Sat` quantifies over
every pair — so where these disagree, ⟦S⟧ and `Sat` are talking about different
substitutions. proof-state flags this as a SUB-OBLIGATION with no invariant
behind it; this counts it. -/
def solDupDiff (s : Sol Unit) : Bool :=
  s.ty.any  (fun p => s.ty.any  (fun q => p.1 == q.1 && !tyEqB  p.2 q.2)) ||
  s.row.any (fun p => s.row.any (fun q => p.1 == q.1 && !rowEqB p.2 q.2))

def solHasDup (s : Sol Unit) : Bool :=
  (dedupKeys s.ty).length != s.ty.length || (dedupKeys s.row).length != s.row.length

def nodeStr (x : Bool × TyVar) : String := (if x.1 then "ᵣ" else "ₜ") ++ x.2

def edgeStr (e : (Bool × TyVar) × (Bool × TyVar)) : String :=
  nodeStr e.1 ++ "→" ++ nodeStr e.2

structure RankVerdict where
  edges  : Nat
  lenDec : Bool
  lenInc : Bool
  idxDec : Bool
  /-- index-decreasing on the DEDUPED solution -/
  idxDecD : Bool
  idxInc : Bool
  hasDup : Bool
  ranked : Bool

def rankVerdict (s : Sol Unit) : RankVerdict :=
  let es := solEdges s
  { edges  := es.length
    lenDec := es.all (fun e => e.2.2.length < e.1.2.length)
    lenInc := es.all (fun e => e.1.2.length < e.2.2.length)
    idxDec := es.all (fun e => domIdx s e.2 < domIdx s e.1)
    idxDecD := let d := solDedup s
               (solEdges d).all (fun e => domIdx d e.2 < domIdx d e.1)
    hasDup := solHasDup s
    idxInc := es.all (fun e => domIdx s e.1 < domIdx s e.2)
    ranked := solRankedB s }

/-- the edges a candidate fails on, for the witness line. -/
def badEdges (s : Sol Unit) (p : ((Bool × TyVar) × (Bool × TyVar)) → Bool) : String :=
  String.intercalate ", " (((solEdges s).filter (fun e => !p e)).map edgeStr)

/-- The FULL result, solution included — the tripwire compares these. -/
def resStr : UResM Unit → String
  | .success s S => "success " ++ solStr s ++ " @supply " ++ toString S.next
  | .clash       => "clash"
  | .occurs      => "occurs"
  | .stuck       => "stuck"
  | .outOfFuel   => "outOfFuel"

def verdictStr : UResM Unit → String
  | .success _ _ => "success"
  | .clash       => "clash"
  | .occurs      => "occurs"
  | .stuck       => "stuck"
  | .outOfFuel   => "outOfFuel"

--------------------- THE ACCUMULATED DEPENDENCY GRAPH ------------------------
-- The two cheap candidates are REFUTED by the sweep above (|name| in `deep`,
-- domS-index in `deep`, and after dedup too), so the question the comment above
-- deferred is now live: does `Θ` carry the topological order?
--
-- `Θ` is an INPUT to `unifyTyF`/`unifySpineMF` and is never returned, so the
-- sweep could not see it. The clone below is the driver with ONE change: it also
-- returns every expansion edge created anywhere in the EXECUTED recursion tree.
-- The threaded `Θ` that the guards read is untouched, so the verdicts must agree
-- with the real driver — `nDisagree` checks that on every pair, and a nonzero
-- count means this clone has drifted and its numbers are worthless.
--
-- Why the union over the tree rather than the threaded `Θ`: at a `.seq` the
-- second stage is called with the SAME `Θ` as the first (Defs.lean:785-800), so
-- the threaded graph is the root-to-node PATH, not the timeline. The solution,
-- though, is composed from both branches. Any rank for `Sol.Ranked` has to order
-- all of it, so the union is the only candidate carrier.

/-- what the clone returns: the verdict, the expansion graph accumulated over the
whole executed tree, and the LEDGER — every key the driver bound, in the order it
bound them. The ledger is the thing `domS` is not: `Sol.domS` is `ty ++ row`, so
it scrambles two sorts that the driver interleaves, and that alone could be why
the index candidates failed. -/
abbrev TRes := UResM Unit × DepGraph × List (Bool × TyVar) × Nat

def solOf : UResM Unit → Sol Unit
  | .success s _ => s
  | _            => Sol.nil

/-- every name a solution MENTIONS, tagged: its keys and its values' free
variables. `SolMentions` (Defs.lean) is the same thing as a predicate. -/
def solMentionsS (s : Sol Unit) : List (Bool × TyVar) :=
  s.domS ++ s.ty.flatMap (fun p => Ty.sortedFtv p.2)
         ++ s.row.flatMap (fun p => Row.sortedFtv p.2)

/-- THE MISSING LEMMA, as a count. `Sol.Ranked` is preserved by `Sol.comp`
if the LATER stage mentions nothing of the EARLIER stage's domain — then the
two ranks stack, later-stage below earlier-stage, and no edge crosses back.
proof-state names exactly this as the step `Supply`/`Avoids`/`SolBelow`
constrains but does not pin down. Every `.seq` and every expansion is a `comp`,
so this counts the violations at all of them. -/
def collides (later earlier : Sol Unit) : Nat :=
  ((solMentionsS later).filter (fun x => earlier.domS.contains x)).length

def ledgerOf : UResM Unit → List (Bool × TyVar)
  | .success s _ => s.domS
  | _            => []

def tSeq (p : TRes) (k : TySubst Unit → Supply → TRes) : TRes :=
  match p with
  | (.success s S, g, L, c) =>
      -- `s'.comp s` makes `s` the EARLIER stage, so its keys are older
      match k s.toSubst S with
      | (.success s' S', g', L', c') =>
          (.success (s'.comp s) S', g' ++ g, L ++ L', c + c' + collides s' s)
      | (r, g', L', c')          => (r, g' ++ g, L ++ L', c + c')
  | (r, g, L, c) => (r, g, L, c)

mutual

def traceTyF (Θ : DepGraph) (S : Supply) (fuel : Nat) :
    Ty Unit → Ty Unit → TRes
  | .var α, τ₂ => let r := bindTy S α τ₂; (r, [], ledgerOf r, 0)
  | τ₁, .var α => let r := bindTy S α τ₁; (r, [], ledgerOf r, 0)
  | .unk, .unk => (.success .nil S, [], [], 0)
  | .base b, .base b' => (if b = b' then .success .nil S else .clash, [], [], 0)
  | .fn a₁ b₁, .fn a₂ b₂ =>
      match fuel with
      | 0 => (.outOfFuel, [], [], 0)
      | f+1 =>
          tSeq (traceTyF Θ S f a₁ a₂) fun θ S' =>
            traceTyF Θ S' f (b₁.applySubst θ) (b₂.applySubst θ)
  | .rcd ρ₁, .rcd ρ₂ =>
      match fuel with
      | 0 => (.outOfFuel, [], [], 0)
      | f+1 => traceSpineMF Θ S f ρ₁.toSpine ρ₂.toSpine
  | _, _ => (.clash, [], [], 0)

def traceSpineMF :
    DepGraph → Supply → Nat → List (Atom Unit) → List (Atom Unit) → TRes
  | _, S, _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => (.success (Sol.ofRow σ) S, [], (Sol.ofRow σ : Sol Unit).domS, 0)
      | none   => (.clash, [], [], 0)
  | _, S, _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => (.success (Sol.ofRow σ) S, [], (Sol.ofRow σ : Sol Unit).domS, 0)
      | none   => (.clash, [], [], 0)
  | _, _, 0, _, _ => (.outOfFuel, [], [], 0)
  | Θ, S, fuel+1, s₁, s₂ =>
      match stripL s₁ s₂ with
      | some (t₁, t₂) => traceSpineMF Θ S fuel t₁ t₂
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => traceSpineMF Θ S fuel t₁ t₂
      | none =>
      match solveVarM Θ S s₁ s₂ with
      | some r => (r, [], ledgerOf r, 0)
      | none =>
      match solveVarM Θ S s₂ s₁ with
      | some r => (r, [], ledgerOf r, 0)
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          tSeq (traceTyF Θ S fuel τ τ') fun θ S' =>
            traceSpineMF Θ S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          tSeq (traceTyF Θ S fuel τ τ') fun θ S' =>
            traceSpineMF Θ S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          tSeq (traceTyF Θ S fuel τ τ') fun θ S' =>
            traceSpineMF Θ S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          tSeq (traceTyF Θ S fuel τ τ') fun θ S' =>
            traceSpineMF Θ S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          tSeq (traceTyF Θ S fuel τ τ') fun θ S' =>
            traceSpineMF Θ S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          tSeq (traceTyF Θ S fuel τ τ') fun θ S' =>
            traceSpineMF Θ S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match expandL Θ S s₁ s₂ with
      | some (β, l, τ, t₁, t₂) =>
          let p := traceSpineMF (expandDeps Θ S β τ) S.fresh.2.fresh.2 fuel t₁ t₂
          (expandResM S β l τ p.1, expandDeps p.2.1 S β τ,
           (false, S.fresh.1) :: (true, β) :: p.2.2.1,
           p.2.2.2 + collides (solOf p.1) ⟨[(S.fresh.1, τ)], [(β, (.cat (.sing l (.var S.fresh.1)) (.var S.fresh.2.fresh.1)))]⟩)
      | none =>
      match expandL Θ S s₂ s₁ with
      | some (β, l, τ, t₁, t₂) =>
          let p := traceSpineMF (expandDeps Θ S β τ) S.fresh.2.fresh.2 fuel t₁ t₂
          (expandResM S β l τ p.1, expandDeps p.2.1 S β τ,
           (false, S.fresh.1) :: (true, β) :: p.2.2.1,
           p.2.2.2 + collides (solOf p.1) ⟨[(S.fresh.1, τ)], [(β, (.cat (.sing l (.var S.fresh.1)) (.var S.fresh.2.fresh.1)))]⟩)
      | none =>
      if projClash s₁ s₂ then (.clash, [], [], 0) else
      match expandR Θ S s₁ s₂ with
      | some (β, l, τ, t₁, t₂) =>
          let p := traceSpineMF (expandDeps Θ S β τ) S.fresh.2.fresh.2 fuel t₁ t₂
          (expandResRM S β l τ p.1, expandDeps p.2.1 S β τ,
           (false, S.fresh.1) :: (true, β) :: p.2.2.1,
           p.2.2.2 + collides (solOf p.1) ⟨[(S.fresh.1, τ)], [(β, (.cat (.var S.fresh.2.fresh.1) (.sing l (.var S.fresh.1))))]⟩)
      | none =>
      match expandR Θ S s₂ s₁ with
      | some (β, l, τ, t₁, t₂) =>
          let p := traceSpineMF (expandDeps Θ S β τ) S.fresh.2.fresh.2 fuel t₁ t₂
          (expandResRM S β l τ p.1, expandDeps p.2.1 S β τ,
           (false, S.fresh.1) :: (true, β) :: p.2.2.1,
           p.2.2.2 + collides (solOf p.1) ⟨[(S.fresh.1, τ)], [(β, (.cat (.var S.fresh.2.fresh.1) (.sing l (.var S.fresh.1))))]⟩)
      | none => (.stuck, [], [], 0)

end

def traceSpineM (fuel : Nat) (s₁ s₂ : Spine) : TRes :=
  traceSpineMF [] (localSupply s₁ s₂) fuel s₁ s₂

------------------------------ GRAPH UTILITIES --------------------------------

def gTargets (G : DepGraph) (x : TyVar) : List TyVar :=
  (G.filter (fun p => p.1 == x)).flatMap Prod.snd

def gNodes (G : DepGraph) : List TyVar :=
  (G.flatMap (fun p => p.1 :: p.2)).foldl
    (fun acc x => if acc.contains x then acc else x :: acc) []

/-- Kahn, again: peel nodes with no surviving outgoing edge. -/
def gPeel (G : DepGraph) : Nat → List TyVar → Bool
  | 0,     rem => rem.isEmpty
  | n + 1, rem =>
      let next := rem.filter (fun x => (gTargets G x).any (fun y => rem.contains y))
      if next.length == rem.length then rem.isEmpty else gPeel G n next

def gAcyclic (G : DepGraph) : Bool := gPeel G ((gNodes G).length + 1) (gNodes G)

/-- Longest path out of `x`. Meaningful only on an acyclic `G`; on a cyclic one
the fuel truncates it, which is why `nGCyc` is reported first. -/
def gDepth (G : DepGraph) : Nat → TyVar → Nat
  | 0,     _ => 0
  | n + 1, x => ((gTargets G x).map (fun y => 1 + gDepth G n y)).foldl max 0

def gStr (G : DepGraph) : String :=
  String.intercalate ", "
    (G.map (fun p => p.1 ++ "→[" ++ String.intercalate " " p.2 ++ "]"))

/-- the solution's own edges with the sort tag DROPPED — `DepGraph` is untagged
(`List (TyVar × List TyVar)`), so a rank read off `Θ` ranks NAMES. If this graph
has a cycle, no untagged rank exists at all and `Θ` cannot be the carrier
whatever else it does. The cross-sort alias (`a` bound at both sorts) is exactly
what can make it cyclic. -/
def solEdgesU (s : Sol Unit) : DepGraph :=
  (solEdges s).map (fun e => (e.1.2, [e.2.2]))

structure DepVerdict where
  /-- clone disagrees with the real driver — invalidates the rest -/
  agree    : Bool
  gEdges   : Nat
  gAcyc    : Bool
  /-- solution edges whose untagged pair is NOT in Θ's reachability -/
  uncov    : Nat
  /-- solution edges along which Θ-depth does not strictly decrease -/
  depthBad : Nat
  /-- the solution's own edge graph, untagged, is a DAG -/
  untagged : Bool
  /-- keys in `domS` the ledger never recorded — a hole in the clone, not a
  finding about the algorithm -/
  ledHole  : Nat
  /-- CREATION ORDER as the rank. `ledInc`: every binding mentions only
  LATER-bound variables, which is what solve-and-apply plus `comp`'s push
  predicts. `ledDec` is the same test the other way round. -/
  ledInc   : Bool
  ledDec   : Bool
  /-- `Sol.comp`'s preservation step: how often a LATER stage mentions an
  EARLIER stage's domain. Zero is what makes the two ranks stack. -/
  coll     : Nat

def ledgerStr (L : List (Bool × TyVar)) : String :=
  String.intercalate " " (L.map nodeStr)

def depVerdict (cap : Nat) (s₁ s₂ : Spine) (sol : Sol Unit) : DepVerdict :=
  let p  := traceSpineM cap s₁ s₂
  let G  := p.2.1
  let L  := p.2.2.1
  let n  := G.length + 1
  let es := solEdges sol
  let li := fun (x : Bool × TyVar) => L.findIdx (· == x)
  { agree    := resStr p.1 == resStr (unifySpineM cap s₁ s₂)
    gEdges   := G.length
    gAcyc    := gAcyclic G
    uncov    := (es.filter (fun e =>
                   !(depReach G [e.1.2]).contains e.2.2 || e.1.2 == e.2.2)).length
    depthBad := (es.filter (fun e => !(gDepth G n e.2.2 < gDepth G n e.1.2))).length
    untagged := gAcyclic (solEdgesU sol)
    ledHole  := (sol.domS.filter (fun x => !L.contains x)).length
    ledInc   := es.all (fun e => li e.1 < li e.2)
    ledDec   := es.all (fun e => li e.2 < li e.1)
    coll     := p.2.2.2 }

--------------------------------- GENERATION ----------------------------------
-- Exhaustive, not random: reproducible, and at these sizes complete coverage of
-- the universe is cheaper than a generator plus a seed to remember.

structure Universe where
  name   : String
  vars   : List TyVar          -- row variables usable as spine atoms
  labels : List Label
  tys    : List (Ty Unit)      -- field payloads
  maxLen : Nat                 -- spines of length 0 .. maxLen, both sides

/-- Atom alphabet: every row variable, and every label at every payload. -/
def Universe.atoms (U : Universe) : List (Atom Unit) :=
  U.vars.map .var ++ U.labels.flatMap fun l => U.tys.map (Atom.field l ·)

def spinesOfLen (as : List (Atom Unit)) : Nat → List Spine
  | 0     => [[]]
  | n + 1 => (spinesOfLen as n).flatMap fun s => as.map (· :: s)

def allSpines (as : List (Atom Unit)) (n : Nat) : List Spine :=
  (List.range (n + 1)).flatMap (spinesOfLen as)

---------------------------------- THE SEARCH ---------------------------------

def reached : UResM Unit → Bool
  | .outOfFuel => false
  | _          => true

/-- Least `f ∈ [lo, hi]` with `p f`, assuming `p hi`. `gas` is the structural
    recursion budget; `hi - lo + 1` always suffices. -/
def leastSat (p : Nat → Bool) : Nat → Nat → Nat → Nat
  | 0,     _,  hi => hi
  | g + 1, lo, hi =>
      if hi ≤ lo then lo
      else
        let mid := (lo + hi) / 2
        if p mid then leastSat p g lo mid else leastSat p g (mid + 1) hi

/-- The least budget at which the pair reaches a verdict, or `none` if it is
    still `outOfFuel` at `cap`. Binary search is sound here by
    `unifyM_fuel_mono` — and step (2) of the report checks that. -/
def minFuel (cap : Nat) (s₁ s₂ : Spine) : Option Nat :=
  let p := fun f => reached (unifySpineM f s₁ s₂)
  if p cap then some (leastSat p (cap + 1) 0 cap) else none

/-- Record-nesting depth of a spine, via the ≈-invariant `Row.rcdDepth`
    (NoMgu.lean). Kept because it is the only ≈-INVARIANT size in the tree
    (`RowEquiv.rcdDepth_eq`) — but it is NOT enough for a fuel bound: its `.fn`
    case takes a `max`, so arrow nesting costs it nothing, and the `fn-d` family
    below duly breaks any bound built from it. Reported for contrast. -/
def sDepth (s : Spine) : Nat := Row.rcdDepth (ofSpine s)

mutual
/-- Constructor count of a type. Not ≈-invariant (`comm` reorders, `unitL/R`
    delete), so a bound stated with it would have to be read on the SPINE the
    driver actually holds, not up to ≈ — a real cost to pay later. -/
def tySize : Ty Unit → Nat
  | .var _    => 1
  | .base _   => 1
  | .unk      => 1
  | .fn τ₁ τ₂ => 1 + tySize τ₁ + tySize τ₂
  | .rcd ρ    => 1 + rowSize ρ

def rowSize : Row Unit → Nat
  | .empty     => 1
  | .var _     => 1
  | .sing _ τ  => 1 + tySize τ
  | .cat ρ₁ ρ₂ => 1 + rowSize ρ₁ + rowSize ρ₂
end

def atomSize : Atom Unit → Nat
  | .var _     => 1
  | .field _ τ => 1 + tySize τ

def sSize (s : Spine) : Nat := (s.map atomSize).foldl (· + ·) 0

/-- CANDIDATE BOUND A, tight. Read off the parametric families: `cancel-n` and
    `crossfield-n` demand `n+1`, `nest-d` demands `2d+1`, `fn-d` demands `d+1` —
    and in each case that is at most half the total constructor count. A tight
    conjecture is the useful kind: it gets refuted, and the refutation names the
    ingredient the measure is missing. -/
def boundA (s₁ s₂ : Spine) : Nat := (sSize s₁ + sSize s₂) / 2 + 1

/-- CANDIDATE BOUND B, loose — the same shape with no halving. If A falls and B
    stands, the bound is linear in the problem with a worse constant. -/
def boundB (s₁ s₂ : Spine) : Nat := sSize s₁ + sSize s₂ + 1

structure Stats where
  total     : Nat := 0
  success   : Nat := 0
  clash     : Nat := 0
  occurs    : Nat := 0
  stuck     : Nat := 0
  /-- still `outOfFuel` at `cap` — divergence candidates -/
  survivors : List (Spine × Spine) := []
  nSurv     : Nat := 0
  /-- result at `minFuel` ≠ result at `cap`: refutes `unifyM_fuel_mono` -/
  monoViol  : List (Spine × Spine × String × String) := []
  nViol     : Nat := 0
  /-- (problem size, max minFuel seen at that size) -/
  bySize    : List (Nat × Nat) := []
  /-- the single hungriest pair -/
  worst     : Option (Nat × Spine × Spine) := none
  /-- minFuel > boundA: refutes the tight candidate -/
  boundViol : List (Nat × Nat × Spine × Spine) := []
  nAViol    : Nat := 0
  /-- minFuel > boundB: refutes the loose candidate too -/
  nBViol    : Nat := 0
  /-- the smallest slack `boundA - minFuel` seen — how close A came -/
  tightest  : Option (Nat × Nat × Spine × Spine) := none
  /-- a success whose solution is not well-formed: refutes `UnifyWF` -/
  idemViol  : List (Spine × Spine × String) := []
  nIdemViol : Nat := 0
  /-- …of which: the ACYCLIC half fails (a genuine cycle) -/
  nAcycViol : Nat := 0
  /-- …of which: only the APPLIED half fails (a TRIANGULAR solution) -/
  nApplViol : Nat := 0
  /-- [6] rank measures. `nEdgy` = successes whose solution has at least one
  dependency edge at all — the only ones where a rank says anything. -/
  nEdgy    : Nat := 0
  nLenDec  : Nat := 0
  nLenInc  : Nat := 0
  nIdxDec  : Nat := 0
  nIdxInc  : Nat := 0
  /-- widest edge count seen, and a witness for each candidate that failed -/
  maxEdges : Nat := 0
  /-- successes no live candidate ranks: neither |name|-increasing nor
  index-decreasing. If this is 0 a hybrid measure covers everything. -/
  nNeither : Nat := 0
  /-- solutions with a shadowed (dead) binding, and index-decreasing failures
  once those are dropped -/
  nDup     : Nat := 0
  nDupDiff : Nat := 0
  wDupDiff : Option (Spine × Spine × String × String) := none
  nIdxDecD : Nat := 0
  wIdxD    : Option (Spine × Spine × String × String) := none
  wLen     : Option (Spine × Spine × String × String) := none
  wIdx     : Option (Spine × Spine × String × String) := none
  /-- [7] the accumulated dependency graph, over the same edgy successes.
  `nDis` is the tripwire on the traced clone: nonzero and nothing else here
  counts. -/
  nDis     : Nat := 0
  nGEmpty  : Nat := 0
  maxG     : Nat := 0
  nGCyc    : Nat := 0
  nUncov   : Nat := 0
  nUncovE  : Nat := 0
  nDepthB  : Nat := 0
  nUntag   : Nat := 0
  wGCyc    : Option (Spine × Spine × String × String) := none
  wUncov   : Option (Spine × Spine × String × String) := none
  wDepth   : Option (Spine × Spine × String × String) := none
  wUntag   : Option (Spine × Spine × String × String) := none
  nLedHole : Nat := 0
  nLedInc  : Nat := 0
  nLedDec  : Nat := 0
  wLed     : Option (Spine × Spine × String × String) := none
  wLedD    : Option (Spine × Spine × String × String) := none
  nColl    : Nat := 0
  nCollE   : Nat := 0
  wColl    : Option (Spine × Spine × String × String) := none

def bumpMax : List (Nat × Nat) → Nat → Nat → List (Nat × Nat)
  | [],            k, v => [(k, v)]
  | (k', v') :: t, k, v => if k' = k then (k', max v v') :: t else (k', v') :: bumpMax t k v

def keep : Nat := 12   -- how many witnesses of each kind to retain

def step (cap : Nat) (s₁ s₂ : Spine) (st : Stats) : Stats :=
  let st := { st with total := st.total + 1 }
  match minFuel cap s₁ s₂ with
  | none   =>
      { st with survivors := if st.nSurv < keep then (s₁, s₂) :: st.survivors else st.survivors
                nSurv     := st.nSurv + 1 }
  | some f =>
      let rMin := resStr (unifySpineM f s₁ s₂)
      let rCap := resStr (unifySpineM cap s₁ s₂)
      let st := if rMin == rCap then st else
        { st with monoViol := if st.nViol < keep then (s₁, s₂, rMin, rCap) :: st.monoViol
                              else st.monoViol
                  nViol    := st.nViol + 1 }
      let bnd := boundA s₁ s₂
      let st := if f ≤ bnd then st else
        { st with boundViol := if st.nAViol < keep then (f, bnd, s₁, s₂) :: st.boundViol
                               else st.boundViol
                  nAViol    := st.nAViol + 1 }
      let st := if f ≤ boundB s₁ s₂ then st else { st with nBViol := st.nBViol + 1 }
      let st := if f ≤ bnd then
                  let slack := bnd - f
                  match st.tightest with
                  -- smaller slack wins; on a tie prefer the LARGER fuel, so the
                  -- witness is an interesting pair rather than the empty one
                  | some (sl, f', _, _) =>
                      if slack < sl || (slack == sl && f > f')
                      then { st with tightest := some (slack, f, s₁, s₂) } else st
                  | none               => { st with tightest := some (slack, f, s₁, s₂) }
                else st
      let st := { st with bySize := bumpMax st.bySize (s₁.length + s₂.length) f }
      let st := match st.worst with
                | some (f', _, _) => if f > f' then { st with worst := some (f, s₁, s₂) } else st
                | none            => { st with worst := some (f, s₁, s₂) }
      match unifySpineM cap s₁ s₂ with
      | .success sol _ =>
          let st := { st with success := st.success + 1 }
          let rv := rankVerdict sol
          let st := if rv.edges == 0 then st else
            { st with
                nEdgy    := st.nEdgy + 1
                maxEdges := max st.maxEdges rv.edges
                nLenDec  := st.nLenDec + (if rv.lenDec then 0 else 1)
                nLenInc  := st.nLenInc + (if rv.lenInc then 0 else 1)
                nIdxDec  := st.nIdxDec + (if rv.idxDec then 0 else 1)
                nIdxInc  := st.nIdxInc + (if rv.idxInc then 0 else 1)
                wLen     := if rv.lenInc then st.wLen else
                              st.wLen.orElse fun _ => some (s₁, s₂, solStr sol,
                                badEdges sol (fun e => e.1.2.length < e.2.2.length))
                wIdx     := if rv.idxDec then st.wIdx else
                              st.wIdx.orElse fun _ => some (s₁, s₂, solStr sol,
                                badEdges sol (fun e => domIdx sol e.2 < domIdx sol e.1))
                nNeither := st.nNeither + (if rv.idxDec || rv.lenInc then 0 else 1)
                nDup     := st.nDup + (if rv.hasDup then 1 else 0)
                nDupDiff := st.nDupDiff + (if solDupDiff sol then 1 else 0)
                wDupDiff := if !solDupDiff sol then st.wDupDiff else
                              st.wDupDiff.orElse fun _ => some (s₁, s₂, solStr sol, "")
                nIdxDecD := st.nIdxDecD + (if rv.idxDecD then 0 else 1)
                wIdxD    := if rv.idxDecD then st.wIdxD else
                              st.wIdxD.orElse fun _ =>
                                let d := solDedup sol
                                some (s₁, s₂, solStr d,
                                  badEdges d (fun e => domIdx d e.2 < domIdx d e.1)) }
          let st := if rv.edges == 0 then st else
            let dv := depVerdict cap s₁ s₂ sol
            let tr := traceSpineM cap s₁ s₂
            let G  := tr.2.1
            { st with
                nDis    := st.nDis    + (if dv.agree then 0 else 1)
                nGEmpty := st.nGEmpty + (if dv.gEdges == 0 then 1 else 0)
                maxG    := max st.maxG dv.gEdges
                nGCyc   := st.nGCyc   + (if dv.gAcyc then 0 else 1)
                nUncov  := st.nUncov  + (if dv.uncov == 0 then 0 else 1)
                nUncovE := st.nUncovE + dv.uncov
                nDepthB := st.nDepthB + (if dv.depthBad == 0 then 0 else 1)
                nUntag  := st.nUntag  + (if dv.untagged then 0 else 1)
                nLedHole := st.nLedHole + dv.ledHole
                nLedInc := st.nLedInc + (if dv.ledInc then 0 else 1)
                nLedDec := st.nLedDec + (if dv.ledDec then 0 else 1)
                nColl   := st.nColl   + (if dv.coll == 0 then 0 else 1)
                nCollE  := st.nCollE  + dv.coll
                wColl   := if dv.coll == 0 then st.wColl else
                             st.wColl.orElse fun _ => some (s₁, s₂, solStr sol,
                               s!"{dv.coll} crossing mention(s); ledger: " ++ ledgerStr tr.2.2.1)
                wLedD   := if dv.ledDec then st.wLedD else
                             st.wLedD.orElse fun _ => some (s₁, s₂, solStr sol,
                               "ledger: " ++ ledgerStr tr.2.2.1 ++ "   dup=" ++
                               toString (solHasDup sol) ++ "   bad at " ++
                               badEdges sol (fun e =>
                                 tr.2.2.1.findIdx (· == e.2) < tr.2.2.1.findIdx (· == e.1)))
                wLed    := if dv.ledInc then st.wLed else
                             st.wLed.orElse fun _ => some (s₁, s₂, solStr sol,
                               "ledger: " ++ ledgerStr tr.2.2.1 ++ "   bad at " ++
                               badEdges sol (fun e =>
                                 tr.2.2.1.findIdx (· == e.1) < tr.2.2.1.findIdx (· == e.2)))
                wGCyc   := if dv.gAcyc then st.wGCyc else
                             st.wGCyc.orElse fun _ => some (s₁, s₂, solStr sol, gStr G)
                wUncov  := if dv.uncov == 0 then st.wUncov else
                             st.wUncov.orElse fun _ => some (s₁, s₂, solStr sol,
                               "Θ = " ++ gStr G ++ "   misses " ++
                               badEdges sol (fun e =>
                                 (depReach G [e.1.2]).contains e.2.2 && e.1.2 != e.2.2))
                wDepth  := if dv.depthBad == 0 then st.wDepth else
                             st.wDepth.orElse fun _ => some (s₁, s₂, solStr sol,
                               "Θ = " ++ gStr G ++ "   flat/rising at " ++
                               badEdges sol (fun e =>
                                 gDepth G (G.length+1) e.2.2 < gDepth G (G.length+1) e.1.2))
                wUntag  := if dv.untagged then st.wUntag else
                             st.wUntag.orElse fun _ => some (s₁, s₂, solStr sol,
                               "untagged edges: " ++ badEdges sol (fun _ => false)) }
          if solWFB sol then st else
            { st with idemViol := if st.nIdemViol < keep
                                  then (s₁, s₂, solStr sol) :: st.idemViol else st.idemViol
                      nIdemViol := st.nIdemViol + 1
                      nAcycViol := st.nAcycViol + (if solAcyclicB sol then 0 else 1)
                      nApplViol := st.nApplViol + (if solRankedB sol then 0 else 1) }
      | .clash       => { st with clash   := st.clash   + 1 }
      | .occurs      => { st with occurs  := st.occurs  + 1 }
      | .stuck       => { st with stuck   := st.stuck   + 1 }
      | .outOfFuel   => st   -- unreachable: minFuel returned `some`

def sweep (U : Universe) (cap : Nat) : Stats :=
  let ss := allSpines U.atoms U.maxLen
  ss.foldl (fun st s₁ => ss.foldl (fun st s₂ => step cap s₁ s₂ st) st) {}

---------------------------------- REPORTING ----------------------------------

def pairStr (s₁ s₂ : Spine) : String := spineStr s₁ ++ "  ≐ᵣ  " ++ spineStr s₂

def report (U : Universe) (cap : Nat) : IO Unit := do
  let nA := U.atoms.length
  let nS := (allSpines U.atoms U.maxLen).length
  IO.println s!"── universe {U.name} ─ {nA} atoms, spines ≤ {U.maxLen}, {nS} spines, {nS*nS} pairs, cap {cap}"
  let st := sweep U cap
  IO.println s!"   {st.total} pairs: {st.success} success, {st.clash} clash, {st.occurs} occurs, {st.stuck} stuck"

  IO.println s!"   [1] divergence candidates (outOfFuel at cap {cap}): {st.nSurv}"
  for (s₁, s₂) in st.survivors.reverse do
    IO.println s!"        {pairStr s₁ s₂}"

  IO.println s!"   [2] fuel-monotonicity violations: {st.nViol}"
  for (s₁, s₂, a, b) in st.monoViol.reverse do
    IO.println s!"        {pairStr s₁ s₂}\n          at minFuel: {a}\n          at cap:     {b}"

  IO.println s!"   [3] candidate-bound violations — A (tight): {st.nAViol}, B (loose): {st.nBViol}"
  for (f, b, s₁, s₂) in st.boundViol.reverse do
    IO.println s!"        fuel {f} > boundA {b}:  {pairStr s₁ s₂}"
  match st.tightest with
  | some (sl, f, s₁, s₂) => IO.println s!"        A's tightest: slack {sl} (fuel {f}) at {pairStr s₁ s₂}"
  | none                 => pure ()

  IO.println s!"   [4] ill-formed solutions (refutes UnifyWF): {st.nIdemViol}"
  IO.println s!"         of which spine-cyclic (Acyclic fails): {st.nAcycViol}; unrankable (Ranked fails): {st.nApplViol}"
  for (s₁, s₂, sol) in st.idemViol.reverse do
    IO.println s!"        {pairStr s₁ s₂}\n          solution: {sol}"

  IO.println s!"   [6] rank measures, over the {st.nEdgy} successes whose solution has any edge (max {st.maxEdges} edges):"
  IO.println s!"        |name| decreasing: {st.nLenDec} fail    |name| increasing: {st.nLenInc} fail"
  IO.println s!"        domS-index decr.:  {st.nIdxDec} fail    domS-index incr.:  {st.nIdxInc} fail"
  match st.wLen with
  | some (s₁, s₂, sol, bad) =>
      IO.println s!"        |name|-increasing fails first at {pairStr s₁ s₂}"
      IO.println s!"          solution: {sol}"
      IO.println s!"          bad edges: {bad}"
  | none => IO.println "        |name|-increasing: no counterexample"
  IO.println s!"        ranked by NEITHER live candidate: {st.nNeither}"
  IO.println s!"        with a SHADOWED binding: {st.nDup}    index-decr. after dedup: {st.nIdxDecD} fail"
  IO.println s!"        …of which the two bindings DISAGREE: {st.nDupDiff}"
  match st.wDupDiff with
  | some (s₁, s₂, sol, _) =>
      IO.println s!"        disagreeing shadow first at {pairStr s₁ s₂}"
      IO.println s!"          solution: {sol}"
  | none => IO.println "        disagreeing shadow: NO COUNTEREXAMPLE"
  match st.wIdxD with
  | some (s₁, s₂, sol, bad) =>
      IO.println s!"        index-decr.-after-dedup fails first at {pairStr s₁ s₂}"
      IO.println s!"          deduped solution: {sol}"
      IO.println s!"          bad edges: {bad}"
  | none => IO.println "        index-decreasing after dedup: NO COUNTEREXAMPLE"
  match st.wIdx with
  | some (s₁, s₂, sol, bad) =>
      IO.println s!"        index-DECREASING fails first at {pairStr s₁ s₂}"
      IO.println s!"          solution: {sol}"
      IO.println s!"          bad edges: {bad}"
  | none => IO.println "        index-decreasing: no counterexample"

  IO.println s!"   [7] the accumulated Θ, over those same {st.nEdgy} successes (max {st.maxG} edges):"
  IO.println s!"        traced clone DISAGREES with the driver: {st.nDis}   (nonzero ⟹ ignore the rest)"
  IO.println s!"        Θ EMPTY though the solution has edges: {st.nGEmpty}"
  IO.println s!"        Θ itself cyclic: {st.nGCyc}"
  IO.println s!"        solution edges Θ-reachability MISSES: {st.nUncovE} (in {st.nUncov} successes)"
  IO.println s!"        Θ-depth not strictly decreasing: {st.nDepthB}"
  IO.println s!"        solution edge graph UNTAGGED is cyclic: {st.nUntag}"
  IO.println s!"        ── creation order (the tagged, COMPLETE ledger the clone records)"
  IO.println s!"        keys the ledger missed: {st.nLedHole}   (nonzero ⟹ the clone is incomplete)"
  IO.println s!"        ledger-index INCREASING (mentions only later): {st.nLedInc} fail"
  IO.println s!"        ledger-index DECREASING (mentions only earlier): {st.nLedDec} fail"
  IO.println s!"        ── `Sol.comp` preservation: a LATER stage mentioning the EARLIER stage's domain"
  IO.println s!"        crossing mentions: {st.nCollE} (in {st.nColl} successes)"
  for (nm, w) in [("Θ cyclic", st.wGCyc), ("Θ-reach misses", st.wUncov),
                  ("Θ-depth flat", st.wDepth), ("untagged cycle", st.wUntag),
                  ("ledger-increasing", st.wLed), ("ledger-DECREASING", st.wLedD),
                  ("stage collision", st.wColl)] do
    match w with
    | some (s₁, s₂, sol, note) =>
        IO.println s!"        {nm} first at {pairStr s₁ s₂}"
        IO.println s!"          solution: {sol}"
        IO.println s!"          {note}"
    | none => IO.println s!"        {nm}: NO COUNTEREXAMPLE"

  IO.println "   [5] fuel profile — max minFuel by problem size |s₁|+|s₂|:"
  for n in List.range (2 * U.maxLen + 1) do
    match st.bySize.find? (fun p => p.1 = n) with
    | some (_, v) => IO.println s!"        size {n}: {v}"
    | none        => pure ()
  match st.worst with
  | some (f, s₁, s₂) => IO.println s!"        hungriest: fuel {f} for {pairStr s₁ s₂}"
  | none             => pure ()
  IO.println ""

---------------------------------- UNIVERSES ----------------------------------
-- Row variables and type variables share ONE namespace (minimal.lean:649), so a
-- payload `.var "a"` alongside a spine atom `.var "a"` is the cross-sort
-- aliasing that makes solve-and-apply feed U-expand — the exact mechanism that
-- killed the lexicographic measure. Both universes below contain it.
--
-- Invented names are `natName n` = n copies of 'a', drawn from `localSupply` =
-- (longest name in the problem) + 1. Keeping every variable one character long
-- means the supply starts at "aa" and can never collide with a name below.

/-- Rich payloads, short spines. Reaches ★, functions, and nested records —
    `{l:{…}}` is the shape behind `terminal_masks_mgu`, whose occurs violation
    passes UNDER a record constructor. -/
def wide : Universe :=
  { name   := "wide"
    vars   := ["a", "b"]
    labels := ["l", "m"]
    tys    := [.base (), .unk, .var "a", .var "b",
               .rcd (.var "a"), .rcd (.sing "l" (.var "a")), .fn (.var "a") (.base ())]
    maxLen := 2 }

/-- Payloads that are themselves multi-atom ROWS. This is the shape the other
    two universes miss and the one the retracted measure died on: a type
    equation solved inside a field can expand a row variable and hand the
    invented tail to the residual, so both the spine length and the variable
    count grow. `{β|α}` is literally the payload in `stuck_masks_mgu`. -/
def nest : Universe :=
  { name   := "nest"
    vars   := ["a", "b"]
    labels := ["l", "k"]
    tys    := [.base (), .var "a",
               .rcd (.cat (.var "a") (.var "b")),
               .rcd (.cat (.sing "l" (.base ())) (.var "a")),
               .rcd (.sing "l" (.var "a"))]
    maxLen := 2 }

/-- Narrow payloads, longer spines. Three variables and length 3 is where the
    Wand shape (a field with several candidate hosts) and repeated expansion
    have room to interact. -/
def deep : Universe :=
  { name   := "deep"
    vars   := ["a", "b", "c"]
    labels := ["l", "m"]
    tys    := [.base (), .var "a", .rcd (.var "a")]
    maxLen := 3 }

--------------------------- PARAMETRIC SCALING --------------------------------
-- Exhaustive search over a small universe can only say "no divergence HERE".
-- The families below say something a measure can be checked against: how the
-- fuel demand GROWS along a parameter. A linear profile is what a bound of the
-- shape `c · (|s₁| + |s₂|)` predicts; anything super-linear tells you which
-- ingredient the measure has to charge for.

def vv (i : Nat) : Atom Unit := .var ("v" ++ toString i)
def ww (i : Nat) : Atom Unit := .var ("w" ++ toString i)
def lf (i : Nat) : Atom Unit := .field ("l" ++ toString i) (.base ())
def mf (i : Nat) : Atom Unit := .field ("m" ++ toString i) (.base ())

def upto (n : Nat) (f : Nat → Atom Unit) : Spine := (List.range n).map f

/-- A record nested `d` deep, ending in `𝓫`. -/
def nestB : Nat → Ty Unit
  | 0     => .base ()
  | d + 1 => .rcd (.sing "l" (nestB d))

/-- Arrows nested `d` deep. `Ty.rcdDepth` charges NOTHING for these — its `.fn`
    case takes a `max`, not a `1 +` — while the type pass still has to walk
    them. If the candidate bound is wrong, this is where it breaks. -/
def arrB : Nat → Ty Unit
  | 0     => .base ()
  | d + 1 => .fn (.base ()) (arrB d)

def arrV : Nat → Ty Unit
  | 0     => .var "x"
  | d + 1 => .fn (.base ()) (arrV d)

/-- The same, ending in a variable — so the pair has a unique mgu at depth `d`. -/
def nestV : Nat → Ty Unit
  | 0     => .var "x"
  | d + 1 => .rcd (.sing "l" (nestV d))

structure Family where
  name : String
  note : String
  gen  : Nat → Spine × Spine

def families : List Family :=
  [ { name := "wand-n",      note := "n variables vs one field — stuck, and must STAY cheap"
      gen := fun n => (upto n vv, [.field "l" (.base ())]) },
    { name := "cancel-n",    note := "n shared vars on both ends — U-var-refl only"
      gen := fun n => (upto n vv ++ [.field "l" (.base ())],
                      upto n vv ++ [.field "l" (.base ())]) },
    { name := "collapse-n",  note := "n vars must all vanish against a var-free side"
      gen := fun n => (upto n vv ++ [.field "l" (.base ())], [.field "l" (.base ())]) },
    { name := "absorb-n",    note := "n distinct fields absorbed into one variable"
      gen := fun n => (upto n lf, [.var "v0"]) },
    { name := "crossfield-n", note := "n disjoint fields each side — the EXPANSION engine"
      gen := fun n => (upto n lf ++ [.var "v0"], upto n mf ++ [.var "w0"]) },
    { name := "interleave-n", note := "expansion with the hosts already carrying fields"
      gen := fun n => (upto n lf ++ [.var "v0"] ++ upto n mf,
                      upto n mf ++ [.var "w0"] ++ upto n lf) },
    { name := "nest-d",      note := "record nesting depth d — the type pass recursing"
      gen := fun d => ([.field "k" (nestB d)], [.field "k" (nestV d)]) },
    { name := "fn-d",        note := "ARROW nesting depth d — rcdDepth cannot see this"
      gen := fun d => ([.field "k" (arrB d)], [.field "k" (arrV d)]) },
    { name := "nestwand-d",  note := "nesting under a Wand payload"
      gen := fun d => ([.field "k" (.rcd (.cat (.var "a") (.var "b")))],
                      [.field "k" (nestB d)]) } ]

def familyLine (F : Family) (cap n : Nat) : IO Bool := do
  let mut out := ""
  let mut blew := false
  for i in List.range (n + 1) do
    let (s₁, s₂) := F.gen i
    let v := verdictStr (unifySpineM cap s₁ s₂)
    let tag := v.take 2
    match minFuel cap s₁ s₂ with
    | some f =>
        let bnd := boundA s₁ s₂
        out := out ++ s!" {i}:{f}{tag}" ++ (if f ≤ bnd then "" else s!"!>{bnd}")
        blew := blew || !(f ≤ bnd)
    | none   => out := out ++ s!" {i}:>{cap}{tag}"; blew := true
  IO.println s!"   {F.name}"
  IO.println s!"      {F.note}"
  IO.println s!"      n:fuel {out}"
  return blew

def familyReport (cap n : Nat) : IO Unit := do
  IO.println s!"── parametric families (n = 0 … {n}, cap {cap}; tag = first 2 letters of the verdict)"
  let mut anyBlew := false
  for F in families do
    let b ← familyLine F cap n
    anyBlew := anyBlew || b
  if anyBlew then
    IO.println "   !! a family did not reach a verdict within cap, or broke the candidate"
    IO.println "      bound (marked !>bound) — DIVERGENCE / BOUND CANDIDATE"
  IO.println ""

------------------------------- KNOWN LANDMARKS -------------------------------
-- Sanity: the harness must reproduce the verdicts the build already pins down.
-- If one of these lines disagrees with Regressions.lean the harness is wrong,
-- not the algorithm.

def landmarks : List (String × Spine × Spine) :=
  [ ("wand (stuck, no mgu)",
       [.var "b", .var "a"], [.field "l" (.base ())]),
    ("lutail (success, α ≔ ε)",
       [.field "l" (.base ())], [.var "a", .field "l" (.base ())]),
    ("shared tail (clash)",
       [.field "l" (.base ()), .var "a"], [.field "m" (.base ()), .var "a"]),
    ("all-var occurrence (success — the ε-collapse, β,γ ≔ ε)",
       [.var "a"], [.var "b", .var "a", .var "c"]),
    ("all-var occurrence, k = 2 (success — α collapses too)",
       [.var "a"], [.var "b", .var "a", .var "a", .var "c"]),
    ("stuck_masks_mgu (conservative — unique mgu)",
       [.field "k" (.rcd (.cat (.var "b") (.var "a"))), .var "b"],
       [.field "k" (.rcd (.sing "l" (.base ()))), .field "l" (.base ())]),
    ("terminal_masks_mgu (WAS terminal — expandR now solves it)",
       [.field "l" (.rcd (.var "w"))], [.var "w", .var "v"]) ]

def landmarkReport (cap : Nat) : IO Unit := do
  IO.println "── landmarks (must agree with Regressions.lean / Refutations.lean)"
  for (nm, s₁, s₂) in landmarks do
    let v := verdictStr (unifySpineM cap s₁ s₂)
    let f := match minFuel cap s₁ s₂ with | some f => toString f | none => "—"
    IO.println s!"   {v} (fuel {f})  {nm}\n        {pairStr s₁ s₂}"
  IO.println ""

def main : IO Unit := do
  let cap := 64
  IO.println "≐ᵣ divergence hunt — Phase D triage\n"
  landmarkReport cap
  report wide cap
  report deep cap
  report nest cap
  familyReport 4000 24
  IO.println "A pair still outOfFuel at cap, with a spine of ≤ 6 atoms, is a"
  IO.println "divergence candidate: minimize it by hand and it becomes a theorem."

end Fuzz
end MinimalCalculus

def main : IO Unit := MinimalCalculus.Fuzz.main
