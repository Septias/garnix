-- FUZZ: an executable REFUTER for the ≐ᵣ / ≐ driver. Every property the
-- metatheory needs but has not proved is compiled to a Bool here and run over
-- every pair of a small universe, so a candidate invariant dies in an afternoon
-- rather than in a week of Lean.
--
-- Not part of the library (`lake build` does not touch it). Run with
--
--     lake build fuzz && lake exe fuzz
--
-- ## Why
-- It began as Phase D triage: there is no closed-form fuel bound, the naive
-- Rémy measure does not close, and the lexicographic measure was retracted
-- because solve-and-apply grows the spine AND the variable count. Before
-- designing a third measure, settle the prior question — is the driver
-- terminating at all? That question is now as settled as a sweep can settle it
-- (section [1] has no survivor in any universe, and the parametric families stay
-- linear), and the harness has been kept for the job it turned out to be better
-- at: REFUTING the invariants a proof would otherwise be built on. Every finding
-- is logged, with the pair that produced it, in ../typesystems/proof-state.md —
-- whose REPRODUCE lines point at the section numbers below, so those are fixed.
--
-- ## What it runs
-- `report`, over the universes `wide` / `deep` / `nest`: every pair of spines
-- drawn from a small atom alphabet, exhaustively rather than at random.
--  [1] DIVERGENCE HUNT. `minFuel` = the least budget at which the pair reaches a
--      verdict, by binary search up to `cap`. A pair still `outOfFuel` at `cap`
--      — with `cap` far above its size — is a divergence CANDIDATE and gets
--      printed. Zero survivors is evidence for termination, not a proof.
--  [2] FUEL-MONOTONICITY TRIPWIRE. `unifyM_fuel_mono` says a verdict once
--      REACHED never changes as the budget grows. So the full result (verdict
--      AND solution) at `minFuel` must equal the one at `cap`. Any mismatch is a
--      counterexample to a theorem in the build — print it loudly.
--  [3] CANDIDATE FUEL BOUNDS. `boundA` (tight) and `boundB` (loose), both read
--      off the parametric families, checked against `minFuel` on every pair.
--  [4] SOLUTION WELL-FORMEDNESS. `Sol.WF` = `Acyclic` ∧ `Ranked` (State.lean),
--      decided on every success. `UnifyWF` — the claim that the driver RETURNS
--      such a solution — is NOT proved; this is its tripwire.
--  [5] FUEL PROFILE. The max `minFuel` observed per problem size |s₁|+|s₂|.
--      This is the empirical shape of the bound a measure would have to beat: if
--      it is flat or linear the Rémy-style argument is worth the effort, if it
--      climbs fast the bound is not what one would guess.
--  [6] WHICH RANK MEASURE WORKS. `Sol.Ranked` is an ∃-rank, so a proof needs a
--      rank the driver can EXHIBIT: candidates (|name|, `domS` index, both
--      directions, and again with shadowed bindings dropped) are run against the
--      solution's own dependency edges, together with the count of SHADOWED
--      bindings — which is where the sweep's current lead came from.
--  [7] THE ACCUMULATED Θ, and creation order. Measured with `traceSpineM`, a
--      clone of the driver that also returns the expansion graph and a LEDGER of
--      every key in the order it was bound; Θ is an input to the real driver and
--      never returned, so it cannot be observed any other way.
-- then `familyReport` — the parametric `families`, where the fuel demand is read
-- as a function of n instead of over a fixed universe — and `landmarkReport`,
-- which must agree with Regressions.lean / Refutations.lean or it is the
-- harness that is wrong, not the algorithm.
--
-- STAGE 0 (`nxMain`) runs the landmarks, universes and families a second time
-- against `nxSpineMF`, a clone with the four U-expand arms stubbed to `.stuck`,
-- to price that arm before deleting it. Its own section comment carries the
-- prediction it checks.
--
-- Two numbers are LOAD-BEARING: `nDis` ([7]) and `nAnom` (Stage 0), each saying
-- that its clone still agrees with the real driver. Nonzero, and nothing else in
-- that section means anything.
--
-- The binary search in [1], [3] and [5] is justified by `unifyM_fuel_mono`:
-- "reaches a verdict" is monotone in the budget. [2] is what checks that.

import RowUnify

namespace MinimalCalculus
namespace Fuzz

abbrev Spine := List (Atom Unit)

---------------------------------- PRINTING -----------------------------------

def keyStr : Key → String
  | .lit l => l
  | .var α => α

mutual
def tyStr : Ty Unit → String
  | .var α    => α
  | .base _   => "𝓫"
  | .lab k    => "⌊" ++ keyStr k ++ "⌋"
  | .unk      => "★"
  | .fn τ₁ τ₂ => "(" ++ tyStr τ₁ ++ "→" ++ tyStr τ₂ ++ ")"
  | .rcd ρ    => "{" ++ rowStr ρ ++ "}"

def rowStr : Row Unit → String
  | .empty     => "ε"
  | .var α     => α
  | .sing l τ  => l ++ ":" ++ tyStr τ
  | .cat ρ₁ ρ₂ => rowStr ρ₁ ++ " | " ++ rowStr ρ₂
  | .dsing k τ => "${" ++ keyStr k ++ "}:" ++ tyStr τ
end

def atomStr : Atom Unit → String
  | .field l τ  => l ++ ":" ++ tyStr τ
  | .dfield α τ => "${" ++ α ++ "}:" ++ tyStr τ
  | .var α      => α

def spineStr (s : Spine) : String :=
  "(" ++ String.intercalate " | " (s.map atomStr) ++ ")"

def solStr (s : Sol Unit) : String :=
  "[" ++ String.intercalate ", " (s.ty.map  fun p => p.1 ++ "≔" ++ tyStr p.2) ++
  " ; " ++ String.intercalate ", " (s.row.map fun p => p.1 ++ "≔" ++ rowStr p.2) ++
  " ; " ++ String.intercalate ", " (s.lab.map fun p => p.1 ++ "≔" ++ keyStr p.2) ++ "]"

-- Structural equality on the syntax, so the semantic half of `Sol.WF` can be
-- decided. (`Ty`/`Row` derive no `DecidableEq`: the algorithm only ever needs
-- `DecidableEq B` at the leaves.)
mutual
def tyEqB : Ty Unit → Ty Unit → Bool
  | .var a,    .var b     => a == b
  | .base _,   .base _    => true
  | .lab k,    .lab k'    => decide (k = k')
  | .unk,      .unk       => true
  | .fn a b,   .fn c d    => tyEqB a c && tyEqB b d
  | .rcd r,    .rcd r'    => rowEqB r r'
  | _,         _          => false

def rowEqB : Row Unit → Row Unit → Bool
  | .empty,    .empty     => true
  | .var a,    .var b     => a == b
  | .sing l τ, .sing l' τ' => l == l' && tyEqB τ τ'
  | .cat a b,  .cat c d   => rowEqB a c && rowEqB b d
  | .dsing k τ, .dsing k' τ' => decide (k = k') && tyEqB τ τ'
  | _,         _          => false
end

/-- The ACYCLIC half of `Sol.WF` (RowUnify/State.lean) as a Bool: no bound row
    variable is reachable at a spine position of a binding. It used to be what
    made the `L-α` chase terminate; with ↓ context-free no lookup depends on it,
    and it remains a tripwire for `UnifyWF` — the claim that the driver RETURNS
    a well-formed solution.

    The syntactic `Sol.NoCapture` ("no bound variable occurs in any binding")
    would be sufficient for everything, and is refuted here in one move by
    `a ≐ᵣ (l:a)` ⇝ `a ≔ (l:a | ε)`, where the bound `a` is a ROW variable and the
    payload `a` a TYPE variable — different variables that `ftv`, spanning one
    untagged namespace, cannot tell apart. -/
def solAcyclicB (s : Sol Unit) : Bool :=
  let keys := s.row.map Prod.fst
  s.row.all (fun p => (sVarSeq p.2.toSpine).all (fun β => !keys.contains β))

/-- `Sol.Applied` as a Bool. NOT part of `Sol.WF` — State.lean drops the demand
    for an applied solution, the driver's being triangular, and asks for a
    CLOSURE instead. So this is not in the [4] tripwire; Stage 0 is its only
    consumer, where the question is whether triangularity comes from U-expand. -/
def solAppliedB (s : Sol Unit) : Bool :=
  s.ty.all  (fun p => tyEqB (p.2.applySubst s.toSubst) p.2) &&
  s.row.all (fun p => rowEqB (p.2.applySubst s.toSubst) p.2) &&
  s.lab.all (fun p => decide (p.2.applySubst s.toSubst = p.2))

/-- `Sol.Ranked` as a Bool: the sorted dependency graph on the solution's
    bindings is a DAG. A rank exists exactly when it is — take the longest-path
    depth, which is below the number of bindings — so this is the decidable face
    of the ∃-rank in `Sol.Ranked`, and with `Sol.closes_closure` it is what says
    ⟦S⟧ EXISTS for this solution. It replaces the `Applied` half of the old
    tripwire, which was false by design against a triangular solution. -/
def solDeps (s : Sol Unit) : Srt × TyVar → List (Srt × TyVar)
  | (.ty, α) =>
      ((s.ty.filter  (fun p => p.1 == α)).flatMap (fun p => Ty.sortedFtv  p.2)).filter
        (fun y => s.domS.contains y)
  | (.row, α) =>
      ((s.row.filter (fun p => p.1 == α)).flatMap (fun p => Row.sortedFtv p.2)).filter
        (fun y => s.domS.contains y)
  | (.lab, α) =>
      ((s.lab.filter (fun p => p.1 == α)).flatMap (fun p => Key.sortedFtv p.2)).filter
        (fun y => s.domS.contains y)

/-- Peel nodes with no surviving dependency; a DAG empties, a cycle stalls. -/
def peelDeps (s : Sol Unit) : Nat → List (Srt × TyVar) → Bool
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
-- WHAT THE ANSWER TURNED OUT TO BE. Every candidate was refuted while U-expand
-- was in the driver, and a sixth — reachability through the threaded `DepGraph`
-- — was measured by a clone (the section below, now deleted). Since Stage 1b
-- the question is VACUOUS rather than answered: every arm solves and applies,
-- so no solution mentions a variable in its own domain, `nEdgy` is 0, and rank
-- ≡ 0 witnesses `Sol.Ranked`. The candidates stay measured as a TRIPWIRE — a
-- nonzero `nEdgy` means an arm that does not apply its solution has come back.

/-- Every dependency edge the solution imposes: from a binding's KEY to a
variable in its value that the solution also binds. `Sol.Ranked` is exactly the
existence of a rank that strictly decreases along all of these. -/
def solEdges (s : Sol Unit) : List ((Srt × TyVar) × (Srt × TyVar)) :=
  (s.ty.flatMap  (fun p => ((Ty.sortedFtv  p.2).filter (fun y => s.domS.contains y)).map
                             (fun y => ((.ty, p.1), y)))) ++
  (s.row.flatMap (fun p => ((Row.sortedFtv p.2).filter (fun y => s.domS.contains y)).map
                             (fun y => ((.row, p.1), y)))) ++
  (s.lab.flatMap (fun p => ((Key.sortedFtv p.2).filter (fun y => s.domS.contains y)).map
                             (fun y => ((.lab, p.1), y))))

def domIdx (s : Sol Unit) (x : Srt × TyVar) : Nat := s.domS.findIdx (· == x)

/-- keep the FIRST binding per key — which is the only one `rowLookup`/`tyLookup`
ever reads, every later one being dead. `Sol.Ranked` quantifies over ALL of them,
so it constrains bindings ⟦S⟧ cannot see; this is the same solution with the dead
weight dropped, and the measurement below is what it is for. -/
def dedupKeys {α : Type} (l : List (TyVar × α)) : List (TyVar × α) :=
  (l.foldl (fun acc p => if acc.any (fun q => q.1 == p.1) then acc else p :: acc) []).reverse

def solDedup (s : Sol Unit) : Sol Unit := ⟨dedupKeys s.ty, dedupKeys s.row, dedupKeys s.lab⟩

/-- a key bound TWICE with values that are not syntactically the same row/type.
`Sol.toSubst` reads only the FIRST binding, while `Sol.Sat` quantifies over
every pair — so where these disagree, ⟦S⟧ and `Sat` are talking about different
substitutions. proof-state flags this as a SUB-OBLIGATION with no invariant
behind it; this counts it. -/
def solDupDiff (s : Sol Unit) : Bool :=
  s.ty.any  (fun p => s.ty.any  (fun q => p.1 == q.1 && !tyEqB  p.2 q.2)) ||
  s.row.any (fun p => s.row.any (fun q => p.1 == q.1 && !rowEqB p.2 q.2)) ||
  s.lab.any (fun p => s.lab.any (fun q => p.1 == q.1 && !decide (p.2 = q.2)))

def solHasDup (s : Sol Unit) : Bool :=
  (dedupKeys s.ty).length != s.ty.length || (dedupKeys s.row).length != s.row.length ||
    (dedupKeys s.lab).length != s.lab.length

def nodeStr (x : Srt × TyVar) : String := (match x.1 with | .row => "ᵣ" | .ty => "ₜ" | .lab => "ₗ") ++ x.2

def edgeStr (e : (Srt × TyVar) × (Srt × TyVar)) : String :=
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
def badEdges (s : Sol Unit) (p : ((Srt × TyVar) × (Srt × TyVar)) → Bool) : String :=
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
-- THIS SECTION IS GONE (Stage 1b, plans/drop-expand.md). It held a second clone
-- of the driver — `traceTyF` / `traceSpineMF` / `traceSpineM` — that returned
-- every expansion edge created anywhere in the executed recursion tree, the
-- graph utilities to test it (`gAcyclic`, `gDepth`, `solEdgesU`), and
-- `depVerdict`, which asked whether `Θ` carried the topological order a
-- `Sol.Ranked` witness needs.
--
-- It was instrumentation for a question that no longer exists. `Θ` was the
-- driver's record of what U-EXPAND had woven together, and with no expansion arm
-- the graph is `[]` at every node — there is nothing to accumulate, nothing to
-- clone, and no rank to carry. Section [6] below keeps the measurement that
-- replaced it: every success is `Applied`, so no solution mentions a variable in
-- its own domain and the rank question is vacuous rather than answered.

--------------------------------- GENERATION ----------------------------------
-- Exhaustive, not random: reproducible, and at these sizes complete coverage of
-- the universe is cheaper than a generator plus a seed to remember.

structure Universe where
  name   : String
  vars   : List TyVar          -- row variables usable as spine atoms
  labels : List Label
  tys    : List (Ty Unit)      -- field payloads
  maxLen : Nat                 -- spines of length 0 .. maxLen, both sides
  keys   : List TyVar := []    -- label variables keying `${k}:` fields (phase B)

/-- Atom alphabet: every row variable, and every label and key at every payload. -/
def Universe.atoms (U : Universe) : List (Atom Unit) :=
  U.vars.map .var ++ U.labels.flatMap (fun l => U.tys.map (Atom.field l ·)) ++
    U.keys.flatMap fun k => U.tys.map (Atom.dfield k ·)

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
  | .lab _    => 1
  | .unk      => 1
  | .fn τ₁ τ₂ => 1 + tySize τ₁ + tySize τ₂
  | .rcd ρ    => 1 + rowSize ρ

def rowSize : Row Unit → Nat
  | .empty     => 1
  | .var _     => 1
  | .sing _ τ  => 1 + tySize τ
  | .cat ρ₁ ρ₂ => 1 + rowSize ρ₁ + rowSize ρ₂
  | .dsing _ τ => 1 + tySize τ
end

def atomSize : Atom Unit → Nat
  | .var _     => 1
  | .field _ τ => 1 + tySize τ
  | .dfield _ τ => 1 + tySize τ

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
  /-- [4] a success whose solution is not well-formed: refutes `UnifyWF` -/
  wfViol    : List (Spine × Spine × String) := []
  nWFViol   : Nat := 0
  /-- …of which: the ACYCLIC half fails (a genuine cycle) -/
  nAcycViol : Nat := 0
  /-- …of which: the RANKED half fails (no rank on the bindings exists) -/
  nRankViol : Nat := 0
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
          if solWFB sol then st else
            { st with wfViol  := if st.nWFViol < keep
                                  then (s₁, s₂, solStr sol) :: st.wfViol else st.wfViol
                      nWFViol   := st.nWFViol + 1
                      nAcycViol := st.nAcycViol + (if solAcyclicB sol then 0 else 1)
                      nRankViol := st.nRankViol + (if solRankedB sol then 0 else 1) }
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

  IO.println s!"   [4] ill-formed solutions (refutes UnifyWF): {st.nWFViol}"
  IO.println s!"         of which spine-cyclic (Acyclic fails): {st.nAcycViol}; unrankable (Ranked fails): {st.nRankViol}"
  for (s₁, s₂, sol) in st.wfViol.reverse do
    IO.println s!"        {pairStr s₁ s₂}\n          solution: {sol}"

  IO.println "   [5] fuel profile — max minFuel by problem size |s₁|+|s₂|:"
  for n in List.range (2 * U.maxLen + 1) do
    match st.bySize.find? (fun p => p.1 = n) with
    | some (_, v) => IO.println s!"        size {n}: {v}"
    | none        => pure ()
  match st.worst with
  | some (f, s₁, s₂) => IO.println s!"        hungriest: fuel {f} for {pairStr s₁ s₂}"
  | none             => pure ()

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

/-- Keyed fields (FC-labels phase B): `${p}:` / `${q}:` barriers next to a
    literal field, with label-typed and keyed payloads, so the label arm of the
    unifier and the dfield barriers both get exercised. -/
def keyed : Universe :=
  { name   := "keyed"
    vars   := ["a", "b"]
    labels := ["l"]
    keys   := ["p", "q"]
    tys    := [.base (), .var "a", .lab (.var "p"), .lab (.lit "l"),
               .rcd (.dsing (.var "p") (.base ()))]
    maxLen := 2 }

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
    ("terminal_masks_mgu (terminal, conservative — unique mgu)",
       [.field "l" (.rcd (.var "w"))], [.var "w", .var "v"]) ]

def landmarkReport (cap : Nat) : IO Unit := do
  IO.println "── landmarks (must agree with Regressions.lean / Refutations.lean)"
  for (nm, s₁, s₂) in landmarks do
    let v := verdictStr (unifySpineM cap s₁ s₂)
    let f := match minFuel cap s₁ s₂ with | some f => toString f | none => "—"
    IO.println s!"   {v} (fuel {f})  {nm}\n        {pairStr s₁ s₂}"
  IO.println ""

--------------------- STAGE 0 — WHAT U-EXPAND ACTUALLY BUYS -------------------
-- THIS SECTION IS GONE (Stage 1b, plans/drop-expand.md). It held a second clone
-- of the driver — `nxTyF` / `nxSpineMF` and the `NXStats` sweep around them —
-- with the four expansion arms stubbed to `.stuck`, run side by side with the
-- real one to price the arm BEFORE deleting it.
--
-- It did its job and then collapsed into its subject. Stage 1a made the four
-- arms `.stuck` in the driver itself, and the two agreed on 771 578 / 771 578
-- pairs in all three universes at cap 64 — which is what licensed the deletion.
-- Stage 1b then removed `Θ`, and the clone became CHARACTER-FOR-CHARACTER the
-- real driver: its comparison could only ever report 100%, by construction, and
-- a reader could mistake that for evidence.
--
-- The numbers it produced are in typesystems/proof-state.md (2026-09-22). The
-- tripwire it used to be became `crossfield_stuck` (now `crossfield_success`,
-- U-host) and `Regressions.unify_crossfield_mirror_stuck`, which still fires the
-- moment an arm that invents variables at the trailing end comes back.

------------------ U-HOST — CAN A SOLE-OCCURRENCE EXPANSION COME BACK? ----------
-- A clone of the driver with ONE new arm, last in dispatch order: the old
-- U-expand detector (`HostShape`) plus SOLE OCCURRENCE — the host β occurs
-- nowhere in the problem except at its spine position. Under that guard,
-- renaming β ↦ β′ on the host side IS applying β ≔ (l:τ | β′) to the whole
-- problem, so nothing stale survives and the solution should stay `Applied`.
-- No δ, no Θ: the arm emits the binding with τ itself.
--
-- Checked against the real driver, per pair:
--  * `recov` — stuck ⇝ success: what the arm buys. Stage 0 priced the old arms
--    at 1036 / 38 808 / 348 lost successes in wide / deep / nest.
--  * `nAnom` — a NON-stuck driver verdict that moved. MUST be 0: the arm is
--    last, so the clone is the real run with some `.stuck` leaves replaced.
--    A replaced leaf may continue to clash/occurs (`sharpen`), never to
--    outOfFuel (`nFuel`).
--  * `wr` adds the right-end mirror, by reversal: β ≔ (β′ | l:τ).
--  * `nAppl` — a clone success whose solution is not `Applied`. Must be 0.
--  * `nSat`  — a clone success whose `toSubst` does not unify the problem,
--    decided by the ≈-characterization (`canonRow`). Must be 0. `rlSat` runs
--    the same check on the real driver as a CONTROL for the checker itself.

/-- β occurs ONLY as the leading spine variable of `s₂`: the `HostShape`
    conditions plus sole occurrence. -/
def soleHost (l : Label) (τ : Ty Unit) (t₁ s₂ : Spine) : Option TyVar :=
  match sVarSeq s₂ with
  | β :: rest =>
      let τv := Ty.allRowVars τ
      let payloadVars := s₂.flatMap fun
        | .field _ σ => Ty.allRowVars σ
        | .dfield _ σ => Ty.allRowVars σ
        | .var _ => []
      if sFieldCount l s₂ == 0 && !sHasKey s₂ && !τv.contains β &&
         rest.all τv.contains &&
         !(Row.allRowVars (ofSpine t₁)).contains β &&
         !payloadVars.contains β && (sVarSeq s₂).count β == 1
      then some β else none
  | [] => none

def hostBind (β β' : TyVar) (l : Label) (τ : Ty Unit) : UResM Unit → UResM Unit
  | .success s S' => .success (s.comp ⟨[], [(β, .cat (.sing l τ) (.var β'))], []⟩) S'
  | r => r

def hostBindR (β β' : TyVar) (l : Label) (τ : Ty Unit) : UResM Unit → UResM Unit
  | .success s S' => .success (s.comp ⟨[], [(β, .cat (.var β') (.sing l τ))], []⟩) S'
  | r => r

instance : Inhabited (UResM Unit) := ⟨.stuck⟩

mutual

partial def hxTyF (wr : Bool) (S : Supply) (fuel : Nat) : Ty Unit → Ty Unit → UResM Unit
  | .var α, τ₂ => bindTy S α τ₂
  | τ₁, .var α => bindTy S α τ₁
  | .unk, .unk => .success .nil S
  | .base b, .base b' => if b = b' then .success .nil S else .clash
  | .lab k₁, .lab k₂ => unifyKey S k₁ k₂
  | .fn a₁ b₁, .fn a₂ b₂ =>
      match fuel with
      | 0 => .outOfFuel
      | f+1 =>
          (hxTyF wr S f a₁ a₂).seq fun θ S' =>
            hxTyF wr S' f (b₁.applySubst θ) (b₂.applySubst θ)
  | .rcd ρ₁, .rcd ρ₂ =>
      match fuel with
      | 0 => .outOfFuel
      | f+1 => hxSpineMF wr S f ρ₁.toSpine ρ₂.toSpine
  | _, _ => .clash

partial def hxSpineMF (wr : Bool) : Supply → Nat → Spine → Spine → UResM Unit
  | S, _, [], s₂ =>
      match allVarsEmpty s₂ with
      | some σ => .success (Sol.ofRow σ) S
      | none   => .clash
  | S, _, s₁, [] =>
      match allVarsEmpty s₁ with
      | some σ => .success (Sol.ofRow σ) S
      | none   => .clash
  | _, 0, _, _ => .outOfFuel
  | S, fuel+1, s₁, s₂ =>
      match stripL s₁ s₂ with
      | some (t₁, t₂) => hxSpineMF wr S fuel t₁ t₂
      | none =>
      match stripR s₁ s₂ with
      | some (t₁, t₂) => hxSpineMF wr S fuel t₁ t₂
      | none =>
      match solveVarM S s₁ s₂ with
      | some r => r
      | none =>
      match solveVarM S s₂ s₁ with
      | some r => r
      | none =>
      match matchL s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (hxTyF wr S fuel τ τ').seq fun θ S' =>
            hxSpineMF wr S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchL s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (hxTyF wr S fuel τ τ').seq fun θ S' =>
            hxSpineMF wr S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (hxTyF wr S fuel τ τ').seq fun θ S' =>
            hxSpineMF wr S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match matchR s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (hxTyF wr S fuel τ τ').seq fun θ S' =>
            hxSpineMF wr S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₁ s₂ with
      | some (τ, τ', t₁, t₂) =>
          (hxTyF wr S fuel τ τ').seq fun θ S' =>
            hxSpineMF wr S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      match groundMatch s₂ s₁ with
      | some (τ', τ, t₂, t₁) =>
          (hxTyF wr S fuel τ τ').seq fun θ S' =>
            hxSpineMF wr S' fuel (sApplySubst θ t₁) (sApplySubst θ t₂)
      | none =>
      if projClash s₁ s₂ then .clash else
      -- U-HOST, leading end, both orientations
      match s₁ with
      | .field l τ :: t₁ =>
          match soleHost l τ t₁ s₂ with
          | some β =>
              let (β', S₁) := S.fresh
              hostBind β β' l τ (hxSpineMF wr S₁ fuel t₁ (renameVar β β' s₂))
          | none => hostR wr S fuel s₁ s₂
      | _ => hostR wr S fuel s₁ s₂

partial def hostR (wr : Bool) : Supply → Nat → Spine → Spine → UResM Unit
  | S, fuel, s₁, s₂ =>
      match s₂ with
      | .field l τ :: t₂ =>
          match soleHost l τ t₂ s₁ with
          | some β =>
              let (β', S₁) := S.fresh
              hostBind β β' l τ (hxSpineMF wr S₁ fuel (renameVar β β' s₁) t₂)
          | none => hostEnd wr S fuel s₁ s₂
      | _ => hostEnd wr S fuel s₁ s₂

-- the RIGHT end, by reversal: trailing field against trailing host,
-- emitting β ≔ (β′ | l:τ). Only when `wr`.
partial def hostEnd (wr : Bool) : Supply → Nat → Spine → Spine → UResM Unit
  | S, fuel, s₁, s₂ =>
      if !wr then .stuck else
      match s₁.reverse with
      | .field l τ :: t₁r =>
          match soleHost l τ t₁r s₂.reverse with
          | some β =>
              let (β', S₁) := S.fresh
              hostBindR β β' l τ (hxSpineMF wr S₁ fuel t₁r.reverse (renameVar β β' s₂))
          | none => hostEnd2 S fuel s₁ s₂
      | _ => hostEnd2 S fuel s₁ s₂

partial def hostEnd2 : Supply → Nat → Spine → Spine → UResM Unit
  | S, fuel, s₁, s₂ =>
      match s₂.reverse with
      | .field l τ :: t₂r =>
          match soleHost l τ t₂r s₁.reverse with
          | some β =>
              let (β', S₁) := S.fresh
              hostBindR β β' l τ (hxSpineMF true S₁ fuel (renameVar β β' s₁) t₂r.reverse)
          | none => .stuck
      | _ => .stuck

end

def hxSpineM (fuel : Nat) (s₁ s₂ : Spine) (wr : Bool := false) : UResM Unit :=
  hxSpineMF wr (localSupply s₁ s₂) fuel s₁ s₂

-- ## Deciding ≈ by the characterization (`rowEquiv_iff_char`)
-- A canonical string: barrier sequence, then per label (sorted) the projection
-- with canonical payloads, then the keyed projection. Two rows are ≈ iff their
-- strings agree, since `Row.Char` is exactly componentwise equality up to ≈ on
-- payloads, and ≈ on types is the congruence closure.
mutual
partial def canonTy : Ty Unit → String
  | .var α  => "t" ++ α
  | .base _ => "B"
  | .lab k  => "L" ++ keyStr k
  | .unk    => "★"
  | .fn a b => "(" ++ canonTy a ++ "→" ++ canonTy b ++ ")"
  | .rcd ρ  => "{" ++ canonRow ρ ++ "}"

partial def canonRow (ρ : Row Unit) : String :=
  let s := ρ.toSpine
  let bars := (sBarSeq s).map fun
    | .var α => "v" ++ α
    | .key o => "k" ++ o
  let labels := (s.filterMap fun
    | .field l _ => some l
    | _ => none).eraseDups.mergeSort (fun a b => decide (a < b))
  let projs := labels.map fun l =>
    l ++ ":" ++ toString ((sProj l s).map fun p => (p.1, canonTy p.2))
  let dp := toString ((sDProj s).map fun p => (p.1, canonTy p.2))
  toString bars ++ "|" ++ toString projs ++ "|" ++ dp
end

def satB (sol : Sol Unit) (s₁ s₂ : Spine) : Bool :=
  let θ := sol.toSubst
  canonRow ((ofSpine s₁).applySubst θ) == canonRow ((ofSpine s₂).applySubst θ)

structure HXStats where
  total   : Nat := 0
  rlSucc  : Nat := 0
  hxSucc  : Nat := 0
  recov   : Nat := 0
  bothStuck : Nat := 0
  nAnom   : Nat := 0
  sharpen : Nat := 0
  nFuel   : Nat := 0
  nAppl   : Nat := 0
  nSat    : Nat := 0
  rlSat   : Nat := 0
  wRecov  : List (Spine × Spine × String) := []
  wStuck  : List (Spine × Spine) := []
  wAnom   : List (Spine × Spine × String × String) := []
  wBad    : List (Spine × Spine × String) := []

def hxStep (wr : Bool) (cap : Nat) (s₁ s₂ : Spine) (st : HXStats) : HXStats :=
  let rl := unifySpineM cap s₁ s₂
  let hx := hxSpineM cap s₁ s₂ wr
  let st := { st with total := st.total + 1 }
  let st := match rl with
    | .success sol _ =>
        { st with rlSucc := st.rlSucc + 1
                  rlSat  := st.rlSat + (if satB sol s₁ s₂ then 0 else 1) }
    | _ => st
  let st := match hx with
    | .success sol _ =>
        let bad := !solAppliedB sol || !satB sol s₁ s₂
        { st with hxSucc := st.hxSucc + 1
                  nAppl  := st.nAppl + (if solAppliedB sol then 0 else 1)
                  nSat   := st.nSat + (if satB sol s₁ s₂ then 0 else 1)
                  wBad   := if bad && st.wBad.length < keep
                            then (s₁, s₂, solStr sol) :: st.wBad else st.wBad }
    | _ => st
  if resStr rl == resStr hx then
    match rl with
    | .stuck => { st with bothStuck := st.bothStuck + 1
                          wStuck := if st.wStuck.length < keep
                                    then (s₁, s₂) :: st.wStuck else st.wStuck }
    | _ => st
  else match rl, hx with
    | .stuck, .success sol _ =>
        { st with recov  := st.recov + 1
                  wRecov := if st.wRecov.length < keep
                            then (s₁, s₂, solStr sol) :: st.wRecov else st.wRecov }
    | .stuck, .clash | .stuck, .occurs => { st with sharpen := st.sharpen + 1 }
    | .stuck, .outOfFuel => { st with nFuel := st.nFuel + 1 }
    | _, _ =>
        { st with nAnom := st.nAnom + 1
                  wAnom := if st.wAnom.length < keep
                           then (s₁, s₂, resStr rl, resStr hx) :: st.wAnom else st.wAnom }

def hxReport (U : Universe) (cap : Nat) (wr : Bool := false) : IO Unit := do
  let ss := allSpines U.atoms U.maxLen
  let st := ss.foldl (fun st s₁ => ss.foldl (fun st s₂ => hxStep wr cap s₁ s₂ st) st) {}
  IO.println s!"── U-host{if wr then " (both ends)" else ""} vs driver: universe {U.name}, {st.total} pairs, cap {cap}"
  IO.println s!"   driver successes {st.rlSucc}, U-host successes {st.hxSucc}"
  IO.println s!"   RECOVERED (stuck ⇝ success)  {st.recov}"
  IO.println s!"   still stuck under both        {st.bothStuck}"
  IO.println s!"   stuck ⇝ clash/occurs          {st.sharpen}"
  IO.println s!"   stuck ⇝ outOfFuel (must be 0) {st.nFuel}"
  IO.println s!"   ANOMALIES: a non-stuck verdict moved (must be 0) {st.nAnom}"
  IO.println s!"   ¬Applied successes (must be 0) {st.nAppl}"
  IO.println s!"   unsound successes (must be 0)  {st.nSat}   [checker control on driver: {st.rlSat}]"
  for (a, b, s) in st.wRecov.reverse do
    IO.println s!"     recov  {pairStr a b}\n            {s}"
  for (a, b) in st.wStuck.reverse do
    IO.println s!"     stuck  {pairStr a b}"
  for (a, b, r, h) in st.wAnom.reverse do
    IO.println s!"     ANOM   {pairStr a b}\n            driver {r}\n            U-host {h}"
  for (a, b, s) in st.wBad.reverse do
    IO.println s!"     BAD    {pairStr a b}\n            {s}"
  IO.println ""

def hxFamilies (cap n : Nat) : IO Unit := do
  IO.println s!"── U-host on the parametric families (verdict at cap {cap})"
  for F in families do
    let mut out := ""
    for i in List.range (n + 1) do
      let (s₁, s₂) := F.gen i
      out := out ++ s!" {i}:{(verdictStr (hxSpineM cap s₁ s₂)).take 2}"
    IO.println s!"   {F.name}: {out}"
  IO.println ""

def hostMain : IO Unit := do
  let cap := 64
  IO.println "U-HOST — sole-occurrence expansion, priced against the driver\n"
  for (nm, s₁, s₂) in landmarks do
    IO.println s!"   {verdictStr (unifySpineM cap s₁ s₂)} ⇝ {resStr (hxSpineM cap s₁ s₂)}  {nm}"
  IO.println ""
  hxReport wide cap
  hxReport nest cap
  hxReport keyed cap
  hxReport deep cap
  hxFamilies 200 12
  IO.println "── AGAIN WITH THE RIGHT-END MIRROR\n"
  for (nm, s₁, s₂) in landmarks do
    IO.println s!"   {verdictStr (unifySpineM cap s₁ s₂)} ⇝ {resStr (hxSpineM cap s₁ s₂ true)}  {nm}"
  hxReport wide cap true
  hxReport nest cap true
  hxReport deep cap true

def main : IO Unit := do
  let cap := 64
  IO.println "≐ᵣ / ≐ driver — invariant refuter: [1]-[7] per universe, then STAGE 0\n"
  landmarkReport cap
  report wide cap
  report deep cap
  report nest cap
  report keyed cap
  familyReport 4000 24
  IO.println "A pair still outOfFuel at cap, with a spine of ≤ 6 atoms, is a"
  IO.println "divergence candidate: minimize it by hand and it becomes a theorem."

end Fuzz
end MinimalCalculus

def main (args : List String) : IO Unit :=
  if args == ["host"] then MinimalCalculus.Fuzz.hostMain else MinimalCalculus.Fuzz.main
