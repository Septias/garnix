#import "../functions.typ": *
#import "04_unification.typ": u_clash, u_occurs, u_stuck

#let st_park = $⊎$
#let st_drop = $∖$
#let st_solve = $scripts(⇝)^!$
#let st_wake = $↝$
#let st_sat = $scripts(↝)^!$
#let st_merge = $scripts(⇝)_m$
#let st_merges = $scripts(⇝)_m^*$

= Inference <inference>

The inference algorithm is a judgement $Γ; S ⊢ e ⇒ τ; S′$ that takes a _solver state_ S and returns the updated state S', which the premise receives in turn. Our biggest deviation from algorithm W is the usage of stumps: Instead of guessing the value of an unknown lookup, we park it in a _stump_. A stump is syntactically similar to the lookup relation ⟨ρ.l ↓ r⟩ itself and stored in the state until a substitution lets lookup advance further in the row, possibly solving it. At the end of inference, every pending stump stays a qualifier of the answer, and a use that leaves its lookup undecided reads it as ★. This way, unification is only ever called on equations between types and never sees a field demand it has to solve. // giving it the properties we want…


// TODO(structure): outcome-first opening above (F): the pipeline (infer, solve, saturate, wake; then merge and generalize at the entry), and that every instance of the answer is a declarative typing.
// TODO(structure): heading "== Solver State" here (J). Define all vocabulary in it: stump, parked, blocker, stale, quiescent, saturation, spent.
#let solver_state = figure(
  caption: "Solver state and the judgements of inference.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "State")[
      $
         #type_name("Solver state") S & ::= (θ, Δ, macron(κ)) \
        #type_name("Parked stumps") Δ & ::= ∅ | ⟨α ⊳ ρ.l ↓ δ⟩, Δ \
         #type_name("Blocked lookup") & (⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α \
      $
    ],
    subbox(caption: "Operations")[
      #flexbox(
        $#type_name("Park a stump") S #st_park q$,
        $#type_name("Drop stumps") S #st_drop Δ′$,
      )
    ],
    subbox(caption: "Judgements")[
      #flexbox(
        $#type_name("Inference") Γ; S ⊢ e ⇒ τ; S′$,
        $#type_name("Solve") S ⊢ τ ≐ τ′ ⇝ S′$,
        $#type_name("Solve, then saturate") S ⊢ τ ≐ τ′ #st_solve S′$,
        $#type_name("Wake one stump") S ⊢ q #st_wake S′$,
        $#type_name("Saturate") S ⊢ Δ #st_sat S′$,
        $#type_name("Merge two stumps") S ⊢ Δ #st_merge S′$,
      )
    ],
  )),
)
#solver_state <solver-state>

The solver state @solver-state consists of three parts: A sort-respecting substitution θ, a list Δ of parked stumps and a record $macron(κ)$ of the sorts at which each name was drawn. ⟦S⟧τ applies the substitution of S to τ, $S(α)$ reads the sort of α from $macron(κ)$, and $Δ_i$ denotes the parked stumps of $S_i$. A parked stump $⟨α ⊳ ρ.l ↓ δ⟩$ is a lookup of l in ρ whose result is represented by the fresh variable δ, annotated with a row-variable α that blocks it. A parked stump is _spent_ if its result has already been used at a type that is neither a variable nor ★, as in @ex-spent. When a new row variable is written, the algorithm checks every stump that is blocked on it and tries to solve it again, writing the result into δ. The implementation additionally reports a warning for every ★ that inference commits.

The state is kept under one invariant, _quiescence_. Quiescence demands that every stump in Δ is genuinely blocked on its recorded variable α. Every written solution can break this condition so a new judgement is needed that rolls all stumps to completion or blockage after every move. The judgement $S ⊢ τ ≐ τ' ⇝ S'$ runs the unifier of @unification and writes its solution into the state. Only a successful unification has a rule: a #u_clash, #u_occurs or #u_stuck verdict has no derivation, so inference rejects the program. The judgement $S ⊢ τ ≐ τ′ #st_solve S′$ does so and _saturates_ afterwards. Saturation is the process of re-examining all unblocked stumps until the state is quiescent again.

== Inference Rules

#let inference = figure(
  caption: "Algorithmic typing rules.",
  flexbox(
    derive("A-cons", (), $Γ; S ⊢ c ⇒ 𝓫_c; S$),
    derive(
      "A-var",
      (
        $x: ∀(macron(α): macron(κ)). Q ⇒ τ ∈ Γ$,
        $"fresh" macron(β): macron(κ)$,
        $S ⊢ Q[macron(β)\/macron(α)] scripts(↝)^(*!) S′$,
      ),
      $Γ; S ⊢ x ⇒ τ[macron(β)\/macron(α)]; S′$,
    ),
    derive(
      "A-λ-I",
      ($"fresh" α: "Type"$, $Γ · (x: α); S ⊢ e ⇒ τ; S′$),
      $Γ; S ⊢ (x: e) ⇒ α -> τ; S′$,
    ),
    derive(
      "A-λ-E",
      (
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁$,
        $Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂$,
        $"fresh" β: "Type"$,
        $S₂ ⊢ τ₁ ≐ (τ₂ -> β) #st_solve S₃$,
      ),
      $Γ; S ⊢ e₁e₂ ⇒ β; S₃$,
    ),
    derive(
      "A-conc",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) Γ; S₁ ⊢ e₂ ⇒ τ₂; S₂ #h(2em) "fresh" ρ₁, ρ₂: "Row"$,
        $S₂ ⊢ τ₁ ≐ {ρ₁} #st_solve S₃ #h(2em) S₃ ⊢ τ₂ ≐ {ρ₂} #st_solve S₄$,
      ),
      $Γ; S ⊢ e₁ ‖ e₂ ⇒ {ρ₂ | ρ₁}; S₄$,
    ),
    derive(
      "A-sel",
      (
        $Γ; S ⊢ e ⇒ τ; S₁$,
        $"fresh" ρ: "Row"$,
        $S₁ ⊢ τ ≐ {ρ} #st_solve S₂$,
        $(⟦S₂⟧ρ).l ↓ τ′$,
      ),
      $Γ; S ⊢ e.l ⇒ τ′; S₂$,
    ),
    derive(
      "A-sel-⊥",
      (
        $Γ; S ⊢ e ⇒ τ; S₁$,
        $"fresh" ρ: "Row"$,
        $S₁ ⊢ τ ≐ {ρ} #st_solve S₂$,
        $(⟦S₂⟧ρ).l ↓ ⊥$,
      ),
      $Γ; S ⊢ e.l ⇒ ★; S₂$,
    ),
    derive(
      "A-sel-?",
      stack(
        spacing: 8pt,
        $Γ; S ⊢ e ⇒ τ; S₁ #h(2em) "fresh" ρ: "Row" #h(2em) S₁ ⊢ τ ≐ {ρ} #st_solve S₂$,
        $(⟦S₂⟧ρ).l ↓ #h(0.2em) ? "on" α #h(2em) "fresh" δ: "Type"$,
      ),
      $Γ; S ⊢ e.l ⇒ δ; S₂ #st_park ⟨α ⊳ ρ.l ↓ δ⟩$,
    ),
    derive("A-rec", ($Γ; S ⊢ ξ ⇒ ρ; S′$,), $Γ; S ⊢ {ξ} ⇒ {ρ}; S′$),
    derive("A-ξ-empty", (), $Γ; S ⊢ ε ⇒ ε; S$),
    derive("A-ξ-field", ($Γ; S ⊢ e ⇒ τ; S′$,), $Γ; S ⊢ (l = e) ⇒ (l: τ); S′$),
    derive(
      "A-ξ-conc",
      ($Γ; S ⊢ ξ₁ ⇒ ρ₁; S₁$, $Γ; S₁ ⊢ ξ₂ ⇒ ρ₂; S₂$),
      $Γ; S ⊢ (ξ₁ | ξ₂) ⇒ (ρ₁ | ρ₂); S₂$,
    ),
  ),
)
#inference <inference-rules>

#let generalization = figure(
  caption: "Generalization.",
  stack(
    spacing: 14pt,
    flexbox(
      derive(
        "A-let",
        stack(
          spacing: 8pt,
          $Γ; S ⊢ e₁ ⇒ τ₁; S₀ #h(2em) S₀ ⊢ Δ₀ #st_merges S₁ #h(2em) macron(α) = "gen"_(Γ, S)(S₁, τ₁)$,
          $Γ · (x: ∀(macron(α): S₁(macron(α))). ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ #st_drop Δ_q ⊢ e₂ ⇒ τ₂; S₂$,
        ),
        $Γ; S ⊢ #b[let] x = e₁ #b[in] e₂ ⇒ τ₂; S₂$,
      ),
      derive(
        "M-merge",
        (
          $⟨α ⊳ ρ.l ↓ δ⟩, ⟨α′ ⊳ ρ′.l ↓ δ′⟩ ∈ Δ$,
          $⌈⟦S⟧ρ⌉ = ⌈⟦S⟧ρ′⌉$,
          $S ⊢ δ ≐ δ′ #st_solve S′$,
        ),
        $S ⊢ Δ #st_merge S′$,
      ),
    ),
    align(
      left,
      $
        Δ_q &= {p ∈ Δ₁ | "blocker"(p) ∈ macron(α)} #h(3em) Δ_Γ = Δ₁ #st_drop Δ_q \
        "gen"_(Γ, S)(S₁, τ₁) &= "the greatest" macron(α) ⊆ "ftv"(⟦S₁⟧τ₁, ⟦S₁⟧Δ₁) "such that" \
        & #h(1em) macron(α) ∩ "ftv"(⟦S₁⟧(Γ, Δ_Γ)) = ∅ \
        & #h(1em) Δ_q ∩ Δ = ∅ \
        & #h(1em) "ftv"(⟦S₁⟧"results"(Δ_q)) ⊆ macron(α) \
        & #h(1em) Δ_q "fillable at" S₁
      $,
    ),
  ),
)
#generalization <generalization>

@inference-rules gives the syntax-directed counterpart of the declarative rules of @declarative-rules. T-eq and T-★-intro have no algorithmic counterpart. T-eq is part of row unification, and T-★-intro would only discard a type, which the algorithm never does. The programs that only T-★-intro makes typable are a source of incompleteness (@incompleteness-inference). A-cons, A-λ-I and the record-literal rules are the familiar ones. A-λ-E and A-conc introduce fresh variables for the shape their premises demand and give the resulting equations to $#st_solve$. A-conc glues the two row-variables together, just like the declarative system.

Selection is where the algorithm departs from W. The first premise of T-sel infers a type for the expression $e$ that is then unified with a record ${ρ}$ with a fresh row. The lookup relation of @row-lookup is then consulted about the returned types. The algorithmic rules handle the three verdicts of the lookup relation by three separate rules, A-sel, A-sel-⊥, and A-sel-\?, similar to the declarative rules (@declarative-rules). A-sel-\? differs from the declarative rule T-sel-★ in the usage of stumps. It does not commit the ★ type, but records the stump in the solver state; a pending lookup becomes ★ only at a use that leaves it undecided, by D-\? (@instantiation). The fresh type variable δ, returned by the statement is a placeholder for the lookup result. No equation mentioning l is ever emitted, and this is the entire reason row unification never has to guess a field into a variable.

A-var instantiates a qualified scheme with fresh variables of the binder's sort. The scheme's stumps are instantiated as a premise and immediately replayed by $scripts(↝)^(*!)$ and the stumps that instantiation already decides are answered on the spot. The others are parked on the variables that block them. Each use of a let-bound variable discharges its own copy of stumps.

A-let is given in a separate figure: @generalization shows let-generalization. The choice of which variables are generalized is delegated to a function $"gen"_(Γ, S)$. The rule itself only infers $e₁$, merges its stumps, binds the scheme and continues with $e₂$. The stumps are partitioned into two groups: One group goes into the scheme and one into the solver state. $Δ_q$ is the list of stumps that are blocked on the let's variables $macron(α)$ and go into the scheme as its qualifiers Q. $Δ_Γ = Δ₁ #st_drop Δ_q$ are the remaining stumps that go into solver state.

M-merge unifies the results of two parked stumps that ask the same lookup: their rows have one spine under the current solution and their labels agree. The move is forced. At every instance that discharges both stumps, the two lookups are one lookup and return one verdict, so the instance identifies the two results up to ≈ (`merge_forced` in Lean). The merge lets `x: {p = (x.l) c; q = x.l}` pass the fillability test below, which asks a field that is selected twice to be spent alike. The algorithm tries every pair once and skips a pair whose equation fails.

Four conditions make a choice admissible. The generalized variables must be fresh for the environment, which here consists of Γ together with the stumps that stay parked; this is the side condition of Hindley-Milner generalization @damas_milner, and it ensures that a parked stump reads the same at every instance. $Δ_q$ may contain only stumps parked while inferring $e₁$, since a stump of an outer binding that is captured by the scheme would never be discharged. This condition does not follow from the others: in `{a = x: x.l; b = let y = c in c}` the stump of field a is blocked on a variable that neither Γ nor any remaining stump mentions, so the remaining three conditions admit its generalization into the scheme of y.

Every variable in a generalized stump's result must itself be generalized, since otherwise each instance would pin a variable of the environment. Finally, the scheme must be inhabited, as the declarative T-let requires through $∃ τ₁. σ ≥ τ₁$. A stump whose result is still a variable is harmless, since its result can be sent to ★ while its lookup stays blocked. A stump whose result has been spent on a non-variable type must instead hit at some instance. The algorithm does not decide this property but checks a sufficient syntactic condition, written $Δ_q$ fillable at $S₁$: every spent stump and every stump on its blocker has a literal label or a key variable ℓ of $macron(α)$ other than the blocker, which the instance reads as the label `${ℓ}` (@sec-fc-labels); a stump on the blocker with the same field name is spent as well and its result agrees with the first once every type variable is replaced by ★; no stump of $Δ_q$ waits on a key variable of a filled blocker; and no spent blocker occurs in a spent result. Under this condition the instance that sends the generalized type variables to ★ and extends each blocker by the fields its spent stumps demand discharges every stump of $Δ_q$. The admissible choices are closed under union, so a greatest one exists (`greatestAlpha_spec` in Lean) and is computed by deleting variables until nothing more is deleted. A failing condition therefore does not make the binding monomorphic; it removes the offending variables and whatever they drag along.

As an example, consider `let f = x: x.l in f {l = c}`. For the bound term, A-λ-I gives x a fresh α, and A-sel-\? unifies α with ${ρ}$ for a fresh ρ, finds that $ρ.l$ answers $?$ on ρ, and parks $⟨ρ ⊳ ρ.l ↓ δ⟩$ with a fresh δ. The result is $⟦S₁⟧(α → δ) = {ρ} → δ$. Since α is solved, $macron(α) = ρ, δ$; the single stump has its result in $macron(α)$ and moves into the scheme, and nothing stays parked. f is bound to $∀(ρ: "Row", δ: "Type"). ⟨ρ.l ↓ δ⟩ ⇒ {ρ} → δ$, which is selQ of @principality. At the use, A-var instantiates the scheme with fresh ρ′ and δ′ and replays the stump, which is still blocked and is parked on ρ′. A-λ-E then solves $ρ′ ≔ (l: 𝓫_c)$ and identifies δ′ with the result variable. Saturation finds the stump stale, K-hit re-asks the lookup, which now hits, and the result is unified with $𝓫_c$.

== Wake-Up and the Entry Point

#let wakeup = figure(
  caption: "Waking and saturating stumps.",
  box(width: 100%, stack(
    spacing: 18pt,
    subbox(caption: "Wake One Stump")[
      #flexbox(
        derive(
          "K-hit",
          ($(⟦S⟧ρ).l ↓ τ′$, $S ⊢ δ ≐ τ′ ⇝ S′$),
          $S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_wake S′ #st_drop ⟨α ⊳ ρ.l ↓ δ⟩$,
        ),
        derive(
          "K-⊥",
          ($(⟦S⟧ρ).l ↓ ⊥$, $S ⊢ δ ≐ ★ ⇝ S′$),
          $S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_wake S′ #st_drop ⟨α ⊳ ρ.l ↓ δ⟩$,
        ),
        derive(
          "K-repark",
          ($(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α′$,),
          $S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_wake (S #st_drop ⟨α ⊳ ρ.l ↓ δ⟩) #st_park ⟨α′ ⊳ ρ.l ↓ δ⟩$,
        ),
      )
    ],
    subbox(caption: "Replay a List of Stumps")[
      #flexbox(
        derive("K-nil", (), $S ⊢ ∅ scripts(↝)^* S$),
        derive(
          "K-cons",
          ($S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_wake S₁$, $S₁ ⊢ Δ′ scripts(↝)^* S′$),
          $S ⊢ (⟨α ⊳ ρ.l ↓ δ⟩, Δ′) scripts(↝)^* S′$,
        ),
        derive(
          "K-park",
          (
            $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
            $(S #st_park ⟨α ⊳ ρ.l ↓ δ⟩) ⊢ Δ′ scripts(↝)^* S′$,
          ),
          $S ⊢ (⟨α ⊳ ρ.l ↓ δ⟩, Δ′) scripts(↝)^* S′$,
        ),
      )
    ],
    subbox(caption: "Saturate")[
      #flexbox(
        derive("K!-done", ($S "quiescent"$,), $S ⊢ Δ #st_sat S$),
        derive(
          "K!-step",
          (
            $⟨α ⊳ ρ.l ↓ δ⟩ ∈ Δ$,
            $¬(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
            $S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_wake S₁$,
            $S₁ ⊢ Δ₁ #st_sat S′$,
          ),
          $S ⊢ Δ #st_sat S′$,
        ),
      )
    ],
  )),
)
#wakeup <wakeup>

#let entry = figure(
  caption: "The entry point.",
  stack(
    spacing: 14pt,
    flexbox(
      derive(
        "Entry",
        (
          $∅; (id, ∅, ∅) ⊢ e ⇒ τ; S₁$,
          $S₁ ⊢ Δ₁ #st_merges S₂$,
          $σ "inhabited at" S₂$,
        ),
        $⊢ e ⇒ σ$,
      ),
    ),
    align(
      left,
      $σ = ∀(macron(α): S₂(macron(α))). ⟦S₂⟧Δ₂ ⇒ ⟦S₂⟧τ #h(3em) macron(α) = "ftv"(⟦S₂⟧τ, ⟦S₂⟧Δ₂)$,
    ),
  ),
)
#entry <entry>

@wakeup gives the judgements that wake stumps. Waking a stump retries the lookup against the current state and has the verdicts a lookup can produce. K-hit is the case when the lookup hits a definite type so the stump can be dropped and the type of δ is set by unifying it against the found type. K-⊥ pins it to ★ and K-repark moves the stump to the new blocker α'. This is the case when substitution lets the search advance but not finish. The stability lemma of @lookup-metatheory makes this sound because a definite lookup never changes, even after further substitutions.

$scripts(↝)^*$ replays a whole list of stumps. K-park parks a stump that is still blocked. Saturation $#st_sat$ steps only on _stale_ stumps whose recorded blocker no longer blocks. It stops as soon as the state is quiescent. A-var uses $scripts(↝)^(*!)$, the replay $scripts(↝)^*$ followed by saturation $#st_sat$, since a replayed stump whose result is already solved can write a row solution and make a stump stale that the replay has already parked.

// PROF: Fixed vocabulary: stump/pending lookup/promise/field demand/qualifier. "Promise" is never defined.
The entry point @entry runs inference from the empty state, merges the stumps that ask the same lookup by M-merge, and generalizes like A-let with Γ = ∅. Every free variable of the type and of the parked stumps is generalized, no stump stays outside the scheme, and the answer is the scheme σ. Nothing is decided at the entry point: a stump that is still pending is a qualifier of σ, and an instance reads it as ★ by D-\? when its lookup stays undecided. In the algorithm itself, ★ arises only where a lookup misses (A-sel-⊥, K-⊥) or a key is a string (@sec-fc-labels).

Of the conditions of A-let, only inhabitation is left: Γ is empty, and the start state parks no stump of an outer binding. The premise σ inhabited at $S₂$ is the fillability test of A-let, run on a scratch copy of $S₂$ in which a bounded search may first solve further equations. For a spent stump whose key is a variable, the search tries the label `${ℓ}` and then the literal labels of the stump's row. It also unifies the results of two stumps on one blocker with one label whose rows differ before the blocker. Without first-class labels, the search only merges. The answer σ is read at $S₂$, before the search, so the search never changes it, and any outcome is sound: an instance for an extension of $S₂$ is an instance of σ (`inhabited_of_ext` in Lean). The search is greedy, and if the test still fails, inference rejects the program. A-let runs the same test without the search, so `x: {p = (({m = c} ‖ x).l) c; q = x.l}` is accepted at the entry point but stays monomorphic under a binding (@incompleteness-inference).

#example(name: [Waking and re-parking])[
  In `r: (x: x.l) (r ‖ {m = c})`, A-sel-\? parks the stump $⟨ρ ⊳ ρ.l ↓ δ⟩$, where ρ is the fresh row of x. The argument has type ${m: 𝓫_c | ρ_r}$ by A-conc, where $ρ_r$ is the row of r, and A-λ-E solves $ρ ≔ (m: 𝓫_c | ρ_r)$. This write makes the stump stale, since its recorded blocker ρ is solved. Saturation re-asks the lookup, which skips m by L-miss and stops at $ρ_r$, and K-repark files the stump under $ρ_r$. Nothing solves $ρ_r$ afterwards, so the stump stays a qualifier, and the program answers $∀(ρ_r: "Row", δ: "Type"). ⟨(m: 𝓫_c | ρ_r).l ↓ δ⟩ ⇒ {ρ_r} → δ$. The instance at which the lookup stays undecided reads δ as ★ by D-\? and is the typing ${ρ_r} → ★$.
] <ex-repark>

#example(name: [Why quiescence matters])[
  Wrap the function of @ex-repark in a binding, `let g = (r: (x: x.l) (r ‖ {m = c})) in {a = g {l = c}; b = g {}}`. After A-λ-E the stump is stale, and K-repark files it under $ρ_r$. The bound term has type ${ρ_r} → δ$, its stump is blocked on $ρ_r ∈ macron(α)$, and A-let generalizes it to $∀(ρ_r: "Row", δ: "Type"). ⟨(m: 𝓫_c | ρ_r).l ↓ δ⟩ ⇒ {ρ_r} → δ$. The use at ${l = c}$ answers $𝓫_c$, the use at ${}$ answers ★. Had the stump stayed filed under the solved ρ, it would not be blocked on a variable of $macron(α)$, so it would stay in $Δ_Γ$. Since $⟦S₁⟧Δ_Γ$ mentions $ρ_r$ and δ, neither could be generalized, g would be monomorphic, and its two uses would clash on $ρ_r$. Quiescence guarantees that the blocker A-let reads is the variable the lookup actually waits on.
] <ex-quiescence>

#example(name: [A spent promise])[
  In `x: (x.l) c`, A-sel-\? parks $⟨ρ ⊳ ρ.l ↓ δ⟩$, and A-λ-E unifies δ with $𝓫_c → γ$ for a fresh γ. The promise is spent: D-\? and D-⊥ would demand ★ for a result that is already $𝓫_c → γ$, so an instance must make the lookup hit. The program answers $∀(ρ: "Row", γ: "Type"). ⟨ρ.l ↓ 𝓫_c → γ⟩ ⇒ {ρ} → γ$. The instance that extends ρ by the field $l: 𝓫_c → γ$ discharges the stump by D-hit, so the scheme is inhabited, and this instance is the typing ${l: 𝓫_c → γ | ρ′} → γ$.
] <ex-spent>

== Inference Metatheory <inference-metatheory>

#theorem(name: [Termination of inference], lean: "runF_terminates, satStep_wf")[
  Saturation admits no infinite run, and for every term e the entry point $⊢ e ⇒ σ$ returns a verdict.
] <thm-infer-term>

#theorem(name: [Soundness of inference], lean: "runSound, run_typed")[
  If $⊢ e ⇒ σ$, then $∅ ⊢ e : τ$ for every instance τ of σ, and σ has an instance.
] <thm-infer-sound>

Inference is not complete; @incompleteness lists where it gives up.
