#import "../functions.typ": *

#let st_park = $⊎$
#let st_drop = $∖$
#let st_solve = $scripts(⇝)^!$
#let st_wake = $↝$
#let st_sat = $scripts(↝)^!$
#let st_fin = $⇓$
#let st_mat = $⇓_m$

= Inference <inference>

The inference algorithm is a judgement $Γ; S ⊢ e ⇒ τ; S′$ that takes a _solver state_ S and returns the updated state S', which the premise receives in turn. Our biggest deviation from algorithm W is the usage of stumps: Instead of guessing the value of an unknown lookup, we park it in a _stump_. A stump is syntactically similar to the lookup relation ⟨ρ.l ↓ r⟩ itself and stored in the state until a substitution lets lookup advance further in the row, possibly solving it. At the end of inference, every pending stump is typed as ★. This way, unification is only ever called on equations between types and never sees a field demand it has to solve. // giving it the properties we want…


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
        $#type_name("Materialize one stump") S ⊢ q #st_mat S′$,
        $#type_name("Finalize one stump") S ⊢ q #st_fin S′$,
      )
    ],
  )),
)
#solver_state <solver-state>

The solver state @solver-state consists of three parts: A sort-respecting substitution θ, a list Δ of parked stumps and a record $macron(κ)$ of the sorts at which each name was drawn. ⟦S⟧τ applies the substitution of S to τ, $S(α)$ reads the sort of α from $macron(κ)$, and $Δ_i$ denotes the parked stumps of $S_i$. A parked stump $⟨α ⊳ ρ.l ↓ δ⟩$ is a lookup of l in ρ whose result is represented by the fresh variable δ, annotated with a row-variable α that blocks it. When a new row variable is written, the algorithm checks every stump that is blocked on it and tries to solve it again, writing the result into δ. The implementation additionally reports a warning for every ★ that inference commits.

// The unifier draws its fresh names from the state's supply, advanced past the variables of the problem and those θ binds, so a name drawn by U-host is new to both and θ stays applied.
The state is kept under one invariant, _quiescence_. Quiescence demands that every stump in Δ is genuinely blocked on its recorded variable α. Every written solution can break this condition so a new judgement is needed that rolls all stumps to completion or blockage after every move. The judgement $S ⊢ τ ≐ τ' ⇝ S'$ runs the unifier of @unification and writes its solution into the state and the judgement $S ⊢ τ ≐ τ′ #st_solve S′$ does so and _saturates_ afterwards. Saturation is the process of re-examining all unblocked stumps until the state is quiescent again.

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
      "A-sel-★",
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
          $Γ; S ⊢ e₁ ⇒ τ₁; S₁ #h(2em) macron(α) = "gen"_(Γ, S)(S₁, τ₁)$,
          $Γ · (x: ∀(macron(α): S₁(macron(α))). ⟦S₁⟧Δ_q ⇒ ⟦S₁⟧τ₁); S₁ #st_drop Δ_q ⊢ e₂ ⇒ τ₂; S₂$,
        ),
        $Γ; S ⊢ #b[let] x = e₁ #b[in] e₂ ⇒ τ₂; S₂$,
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

Selection is where the algorithm departs from W. The first premise of T-sel infers a type for the expression $e$ that is then unified with a record ${ρ}$ with a fresh row. The lookup relation of @row-lookup is then consulted about the returned types. The algorithmic rules handle the three verdicts of the lookup relation by three separate rules, A-sel, A-sel-⊥, and A-sel-★, similar to the declarative rules @[cite]. A-sel-★ differs from the declarative rule in the usage of stumps. It does not commit the ★ type yet, but records the stump in the solver state. The fresh type variable δ, returned by the statement is a placeholder for the lookup result. No equation mentioning l is ever emitted, and this is the entire reason row unification never has to guess a field into a variable.

A-var instantiates a qualified scheme with fresh variables of the binder's sort. The scheme's stumps are instantiated as a premise and immediately replayed by $scripts(↝)^(*!)$ and the stumps that instantiation already decides are answered on the spot. The others are parked on the variables that block them. Each use of a let-bound variable discharges its own copy of stumps.

A-let is given in a separate figure: @generalization shows let-generalization. The choice of which variables are generalized is delegated to a function $"gen"_(Γ, S)$. The rule itself only infers $e₁$, binds the scheme and continues with $e₂$. The stumps are partitioned into two groups: One group goes into the scheme and one into the solver state. $Δ_q$ is the list of stumps that are blocked on the let's variables $macron(α)$ and go into the scheme as its qualifiers Q. $Δ_Γ = Δ₁ \\ Δ$ are the remaining stumps that go into solver state.

Four conditions make a choice admissible. The generalized variables must be fresh for the environment, which here consists of Γ together with the stumps that stay parked; this is the side condition of Hindley-Milner generalization @damas_milner, and it ensures that a parked stump reads the same at every instance. $Δ_q$ may contain only stumps parked while inferring $e₁$, since a stump of an outer binding that is captured by the scheme would never be finalized. This condition does not follow from the others: in `{a = x: x.l; b = let y = c in c}` the stump of field a is blocked on a variable that neither Γ nor any remaining stump mentions, so the remaining three conditions admit its generalization into the scheme of y.

Every variable in a generalized stump's result must itself be generalized, since otherwise each instance would pin a variable of the environment. Finally, the scheme must be inhabited, as the declarative T-let requires through $∃ τ₁. σ ≥ τ₁$. A stump whose result is still a variable is harmless, since its result can be sent to ★ while its lookup stays blocked. A stump whose result has been spent on a non-variable type must instead hit at some instance. The algorithm does not decide this property but checks a sufficient syntactic condition, written $Δ_q$ fillable at $S₁$: every spent stump has a literal label, all stumps on its blocker have literal labels, a stump that shares its label is [spent](not yet introduced) as well and its result agrees with the first once every type variable is replaced by ★, and no spent blocker occurs in a spent result. Under this condition the instance that sends the generalized type variables to ★ and extends each blocker by the fields its spent stumps demand discharges every stump of $Δ_q$. The admissible choices are closed under union, so a greatest one exists (`greatestAlpha_spec` in Lean) and is computed by deleting variables until nothing more is deleted. A failing condition therefore does not make the binding monomorphic; it removes the offending variables and whatever they drag along.

As an example, consider `let f = x: x.l in f {l = c}`. For the bound term, A-λ-I gives x a fresh α, and A-sel-★ unifies α with ${ρ}$ for a fresh ρ, finds that $ρ.l$ answers $?$ on ρ, and parks $⟨ρ ⊳ ρ.l ↓ δ⟩$ with a fresh δ. The result is $⟦S₁⟧(α → δ) = {ρ} → δ$. Since α is solved, $macron(α) = ρ, δ$; the single stump has its result in $macron(α)$ and moves into the scheme, and nothing stays parked. f is bound to $∀(ρ: "Row", δ: "Type"). ⟨ρ.l ↓ δ⟩ ⇒ {ρ} → δ$, which is selQ of @principality. At the use, A-var instantiates the scheme with fresh ρ′ and δ′ and replays the stump, which is still blocked and is parked on ρ′. A-λ-E then solves $ρ′ ≔ (l: 𝓫_c)$ and identifies δ′ with the result variable. Saturation finds the stump stale, K-hit re-asks the lookup, which now hits, and the result is unified with $𝓫_c$.

== Wake-Up and Finalization

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

#let finalization = figure(
  caption: "Finalization and the entry point.",
  flexbox(
    derive(
      "F-hit",
      (
        $⟨α ⊳ ρ.l ↓ δ⟩ ∈ Δ$,
        $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
        $⟦S⟧δ$ + [ spent],
        $"fresh" α′: "Row"$,
        $S ⊢ {α} ≐ {l: δ | α′} #st_solve S′$,
      ),
      $S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_mat S′$,
    ),
    derive(
      "F-★",
      (
        $⟨α ⊳ ρ.l ↓ δ⟩ ∈ Δ$,
        $(⟦S⟧ρ).l ↓ #h(0.2em) ? "on" α$,
        $S ⊢ δ ≐ ★ ⇝ S′$,
      ),
      $S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_fin S′ #st_drop ⟨α ⊳ ρ.l ↓ δ⟩$,
    ),
    derive("F-nil", (), $S ⊢ ∅ scripts(#st_fin)^* S$),
    derive(
      "F-cons",
      ($S ⊢ ⟨α ⊳ ρ.l ↓ δ⟩ #st_fin S₁$, $S₁ ⊢ Δ′ scripts(#st_fin)^* S′$),
      $S ⊢ (⟨α ⊳ ρ.l ↓ δ⟩, Δ′) scripts(#st_fin)^* S′$,
    ),
    derive(
      "Entry",
      (
        $∅; (id, ∅, ∅) ⊢ e ⇒ τ; S₁$,
        $S₁ ⊢ Δ₁ scripts(#st_mat)^* S₂$,
        $S₂ ⊢ Δ₂ scripts(#st_fin)^* S′$,
      ),
      $⊢ e ⇒ τ; S′$,
    ),
  ),
)
#finalization <finalization>

@wakeup gives the judgements that wake stumps. Waking a stump retries the lookup against the current state and has the verdicts a lookup can produce. K-hit is the case when the lookup hits a definite type so the stump can be dropped and the type of δ is set by unifying it against the found type. K-⊥ pins it to ★ and K-repark moves the stump to the new blocker α'. This is the case when substitution lets the search advance but not finish. The stability lemma of @lookup-metatheory makes this sound because a definite lookup never changes, even after further substitutions.

// meta: explain the phases and why they need to be done this way
$scripts(↝)^*$ replays a whole list of stumps. K-park parks a stump that is still blocked. Saturation $#st_sat$ steps only on _stale_ stumps whose recorded blocker no longer blocks. It stops as soon as the state is quiescent.

// PROF: Fixed vocabulary: stump/pending lookup/promise/field demand/qualifier. "Promise" is never defined.
Finalization resolves any open ? in two phases after inference. A parked stump is _spent_ if its result has already been used at a type that is neither a variable nor ★, as in @ex-spent. The first phase, $scripts(#st_mat)^*$, materializes every spent stump by F-hit; it skips the stumps that are not spent and those that the saturation of an earlier step has already discharged. The second phase, $scripts(#st_fin)^*$, forces every stump still parked to ★ by F-★.

The order of the phases is forced. F-★ cannot finalize a spent stump: its equation $δ ≐ ★$ clashes, since ★ is rigid. F-hit instead extends the blocking row-variable by the demanded field at the type its result was spent on, $α ≔ (l: δ | α′)$, and the saturation of $#st_solve$ wakes the stump, whose lookup now hits, together with every other stump that waited on α. These may hit as well, advance to α′, or stay blocked further along, so the set of stumps that F-★ must handle is known only after materialization. Running F-★ first would be unsound: a stump with label l forced to ★ on α would afterwards hit, once a spent stump on the same α and the same l is materialized, while its selection keeps the type ★.

// PROF: Finalization at top level gives star to every unspent selection of a nixpkgs file (each is a top-level function): the dominant case gets the weak typing.
Materialization is the one step of the algorithm that places a field into a row-variable. It is not a forced move: the field could as well be supplied further along the row, and the typing it produces is one of several. It is sound, since the result is a declarative typing, and it happens after inference proper, where no further equation depends on the choice. Neither phase runs at a `let` boundary. Stumps that were generalized by A-let live in their scheme and are discharged per use, and a stump blocked on a variable of Γ may still be answered by an application further out. The remaining $?$ of @sec-motivation become ★ here and nowhere else.

#example(name: [Waking, re-parking and finalizing])[
  In `r: (x: x.l) (r ‖ {m = c})`, A-sel-★ parks the stump $⟨ρ ⊳ ρ.l ↓ δ⟩$, where ρ is the fresh row of x. The argument has type ${m: 𝓫_c | ρ_r}$ by A-conc, where $ρ_r$ is the row of r, and A-λ-E solves $ρ ≔ (m: 𝓫_c | ρ_r)$. This write makes the stump stale, since its recorded blocker ρ is solved. Saturation re-asks the lookup, which skips m by L-miss and stops at $ρ_r$, and K-repark files the stump under $ρ_r$. Nothing solves $ρ_r$ afterwards, so finalization pins δ to ★ by F-★. The program has type ${ρ_r} → ★$.
] <ex-repark>

#example(name: [Why quiescence matters])[
  Wrap the function of @ex-repark in a binding, `let g = (r: (x: x.l) (r ‖ {m = c})) in {a = g {l = c}; b = g {}}`. After A-λ-E the stump is stale, and K-repark files it under $ρ_r$. The bound term has type ${ρ_r} → δ$, its stump is blocked on $ρ_r ∈ macron(α)$, and A-let generalizes it to $∀(ρ_r: "Row", δ: "Type"). ⟨(m: 𝓫_c | ρ_r).l ↓ δ⟩ ⇒ {ρ_r} → δ$. The use at ${l = c}$ answers $𝓫_c$, the use at ${}$ answers ★. Had the stump stayed filed under the solved ρ, it would not be blocked on a variable of $macron(α)$, so it would stay in $Δ_Γ$. Since $⟦S₁⟧Δ_Γ$ mentions $ρ_r$ and δ, neither could be generalized, g would be monomorphic, and its two uses would clash on $ρ_r$. Quiescence guarantees that the blocker A-let reads is the variable the lookup actually waits on.
] <ex-quiescence>

#example(name: [A spent promise])[
  In `x: (x.l) c`, A-sel-★ parks $⟨ρ ⊳ ρ.l ↓ δ⟩$, and A-λ-E unifies δ with $𝓫_c → γ$ for a fresh γ. The promise is spent: F-★ would have to unify $𝓫_c → γ$ with ★, which clashes because ★ is rigid. F-hit instead solves $ρ ≔ (l: 𝓫_c → γ | ρ′)$ for a fresh ρ′. Saturation wakes the stump, the lookup now hits, and K-hit unifies the found type with the promised one. The program has type ${l: 𝓫_c → γ | ρ′} → γ$.
] <ex-spent>

== Inference Metatheory <inference-metatheory>

#theorem(name: [Termination of inference], lean: "runF_terminates, satStep_wf")[
  Saturation admits no infinite run, and for every term e the entry point $⊢ e ⇒ τ; S′$ returns a verdict.
] <thm-infer-term>

// PROF: No rule consumes a Stuck verdict; the behaviour on Stuck is unspecified, but G3 relies on it.
#theorem(name: [Soundness of inference], lean: "runSound, run_typed")[
  If $⊢ e ⇒ τ; S′$, then $∅ ⊢ e : ⟦S′⟧τ$.
] <thm-infer-sound>

Inference is not complete; @incompleteness lists where it gives up.
