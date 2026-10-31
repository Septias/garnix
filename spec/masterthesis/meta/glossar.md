
## Proofs
- *Head Rigidity*: The head constructor does not change (functions stay functions)
- *Inversion*: Invert a typing judgement to learn more about the premise type
- *Frozen*: A position typed at ★ that no substitution can refine again
- *Blurring*: Weakening a definite type to ★ (T-★-intro)
- *Context Conversion*: Typing sees the context only through lookups
- *⊑-rigidity*: τ′ ⊑ τ and τ ≠ ★ implies τ′ and τ share their head constructor; ★ sits only below itself
- *On the nose*: Math slang for "exactly" — holds strictly, not just up to ≈ or ⊑
- *Instance-closed*: Our version of let, where e₁ must type at every instance of the scheme
- *Factorization*: An alignment of one row's atoms against the other's (Levi-style); a unifier is one, two distinct ones mean no mgu
- *Cancellative*: a | ρ₁ ≈ a | ρ₂ implies ρ₁ ≈ ρ₂ (and from the right) — shared atoms strip off either end
- *mgu*: Most general unifier


## Stumps
- *Stump*: A lookup ⟨ρ.l ↓ δ⟩ carried by a qualified scheme; δ names its result. Instantiation discharges it (D-hit, D-⊥, D-?)
- *Spent stump*: A stump whose result is already used at a type that is neither a variable nor ★; an instance must make its lookup hit. Never "promise"
- *Parked stump*: A stump in the solver state, ⟨α ⊳ ρ.l ↓ δ⟩, filed under its blocker α
- *Blocker*: The variable a parked stump waits on: a row or label variable
- *Pending lookup*: Informal word for a stump; only in the introduction, the overview, the conclusion and the definition of stumps
- *Wake-up*: Re-running a stump's lookup when unification solves its blocker
- *Instance-Closed T-let*: ∀ τ₁ ≤ σ. Γ ⊢ e₁: τ₁
- *↯*: Lookup error;  residual-risk disjunct of progress
- *Stuck*: Verdict of ≐ᵣ when no move applies. Conservative: the problem may still have an mgu (`stuck_masks_mgu`). The Wand equation is the prime example


## Scheme
- *Qualified Scheme*: A scheme carrying its pending lookups as constraints
- *Qualifier*: The role of a stump in a scheme ("stays a qualifier of the answer"), not a separate object
- *Refinement*: A lookup result improving (? → τ or ⊥) once instantiation unblocks it; only ? can improve, definite results are final
- *Fillability test*: The syntactic check of A-let and the entry point that a scheme is inhabited: the instance that sends the generalized type-variables to ★ and fills each blocker with the fields its spent stumps demand discharges every stump. Sufficient, not complete: the spent chain `x: x.l.m // {}` is inhabited but fails it. *Fillable*: passes the test


## Spine
- *Spine*: A row normalized mod assoc/units to a flat atom sequence
- *Segment*: A var-free part in a spine; distinct labels commute inside it, nothing crosses a var
- *Window*: The leading (or trailing) segment of a spine


## Misc
- *Wand ambiguity*: The lookup in `a: b: (a ‖ b).l`: it answers ? because the field may come from either operand. Handled by stumps
- *Wand equation*: (α₁ | … | αₙ) ≐ᵣ (l: 𝓫) with n ≥ 2: it has unifiers but no mgu and is reported stuck. Never called "Wand ambiguity"
- *Soft typing*: Programs keep their untyped semantics; ★ marks where the analysis gave up, warnings say why
- *Key*: A label l or a label variable α
- *Keyed field*: A row field ${k}: τ whose label is a key
- *Unknown type ★*: The type of what the analysis cannot decide; no elimination form, unifies only with itself
- *Field demand*: What other row systems hand to unification for a selection; used only to contrast with our lookup relation
- *Scoped labels*: Duplicate labels stay in the row; lookup takes the leftmost occurrence, and ‖ puts its right operand first, so the right operand wins
