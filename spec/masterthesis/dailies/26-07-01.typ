
./26-07-02.typ

== Proofs
Given a set of values and smallstep reduction semantic (→):

*Progress*: If ⊢ e: τ then either e ∈ Values or there ∃e' such that e → e'

- I start from the syntax.
- And show that for every term, either it reduces, or it is a value
- Basically that is inductive / case analysis
- Why exactly do I need the typing statement?
  - Because we need to rule out ill-typed stuck terms
    - For example lookup
    - Lookup on an int
- Reduction of scoped records could be verbose

*Preservation*: If e: τ and e → e' then e': τ' und τ ≤ τ'

- Preservation basically shows that when I reduce a term it's type doesn't change
- This ensures that after static analysis, the properties hold during evaluation
- Type preservation is also proven over the small step relation and typing relation
- We show that after we made a step, then the type is still the same
- Also an inductive proof on the terms and the step function
- _Substitution Lemma_: if Γ, x:τ₁ ⊢ e : τ₂ and Γ ⊢ v : τ₁ then Γ ⊢ e[x:=v] : τ₂


Given an algorithmic ⊨ and deductive typesystem ⊢ we show:

*Soundness*: If ⊨ e: τ then ⊢ e: τ
- Hier ist die Frage, wie sich die Constraints

*Copmletenes*: If ⊢ e: τ then ⊨ e: τ

