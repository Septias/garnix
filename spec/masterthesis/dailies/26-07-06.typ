./26-07-04.typ
./26-07-07.typ

== Fragen
- Warum ist der preservation-proof noop?
  - Weil Claude quatsch gemacht hat?
    - Ja
- Nur closed terms?
  - Über non-closed terms kann man eh nicht rechnen…
- Instantiiere ich type-variablen, bei application?
  - Nur da? Ich glaub in `Infix…` ist Instantiation eine generelle Regel


== Lookup-Relation
- Basically: Lookup type as long as no row-var
  - Then return a type
- On first row-var: Create new constraint with all row-vars
  - And return ★

-----------------------
Γ ⊢ { l: τ | ρ }.l → Ξ


- I kind of have to proof:
  - I collect all row-vars
  - This really has to contain the field
    - l ∈ (S): if l ∈ α ∈ R then α ∈ S
- What I don't like is: the lookup relation does two things:
  - Create new constraints
  - Return a type
  - I think this could be cumbersom in proofs


== Subtype Relation
- Should I only define subtyping for records?
- I think that is possible for now and simplifies the work
- What do I do if I have to compare two type variables
- That is not possible lule
- So same instantation trick?
- This way, many more places can get type refinements
- Subtyping can create constraints?
  - `(l: τ) ∈< S`?
  - This could also just "fail with unknown"
  - But what do we do then typewise?
  - We can not "just get stuck because we can not argue"
  - We have to allow normal execution
  - But then we lose our type guarantee
  - Okay, so we have to convert to ★

Example:

```
f :: { l: int } -> int

(a: (f a)) :: r <= {l: int} => r -> int
(b: (a: (f (a || b) ))) -> ? :: ★ -> ★
(b: (a: (f (a || b) )){}) -> ? :: r <= {l: int} => r -> int
```
- In this example, we can not decide whether (a || b) <= {l: int}
- This means even though we return (f x) which is `int` we still have to return ★
  - This is because we can't guarantee that the program will turn out successful at this point
  - But now: If a programmer never sees an application, what can he do?
    - If there is an application somewhere: the typesystem could propagate that information?
  - But after we have supplied an empty record, (a || b -> b) and we know b !≤ { l: int}
- TODO: example for two type variables


== Inference Quirks
- Type is specialized during function application
  - Not only stucking!


== The hard proof steps
> I generally want to write the proofs paper-first and than translate them (possibly) to lean.

- For that I want to sketch out the hard proof parts on paper first.
- Hard proof parts: Preservation is ≤
- The context moves constraints around


*Preservation in Record-access*:
1. e = (e.l) -> ρ.l


*application*:
- e = (e₁e₂) then step(e):
  - case e₁:
    - case e₁ is Value (x: e₃): □ triv
    - case e₁ steps: e₁ -> e₁': □ (by induction)
  - case e₂
    - case e₂ is Value: e₁[x -> e₂] (by subst-lemma)
    - case e₂ steps: □ (by induction)

- The thing is, the context changes!
  - And it is generally a hard thing to proof, that this only improves type info
  - Basically proof that when I instantiate a type variable, I only get more precise types
  - Case ★: When (e.l: ★) before, then we might get a new type τ and (τ ≤ ★) for every τ
  - Case τ: When there was a type already, instantiation can not change that type (due to left-first lookups)
  - I only remove something

