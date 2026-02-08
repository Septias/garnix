Things to do for algebraic subtyping:

== Form the distributive lattice of types (part of subtyping rules basically)

- Every type needs to be in lattice
- There needs to a top and bottom type (such that every element has glb an lup)
- Distributivity rules τ₁ ∧ (τ₂ ∨ τ₃) == τ₁ ∧ τ₂ ∧ τ₁ ∧ τ₃
- Meet and Join for every two elements τ₁, τ₂
- Every τ has a complement ¬τ where τ ∧ ¬τ === T and τ ∨ ¬τ === ⊥

== Define Contexts
- Typing Γ
- Constraints (Bounds) Ξ
- Subtyping Σ

== Define declarative typing rules
- Simple type rules but they need to be sound

== Define algorithmic typing rules
- Actually think type vars and constraints

=== Bounds
- Bounds are of the form τ ≤ τ

=== Hypotheses
- Hypotheses must be guarded against immediate use, otherwise unsound

=== Levels
- Levels are used such that more more general tvs can not escape functions
- Skolem: A skolem is used to fix a more polymorphic type (let-var inside a function)
- Rigid Variable: A rigid variable is used fix a less polymorhpic type (function parameter)

==== Skolem
> used to fix a more polymorphic type

```nix
x: let
  f = y: x (y, y);
in E f

```
It would be problematic if we constrain α (typvar from x) with types from β (typevar from f) since f might be instantiated to different explicit types in the function body. These constraints are no longer connected to the initial α.


==== Rigid Variable
> used to fix a less polymorphic type

```

```


=== Recursion
- Manual unfolding
- Building types
- Noticing cycles


=== Consistency
- New ∀-quantified variable bounds have to be propagated


=== Normal Forms
- Needed to get efficient inference in face of boolean algebra


=== Constraining
- Subroutine to create new bounds



