#import "../functions.typ": *

== Reduction Semantic
- Laziness
  - Second arg of operators: Shouldn't that be handled by eval context?
  - Every array element: Evaluation context?
  - Record fields
  - Pattern default values
- Recursiveness
  - Records
  - Patterns w/ defaults
  - Let-statements


- Matching recursive patterns through defaults
- With binding power & deferred substitutions
- Lazyness
- Deep / Shallow
  - Needed only for operators and deepSeq
- Recursiveness


Indirect:
{ x = x; } -> {x = {x = x;}.x;}
{ x = 2; } -> { x = { x = 2 }.x}

Unfold:
a @ { x = x; } | x ∈ a -> { x = {x = x;}}

rec { l_i = t_i }.l | ∃l_i. l = l_i -> { x = {x = x;}}
rec { l_i = t_i }.l | ∃l_i. l = l_i  -> { x = {x = x;}}

rec { l_i = 2 + }.l | ∃l_i. l = l_i  -> { x = {x = x;}}


For recursion: Check if the var is defined in the record and if yes, take it from there?

{ x = 2 + x;}.x -> { x = 2 + {x = 2 + x}.x }
{ x = 2 + y; y = x;}.x -> { x = 2 + {y = x}.y; } -> { x = 2 + {y = {x = 2 + y}.x}.y; }
{ x = { y = x;}; y = x;}.x -> { x = 2 + {y = x}.y; } -> { x = 2 + {y = {x = 2 + y}.x}.y; }

{x = {x = x;}.x;}.x = {x = x;}.x


{ x ? y; y ? x}:


with { x = 2; }; with {y = 2 + x;}; y -> 4

== Occurrence

We add to our typesystem the usual


== Records

- { l = t; | r } <- rows
- { } ∧ { } ∧ { } <- Conjunctions (since we have them anyways)
- lacks : { x without }


Unchanged rest: { a: int; | r} -> { a: float; r}
Unchanged rest: { a: int } ∧ { .. } -> { a: int } ∧ ¬{ a : int} ∧ { .. } ~> { .. }
Extension: { .. }


```
T-RCD
Ξ,Γ ⊢ t_0: τ_0 ... Ξ, Γ ⊢ t_n: τ_n
---------------------------------------
Ξ,Γ ⊢ { ḻ:  ṯ  }: {arrow(l): τ}


T-SEL
Ξ,Γ ⊢ t: {l: τ}
---------------------
Ξ,Γ ⊢ t.l: τ


T-OR-NEG
Ξ,Γ ⊢ t_1: {l: τ_1}      Ξ, Γ ⊢ t₂: τ₁
--------------------------------------------
Ξ,Γ ⊢ (t₁).l #b[or] t₂ : τ₁


T-OR-POS
Ξ, Γ ⊢ t₁: τ₁     l ∉ τ₁    Ξ, Γ ⊢ t₂: τ₂
--------------------------------------------------
Ξ, Γ ⊢ (t_1).l #b[or] t₂: τ₂


T-REC-CONC
Ξ, Γ ⊢ a: { overline(l): overline(τ) } Ξ, Γ ⊢ b: { overline(b): overline(τ) }
-----------------------------------------------------
          Ξ, Γ ⊢ a "\/\/" b: 

T-REC-CHECK
Ξ, Γ ⊢ e: {..}
-----------------------
Ξ, Γ ⊢ e #b[?] l: bool


T-REC-DYN
Γ ⊢ a: { l: τ }     t -> l 
-----------------------
Γ ⊢ a.\${t} : τ


T-SEL-STR
Γ ⊢ a: { l: τ }
------------------
Γ ⊢ a.\${t} : τ
```


```
S-PAT


--------------
{ a: t } <= {| a: t |}

```


== Regarding Perreaux
- Since overloading is not possible in boolean algebra systems, we can not fully type nix in them. This is because due to the reflection checks, it is possible to overload functions.
- μ-rule is not possible because recursion might aries during type checking and is not attached to any syntax
- Tagged classes as 
