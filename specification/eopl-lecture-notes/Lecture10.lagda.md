	-*- mode: agda2;-*-

```
module Lecture10 where
open import Data.Empty using (⊥)
open import Data.Nat using (ℕ; zero; suc)
-- open import Data.Product using (_×_; proj₁; proj₂; _,_; ∃-syntax)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.String using (String; _≟_)
open import Data.Unit using (⊤; tt)
open import Function using (_∘_)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; cong; sym; trans)
open import Relation.Nullary using (¬_; contradiction)
open import Relation.Nullary.Decidable using (Dec; yes; no; False; toWitnessFalse; ¬?)



infix  4 _⊢_
infix  4 _∋_
infixl 5 _,_

infixr 7 _⇒_

infix  5 ƛ_
infix  5 μ_
infixl 7 _·_
infix  8 `suc_
infix  9 `_
infix  9 S_
-- infix  9 #_
```



# The Simply-Typed Lambda Calculus, Again

In this lecture, we take two steps away from the previous development for the simply typed lambda calculus.

1. We adopt a new representation for variables, called de Bruijn representation.
2. We change the term representation from Curry style to Church style.

## De Bruijn encoding for variables

Traditionally, we write lambda terms with explicit variables.

    λx.x
    λx.λy.x
    λf.λx.f x
    λf.λx.f (f x)
    λf.f z

As the (reduction) behavior of a term is independent of the naming of its bound variables,
lambda terms are usually consider up to *α conversion*. For example

    λx.x
    λy.y
    λz.z

are considered equal (up to α conversion). Similarly,

    λx.λy.x ≡ λy.λx.y ≡ λy.λz.y ≡ ....

    λf.f z ≡ λg.g z ≡ ... (the free variable z must neither be changed nor captured; the term λz.z z is *different*!)

On paper and in informal proof, one often uses the *Barendregt convention* which assumes that the bound variables of a
term are always disjoint from the free variables of a term. That is, it assumes that we tacitly α convert terms as convenient.

We already discussed problems with variable scopes in the context of substitution and used a version of substitution
restricted to closed substituends to avoid issues with name capturing. Here is the offending case:

Considering (λx. M)[y := N]  (where x ≠ y)
If x occurs free in N, then we have to α convert λx to some λz such that z does not occur free in M or N.
On paper, using the Barendregt convention, we can just tacitly assume that x ∉ free(N), but in a mechanized setting, we cannot.



### De Bruijn Indices

(Nicolaas Govert) De Bruijn's automath system was one of the first systems supporting mechanized proof.
This system relies on *de Bruijn indices* to represent binding structure without names.
Introduced in de Bruijn's paper "Lambda Calculus Notation with Nameless Dummies: A Tool for Automatic Formula Manipulation, with
Application to the Church-Rosser Theorem".











Idea of de Bruijn indices:
Instead of referring to the binding location of a variable by name, we refer to it by distance to the binder.
That is, the encoding of a use of a variable is a natural number that tells us how many binders we have to traverse
to arrive at the binding site (e.g., a lambda) of the variable.

Example:

    λx.x           ⟶    λ 0
    λx.λy.x        ⟶    λ λ 1
    λf.λx.f x      ⟶    λ λ (1 0)
    λf.λx.f (f x)  ⟶    λ λ 1 (1 0)
    λf. f (λx. f)  ⟶    λ (0 λ 1)

Dealing with free variables depends on a context that does the bookkeeping for binding introduction.
If the context introduces z before x  (as in Γ = ∅ , z ⦂ Tᶻ , x ⦂ Tˣ), then

    λf.f z         ⟶    λ (0 2)

In this notation, defining substitution *for closed terms* is very simple.
A (simple) substitution has the first

M [j ↦ N]    where j is a valid index.

Variable case:

i [j ↦ N] = N if i = j else i

Application:

(L M) [j ↦ N] = L[j ↦ N] M[j ↦ N]

Lambda:

(λ M) [j ↦ N] = λ (M[j+1 ↦ N])







If N does contain free variables, we have to shift them at each lambda because their binding site is one step further away.

(λ M) [j ↦ N] = λ (M[j+1 ↦ N ^ 0])

N ^ j means "shift every variable >= j, but leave variables < j alone"

i ^ j    = i+1 if i >= j else i
(L M) ^j = (L ^j) (M ^j)
(λ M) ^j = λ (M ^ j+1)






## Curry vs. Church style

Question: what is a legal term?

Curry (extrinsic): admit every syntactically well-formed term; every term has a semantics (even if it is nonsense).
                   Subsequently, a type system filters out a subset of well-typed terms.

Church (intrinsic): terms must be well-typed by construction and only those terms are given a semantics.

In the previous chapter, we discussed a Curry-style encoding of lambda calculus.
Now we switch to Church style.

At the same time, we switch to a typed variable representation, which is inspired by de Bruijn indices.




# Syntax

## Simple Types (as before)


```
data Type : Set where
  _⇒_ : Type → Type → Type
  `ℕ : Type

variable
  A B C : Type
```

## Contexts

As before, but we drop the names as we now rely on de Bruijn indices!

```
data Context : Set where
  ∅   : Context
  _,_ : Context → Type → Context

variable
  Γ Δ : Context
```

Contexts are isomorphic to `List Type`.





## Variable lookup

`Γ ∋ A` is the index of a variable of type `A` in context `Γ`


```
data _∋_ : Context → Type → Set where

  Z : ∀ {Γ}
      ---------
    → Γ , A ∋ A

  S_ : ∀ {Γ}
    → Γ ∋ A
      ---------
    → Γ , B ∋ A
```




## Terms and typing

In Church-style, constructing a term is the same as constructing its typing derivation!

We write `Γ ⊢ A` for the type of terms of type `A` in context `Γ`.

```
data _⊢_ : Context → Type → Set where

  `_ : ∀ {Γ A}
    → Γ ∋ A
      -----
    → Γ ⊢ A

  ƛ_  : ∀ {Γ A B}
    → Γ , A ⊢ B
      ---------
    → Γ ⊢ A ⇒ B

  _·_ : ∀ {Γ A B}
    → Γ ⊢ A ⇒ B
    → Γ ⊢ A
      ---------
    → Γ ⊢ B

  `zero : ∀ {Γ}
      ---------
    → Γ ⊢ `ℕ

  `suc_ : ∀ {Γ}
    → Γ ⊢ `ℕ
      ------
    → Γ ⊢ `ℕ

  case : ∀ {Γ A}
    → Γ ⊢ `ℕ
    → Γ ⊢ A
    → Γ , `ℕ ⊢ A
      ----------
    → Γ ⊢ A

  μ_ : ∀ {Γ A}
    → Γ , A ⊢ A
      ---------
    → Γ ⊢ A
```

# Semantics

Recall that substitution is needed to define β reduction.
We already know that a proper definition of substitution requires renaming.
This time, we start bottom up...



## Renaming

As before, a renaming is a mapping between variable lookups in different environments:

```
Ren : Context → Context → Set
Ren Γ Δ = ∀ {A} → Γ ∋ A → Δ ∋ A
```

Extend a renaming to adapt to an extra binding.

```
extr : Ren Γ Δ → Ren (Γ , A) (Δ , A)
extr ρ Z = Z
extr ρ (S x) = S (ρ x)
```

We apply a renaming to a term.

```
rename : ∀ {Γ Δ}
  → Ren Γ Δ
  → Γ ⊢ A
  → Δ ⊢ A
rename ρ (` x) = ` (ρ x)
rename ρ (ƛ ⊢A) = ƛ rename (extr ρ) ⊢A
rename ρ (⊢A · ⊢A₁) = (rename ρ ⊢A) · (rename ρ ⊢A₁)
rename ρ `zero = `zero
rename ρ (`suc ⊢A) = `suc (rename ρ ⊢A)
rename ρ (case ⊢A ⊢A₁ ⊢A₂) = case (rename ρ ⊢A) (rename ρ ⊢A₁) (rename (extr ρ) ⊢A₂)
rename ρ (μ ⊢A) = μ (rename (extr ρ) ⊢A)
```













## Substitution

Here, we define full, unrestricted substitution.
Instead of restricting to a single variables, we define *simultaneous substitution*,
which substitutes all variables in scope at once.
This generalized setting makes dealing with the substitution easier!

A substitution from Γ to Δ maps any variable of type `A` in an environment `Γ` to a term in environment Δ.

```
Sub : Context → Context → Set
Sub Γ Δ =  ∀ {A} → Γ ∋ A → Δ ⊢ A
```

Extension for substitution.

```
exts : Sub Γ Δ → Sub (Γ , A) (Δ , A)
exts σ Z = ` Z
exts σ (S x) = rename S_ (σ x)
```

We apply a substitution to a term.

```
subst : ∀ {Γ Δ}
  → Sub Γ Δ
  → Γ ⊢ A
  → Δ ⊢ A
subst σ (` x) = σ x
subst σ (ƛ ⊢A) = ƛ subst (exts σ) ⊢A
subst σ (⊢A · ⊢A₁) = (subst σ ⊢A) · (subst σ ⊢A₁)
subst σ `zero = `zero
subst σ (`suc ⊢A) = `suc (subst σ ⊢A)
subst σ (case ⊢A ⊢A₁ ⊢A₂) = case (subst σ ⊢A) (subst σ ⊢A₁) (subst (exts σ) ⊢A₂)
subst σ (μ ⊢A) = μ (subst (exts σ) ⊢A)
```











### special case: single substitution

Required case for type preservation / β reduction

```
σ₀ : (M : Γ ⊢ B) → Sub (Γ , B) Γ
σ₀ M Z = M
σ₀ M (S x) = ` x


_[_] : ∀ {Γ A B}
  → Γ , B ⊢ A
  → Γ ⊢ B
    ---------
  → Γ ⊢ A
_[_] {Γ} {A} {B} N M = subst (σ₀ M) N

```



## Values

```
data Value  {Γ} : ∀ {A} → Γ ⊢ A → Set where

  ƛ_ : (N : Γ , A ⊢ B)
      ---------------------------
    → Value (ƛ N)

  `zero : 
      -----------------
      Value `zero

  `suc_ : ∀ {V : Γ ⊢ `ℕ}
    → Value V
      --------------
    → Value (`suc V)
```

## Reduction

Due to intrinsic, Church-style encoding, reduction comes with proof of type preservation by construction!

```
infix 2 _—→_

data _—→_ : ∀ {Γ A} → (Γ ⊢ A) → (Γ ⊢ A) → Set where

  ξ-·₁ : ∀ {Γ A B} {L L′ : Γ ⊢ A ⇒ B} {M : Γ ⊢ A}
    → L —→ L′
      ---------------
    → L · M —→ L′ · M

  -- remove ξ-·₂ for call-by-name
  ξ-·₂ : ∀ {Γ A B} {V : Γ ⊢ A ⇒ B} {M M′ : Γ ⊢ A}
    → Value V
    → M —→ M′
      ---------------
    → V · M —→ V · M′

  β-ƛ : ∀ {Γ A B} {N : Γ , A ⊢ B} {W : Γ ⊢ A}
    → Value W         -- remove for call-by-name
      --------------------
    → (ƛ N) · W —→ N [ W ]

  ξ-suc : ∀ {Γ} {M M′ : Γ ⊢ `ℕ}
    → M —→ M′         -- remove for lazy numbers
      -----------------
    → `suc M —→ `suc M′

  ξ-case : ∀ {Γ A} {L L′ : Γ ⊢ `ℕ} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
    → L —→ L′
      -------------------------
    → case L M N —→ case L′ M N

  β-zero :  ∀ {Γ A} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
      -------------------
    → case `zero M N —→ M

  β-suc : ∀ {Γ A} {V : Γ ⊢ `ℕ} {M : Γ ⊢ A} {N : Γ , `ℕ ⊢ A}
    → Value V          -- remove for lazy numbers
      ----------------------------
    → case (`suc V) M N —→ N [ V ]

  β-μ : ∀ {Γ A} {N : Γ , A ⊢ A}
      ----------------
    → μ N —→ N [ μ N ]

```

For full reduction add

  ξ-ƛ : ∀ {Γ A B} {N N′ : Γ , A ⊢ B}
    → N —→ N′
    → ƛ N —→ ƛ N′ 

Now you can replace Value by Normalform and prove an analogous progress result for open terms.

## Reflexive-transitive closure

```
infix  2 _—↠_
infix  1 begin_
infixr 2 _—→⟨_⟩_
infix  3 _∎

data _—↠_ {Γ A} : (Γ ⊢ A) → (Γ ⊢ A) → Set where

  _∎ : (M : Γ ⊢ A)
      ------
    → M —↠ M

  step—→ : (L : Γ ⊢ A) {M N : Γ ⊢ A}
    → M —↠ N
    → L —→ M
      ------
    → L —↠ N

pattern _—→⟨_⟩_ L L—→M M—↠N = step—→ L M—↠N L—→M

begin_ : ∀ {Γ A} {M N : Γ ⊢ A}
  → M —↠ N
    ------
  → M —↠ N
begin M—↠N = M—↠N
```

## Progress

```
data Progress {A} (M : ∅ ⊢ A) : Set where

  step : ∀ {N : ∅ ⊢ A}
    → M —→ N
      ----------
    → Progress M

  done :
      Value M
      ----------
    → Progress M
```

The progress theorem.

```
progress : (M : ∅ ⊢ A) → Progress M
progress (ƛ ⊢A) = done (ƛ ⊢A)
progress (⊢A · ⊢A₁)
  with progress ⊢A
... | step x = step (ξ-·₁ x)
... | done val-A
  with progress ⊢A₁
... | step x = step (ξ-·₂ val-A x)
progress (⊢A · ⊢A₁) | done (ƛ N) | done x = step (β-ƛ x)
progress `zero = done `zero
progress (`suc ⊢A)
  with progress ⊢A
... | step x = step (ξ-suc x)
... | done x = done (`suc x)
progress (case ⊢A ⊢A₁ ⊢A₂)
  with progress ⊢A
... | step x = step (ξ-case x)
... | done `zero = step β-zero
... | done (`suc x) = step (β-suc x)
progress (μ ⊢A) = step β-μ
```
