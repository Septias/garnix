```
module Lecture1 where
import Data.Vec using (lookup)
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong₂; cong-app; subst)
open Eq.≡-Reasoning using (begin_; step-≡-∣; step-≡-⟩; _∎)
```

# Essentials of Programming Languages SS 2025

## Core Topic: Specify the meaning of programs (semantics)

There a several popular ways to do that.

### Denotational semantics

* a mapping ⟦_⟧ : Syntax → MathObject
* this mapping is compositional:
  the meaning of a composite expression is a function of the meanings of the subexpressions

Example:
Arithmetic expressions

E ::= E + E | cst n

looks like a context free grammar...
but we are not interested in concrete syntax like (5+6)+7
rather we consider it *abstract syntax*, that is, we're only interested in the derivation trees.

Let's define the semantics of expressions:

⟦_⟧ : E → ℕ
⟦ E₁ + E₂ ⟧ = ⟦ E₁ ⟧ + ⟦ E₂ ⟧
⟦ cst n ⟧ = n

* the denotation should be stable under equivalence of expressions
For example

⟦ E + cst 0 ⟧ = ⟦ E ⟧ + ⟦ cst 0 ⟧ = ⟦ E ⟧ + 0 = ⟦ E ⟧

### Operational semantics

a syntax-based method: do the computation directly on the syntax
one way: small-step reduction semantics

we specify a single reduction relation ⟶ ⊆ E × E

ADD: (cst m) + (cst n) ⟶ cst (m+n)

not applicable to: (cst k) + ((cst m) + (cst n))

E₁ ⟶ E₁′
------------------------
(E₁ + E₂) ⟶ (E₁′ + E₂)


E₂ ⟶ E₂′
------------------------
(E₁ + E₂) ⟶ (E₁ + E₂′)


Now: (cst k) + ((cst m) + (cst n)) ⟶ (cst k) + (cst (m+n))

We want to iterate reduction, so we consider ⟶* , the multi-step reduction relation
(the reflexive, transitive closure of ⟶)

What if E₁ ⟶ E₂ , does it hold that ⟦ E₁ ⟧ = ⟦ E₂ ⟧ ?

#### a second syntax-based method: big-step operational semantics

we do the computation on the syntax, but take some liberty with the results

we specify a binary relation ⇓ ⊆ E × V  , V is a set of *values*, often a subset of expressions

Back to the example: take V = ℕ

⇓-const : cst n ⇓ n

⇓-add :
E₁ ⇓ n₁
E₂ ⇓ n₂
---------------
E₁ + E₂ ⇓ (n₁+n₂)


# Agda demo

A natural number is either zero or the successor of another natural number.

```
data ℕ : Set where
  zero : ℕ
  suc : ℕ → ℕ

_+_ : ℕ → (ℕ → ℕ)
zero + n = n
suc m + n = suc (m + n)
```

Proving a property by induction is just like implementing a recursive function...
To do so ʻ we write the property as the *type* of a function:

```
+-identityˡ : ∀ (n : ℕ) → zero + n ≡ n
+-identityˡ n = refl

+-identityʳ : ∀ (n : ℕ) → n + zero ≡ n
+-identityʳ zero = refl
+-identityʳ (suc n) = cong suc (+-identityʳ n)

+-assoc : ∀ m n o → m + (n + o) ≡ (m + n) + o
+-assoc zero n o = refl
+-assoc (suc m) n o rewrite +-assoc m n o = refl

+-suc : ∀ m n → m + (suc n) ≡ (suc m) + n  {- ≡ suc (m + n)   definitionally -}
+-suc zero n = refl
+-suc (suc m) n = cong suc (+-suc m n)
```

### Agda for safe programming


Recurring problem: buffer overflows causing security breaches

In Agda, we can define a vector datatype, where the type contains the number of elements in the vector.

```
data Vec : ℕ → Set where
  [] : Vec zero
  _∷_ : ∀ {n} → ℕ → Vec n → Vec (suc n)

-- concatenation

-- using implicit arguments

_++_ : ∀ {m} {n} → Vec m → Vec n → Vec (m + n)
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

-- equality proof using *equational reasoning* 

sym' : {A : Set}{x y : A} → x ≡ y → y ≡ x
sym' refl = refl

lemma : ∀ {n} → suc n ≡ n + suc zero
lemma {n} =
  begin
    suc n
  ≡⟨ sym (+-identityʳ (suc n)) ⟩
    (suc n + zero)
  ≡⟨ sym (+-suc n zero) ⟩
    n + suc zero
  ∎

reverse : ∀ {m} → Vec m → Vec m
reverse [] = []
reverse {suc m} (x ∷ vs) rewrite lemma{m} = reverse vs ++ (x ∷ [])
```

That is `v : Vec n` contains exactly `n` elements.

```
postulate
  _<_ : ℕ → ℕ → Set
  index : ∀ {n} → ∀ i → i < n → Vec n → ℕ
```

Now we want to program a safe indexing operation `lookup` so that `lookup i v`
only typechecks if `i` is provably less than `n`. To do so, we can either
proceed as outlined with `index` or we can define a special datatype of
admissible indices for `v`:

```
data Fin : ℕ → Set where

  zero : ∀ {n : ℕ} → Fin (suc n)
  suc  : ∀ {n : ℕ} → Fin n → Fin (suc n)
```

Elements of `Fin n` are the natural number less than `n`.
For example, `Fin zero` is the empty set.
`Fin (suc zero)` contains only `zero`.

```
lookup : ∀ {n} → Vec n → Fin n → ℕ
lookup [] ()
lookup (x ∷ _) zero = x
lookup (_ ∷ vs) (suc i) = lookup vs i
```



