```
module Lecture3 where
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong₂; cong-app; subst)
open Eq.≡-Reasoning using (begin_; step-≡-∣; step-≡-⟩; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)
open import Data.Nat.Properties using (+-assoc; +-suc)
```


## Chapter "Relations"

### Less-than-or-equal

Let's define `≤` on natural numbers!

Approach: define as an inductive datatype.

```
data _≤_ : ℕ → ℕ → Set where

  z≤n : ∀ {n : ℕ}
        -----------
      → zero ≤ n

  s≤s : ∀ {m n : ℕ}
      → m ≤ n
        -------------
      → suc m ≤ suc n
```

```
_ : 1 ≤ 1
_ = s≤s z≤n
```

```
_ : 2 ≤ 3
_ = s≤s (s≤s z≤n)
```

```
-- _ : 1 ≤ 0
-- _ = {!!}
```
Right now, we see that, apparently, there is no proof for 1 ≤ 0
Later on, when we discuss negation, we can make that more precise!

```
infix 4 _≤_
```

(maybe: alternative definition for ≤′)

### Inversion

aka applying the inductive rules "backwards"

```
inv-s≤s : ∀ {m n} → suc m ≤ suc n → m ≤ n
inv-s≤s (s≤s s) = s

inv-z≤n : ∀ {m} → m ≤ zero → m ≡ zero
inv-z≤n z≤n = refl
```

### Properties

* reflexive
* transitive
* anti-symmetric
* total

Reflexivity
```
≤-refl : ∀ {n} → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl
```
Transitivity

```
≤-trans : ∀ {m n o} → m ≤ n → n ≤ o → m ≤ o
≤-trans z≤n n≤o = z≤n
≤-trans (s≤s m≤n) (s≤s n≤o) = s≤s (≤-trans m≤n n≤o)
```

==> ≤ is a preorder

Anti-symmetry

```
anti-sym : ∀ {m n} → m ≤ n → n ≤ m → m ≡ n
anti-sym z≤n z≤n = refl
anti-sym (s≤s m≤n) (s≤s n≤m) = cong suc (anti-sym m≤n n≤m)
```

==> ≤ is a partial order

Totality

we want to express that
for all m, n ∈ ℕ, m ≤ n or n ≤ m

we use a data type `Total m n`

```
data Total (m n : ℕ) : Set where

  fwd : m ≤ n → Total m n
  bwd : n ≤ m → Total m n

≤-total : ∀ {m n} → Total m n
≤-total {zero} {n} = fwd z≤n
≤-total {suc m} {zero} = bwd z≤n
≤-total {suc m} {suc n}
  with ≤-total {m} {n}
... | fwd x = fwd (s≤s x)
... | bwd x = bwd (s≤s x)

-- equivalently the dots can be expanded

≤-total'' : ∀ {m n} → Total m n
≤-total'' {zero} {n} = fwd z≤n
≤-total'' {suc m} {zero} = bwd z≤n
≤-total'' {suc m} {suc n} with ≤-total'' {m} {n}
≤-total'' {suc m} {suc n} | fwd x = fwd (s≤s x)
≤-total'' {suc m} {suc n} | bwd x = bwd (s≤s x)

```

introduces the `with` abstraction: to pattern match on a computed value, here the inductive hypothesis


```
≤-total'-aux : ∀ {m n} → Total m n → Total (suc m) (suc n)
≤-total'-aux (fwd x) = fwd (s≤s x)
≤-total'-aux (bwd x) = bwd (s≤s x)

≤-total' : ∀ m n → Total m n
≤-total' zero n = fwd z≤n
≤-total' (suc m) zero = bwd z≤n
≤-total' (suc m) (suc n) = ≤-total'-aux (≤-total' m n)
```





### Monotonicity

```
+-monoʳ-≤ : ∀ {n₁ n₂ m} → n₁ ≤ n₂ → m + n₁ ≤ m + n₂
+-monoʳ-≤ {m = zero} n1≤n2 = n1≤n2
+-monoʳ-≤ {m = suc m} n1≤n2 = s≤s (+-monoʳ-≤ n1≤n2)
```

```
m≤n+m : ∀ m n → m ≤ n + m
m≤n+m m zero = ≤-refl
m≤n+m zero (suc n) = z≤n
m≤n+m (suc m) (suc n) rewrite +-suc n m = s≤s (m≤n+m m (suc n))

+-monoˡ-≤ : ∀ {n m₁ m₂} → m₁ ≤ m₂ → m₁ + n ≤ m₂ + n
+-monoˡ-≤ {n} z≤n = m≤n+m n _
+-monoˡ-≤ (s≤s m1≤m2) = s≤s (+-monoˡ-≤ m1≤m2)
```

```
+-mono-≤ : ∀ {n₁ n₂ m₁ m₂} → n₁ ≤ n₂ → m₁ ≤ m₂ → n₁ + m₁ ≤ n₂ + m₂
+-mono-≤ n1≤n2 m1≤m2 = ≤-trans (+-monoˡ-≤ n1≤n2) (+-monoʳ-≤ m1≤m2)
```

### Strict inequality

The book gives an inductive definition...
The standard library definition is this one:

```
_<_ : ℕ → ℕ → Set
m < n = suc m ≤ n
```

Trichotomy

Structure of the proof
* define a datatype for Tri (with three cases m < n, m ≡ n, n < m)
* induction on m and n
* use with-abstraction for the successor cases (like for Total)

### Even and odd

Want formalize even and odd numbers.
New feature: mutual definition of two inductive datatypes.

```
data odd  : ℕ → Set

data even : ℕ → Set where

  zero : even zero

  suc  : ∀ {n} → odd n → even (suc n)

data odd where

  suc : ∀ {n} → even n → odd (suc n)
```

Want to prove that the sum of even number is even.
(requires to also prove odd+even is odd)
```
e+e≡e : ∀ {m n} → even m → even n → even (m + n)
o+e≡o : ∀ {m n} → odd m → even n → odd (m + n)

e+e≡e zero en = en
e+e≡e (suc om) en = suc (o+e≡o om en)

o+e≡o (suc em) en = suc (e+e≡e em en)
```
