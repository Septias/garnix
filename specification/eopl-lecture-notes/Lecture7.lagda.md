	-*- mode: agda2;-*-

```
module Lecture7 where
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; sym; trans)
open import Data.Empty using (⊥)
open import Data.Unit using (⊤; tt)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_; proj₁; proj₂; _,_)
open import Relation.Nullary using (¬_)
open import Function using (_∘_)


variable A : Set
```

# Decidable - Booleans and Decision Procedures

Relations: evidence or computation?

## The evidence approach: _≤_ : ℕ → ℕ → Set
```
data _≤_ : ℕ → ℕ → Set where
  z≤n : ∀ {n} → zero ≤ n
  s≤s : ∀ {m n} → m ≤ n → suc m ≤ suc n

_ : 1 ≤ 3
_ = s≤s z≤n
```

## The computation approach

```
data Bool : Set where
  true false : Bool
```

## Example
```
_≤ᵇ_ : ℕ → ℕ → Bool
zero ≤ᵇ n = true
suc m ≤ᵇ zero = false
suc m ≤ᵇ suc n = m ≤ᵇ n
```

What is the tradeoff between `m ≤ n` or `m ≤ᵇ n ≡ true`?

## Relating Evidence and Computation

```
T : Bool → Set
T true = ⊤
T false = ⊥

T⇒≡ : ∀ b → T b → b ≡ true
T⇒≡ true tb = refl
T⇒≡ false ()

≡⇒T : ∀ {b} → b ≡ true → T b
≡⇒T refl = tt

≤ᵇ⇒≤ : ∀ m n → T (m ≤ᵇ n) → m ≤ n
≤ᵇ⇒≤ zero n = λ m≤ᵇn → z≤n
≤ᵇ⇒≤ (suc m) zero = λ()
≤ᵇ⇒≤ (suc m) (suc n) = λ m≤ᵇn → s≤s (≤ᵇ⇒≤ m n m≤ᵇn)


≤⇒≤ᵇ : ∀ {m}{n} → m ≤ n → T (m ≤ᵇ n)
≤⇒≤ᵇ z≤n = tt
≤⇒≤ᵇ (s≤s m≤n) = ≤⇒≤ᵇ m≤n
```

## Booleans are not enough

A proposition `A` is decidable, if we can either prove `A` or `¬ A`.
Cf. law of excluded middle!

```
data Dec (A : Set) : Set where
  yes  :   A → Dec A
  no   : ¬ A → Dec A
```

Many datatypes come with decidable equality and/or decidable ordering

```
¬suc≤zero : ∀ {m} → ¬ (suc m ≤ zero)
¬suc≤zero ()

_≤?_ : (m n : ℕ) → Dec (m ≤ n)
zero ≤? n = yes z≤n
suc m ≤? zero = no ¬suc≤zero
suc m ≤? suc n
  with m ≤? n
... | yes m≤n = yes (s≤s m≤n)
... | no ¬m≤n = no (λ{(s≤s m≤n) → ¬m≤n m≤n})
```

An alternative definition exploiting `_≤ᵇ_`

```
_≤?′_ : (m n : ℕ) → Dec (m ≤ n)
m ≤?′ n
  with m ≤ᵇ n | ≤ᵇ⇒≤ m n | ≤⇒≤ᵇ {m}{n}
... | true    | x       | y = yes (x tt)
... | false   | x       | y = no y
```

The problem of the `with` abstraction is that it forgets the connection
between the abstracted expression and the result obtained by pattern
matching. The above pattern is one way to amend this situation.

Alternatively, the `with` abstraction has a facility to reify the connection
as an equality. The implementation of `_≤?″_` gives an example.

```
case_of_ : ∀ {A B : Set} → A → (A → B) → B
case a of f = f a

_≤?″_ : (m n : ℕ) → Dec (m ≤ n)
m ≤?″ n
  with m ≤ᵇ n in eq
... | true  = yes (≤ᵇ⇒≤ m n (≡⇒T eq))
... | false = no (λ m≤n → case (trans (sym eq) (T⇒≡ (m ≤ᵇ n) (≤⇒≤ᵇ m≤n))) of λ())
```

The `with e in v ...` works as follows. The expression `e` is matched against the
subsequent patterns and the variable `v` scopes over the right-hand sides of these
patterns. In the right-hand side expression for pattern `p`, the variable `v` has
type `e ≡ p`.

In the `true` branch of the above example, we have `eq : m ≤ᵇ n ≡ true`.
In the `false` branch, we have `eq : m ≤ᵇ n ≡ false`.
In the same branch, we first derive `m ≤ᵇ n ≡ true` from `m≤n : m ≤ n`.
Using transitivity of `_≡_` we obtain `true ≡ false`, a contradiction.
We expose this contradiction with the `case_of_` function (also in the standard library
module `Function`), which allows us to pattern match against `true ≡ false` in an expression.

```
¬zero≡suc : ∀ {m} → ¬ zero ≡ suc m
¬zero≡suc ()

_=?_ : (m n : ℕ) → Dec (m ≡ n)
zero =? zero = yes refl
zero =? suc n = no ¬zero≡suc
suc m =? zero = no (¬zero≡suc ∘ sym)
suc m =? suc n
  with m =? n
... | yes m≡n = yes (cong suc m≡n)
... | no m≢n  = no (λ{ refl → m≢n refl})
```

## From Booleans to Decidables and back

```
⌊_⌋ : Dec A → Bool
⌊ yes x ⌋ = true
⌊ no x ⌋ = false

toWitness : (D : Dec A) → T ⌊ D ⌋ → A
toWitness (yes x) td = x
toWitness (no ¬x) ()

fromWitness : (D : Dec A) → A → T ⌊ D ⌋
fromWitness (yes x) a = tt
fromWitness (no ¬a) a = ¬a a
```

### Example

```
_≤ᵇ′_ : ℕ → ℕ → Bool
m ≤ᵇ′ n = ⌊ m ≤? n ⌋
```

## Connectives for Decidables

×-dec, ⊎-dec, ¬-dec

## Proof by reflection

Example: minus with predicate

```
minus : (m n : ℕ) → n ≤ m → ℕ
minus zero zero n≤m = zero
minus (suc m) zero n≤m = suc m
minus (suc m) (suc n) (s≤s n≤m) = minus m n n≤m


_ : minus 3 1 (s≤s z≤n) ≡ 2
_ = refl

minus″ : (m n : ℕ) → { T (n ≤ᵇ m) } → ℕ
minus″ zero zero = zero
minus″ (suc m) zero = suc m
minus″ (suc m) (suc n) { tnm} = minus″ m n {tnm}

_ : minus″ 3 1 ≡ 2
_ = refl
```
The following typing cannot be completed because the type of the first hole is `⊥`.
```
-- _ : minus″ 1 3 {{!!}} ≡ 42
-- _ = {!!}
```

Completing the following definitions requires similar contortions as in the definition
of `_≤?″_`.
```
≤⇒≤? : ∀{n}{m} → n ≤ m → T ⌊ n ≤? m ⌋
≤⇒≤? z≤n = tt
≤⇒≤? {suc n}{suc m} (s≤s n≤m)
  with n ≤? m
... | yes _   = tt
... | no ¬n≤m = ¬n≤m n≤m

minus′ : (m n : ℕ) → T ⌊ n ≤? m ⌋ → ℕ
minus′ zero zero tnm = zero
minus′ (suc m) zero tnm = suc m
minus′ (suc m) (suc n) tnm
  with n ≤? m
... | yes n≤m = minus′ m n (≤⇒≤? n≤m)
minus′ (suc m) (suc n) () | no ¬n≤m
```


