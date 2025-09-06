	-*- mode: agda2;-*-

```
module Lecture6 where
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong)
open import Data.Empty using (⊥)
open import Data.Nat using (ℕ; zero; suc; _<_; _≤_; s≤s; z≤n)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_; proj₁; proj₂; _,_)

-- from isomorphism
infix 0 _≃_
record _≃_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_


```

Let's assume that A and B are implicitly quantified as of type Set:

```
variable
  A B C : Set
  n : ℕ
```

```
⊎-elim : A ⊎ B → (A → C) → (B → C) → C
⊎-elim (inj₁ x) ac bc = ac x
⊎-elim (inj₂ y) ac bc = bc y

×-elim : A × B → (A → B → C) → C
×-elim (a , b) c = c a b
```

```
⊥-elim : ⊥ → C
⊥-elim ()
```

# Negation

Constructive negation `¬ A` is modeled by *reductio ad absurdum*, i.e,
if we assume `A`, then we obtain a contradiction.


Negation can now be defined as a function that maps to the empty type.

```
infix 3 ¬_
¬_ : Set → Set
¬ A = A → ⊥

¬-elim : ¬ A → ¬ A
¬-elim ¬x = ¬x
```

For convenience, we define an eliminator for negation.

```
contradiction : ¬ A → A → B
contradiction ¬x x = ⊥-elim (¬x x)
```


In classical logic, double negation of some proposition is equivalent to the proposition.
In intuitionistic logic, only one direction holds.

```
¬¬-intro : A → ¬ (¬ A)
¬¬-intro a ¬a = ¬a a
```

The reverse direction does not hold, but ...


Contraposition holds

```
contraposition : (A → B) → (¬ B → ¬ A)
contraposition f ¬b a = ¬b (f a)
```

Inequality

```
_≢_ : A → A → Set
x ≢ y = ¬ (x ≡ y)

1≢2 : 1 ≢ 2
1≢2 ()

z≠suc : ∀ {n : ℕ} → zero ≢ suc n
z≠suc ()

```

Any two proofs of a negation are equal!

```
postulate
  ext : (f g : A → B) → (∀ x → f x ≡ g x) → f ≡ g

assimilate : (p q : ¬ A) → p ≡ q
assimilate p q = ext p q (λ a → ⊥-elim (p a))
```

Ex: Show that `_<_` is irreflexive.

```
<-irrefl : ¬ (n < n)
<-irrefl (s≤s n<n) = <-irrefl n<n
```

Ex: Show trichotomy
Trichotomy is a property of total orderings on A.
For m, n ∈ A either
* m < n ( and m ≢ n and ¬ (m > n))
* m ≡ n ...
* m > n ...

```
data Trichotomy (m n : ℕ) : Set where
  m<n : m < n → m ≢ n → ¬ (n < m) → Trichotomy m n
  m≡n : ¬ (m < n) → m ≡ n → ¬ (n < m) → Trichotomy m n
  m>n : ¬ (m < n) → m ≢ n → n < m → Trichotomy m n

trichotomy : (m n : ℕ) → Trichotomy m n
trichotomy zero zero = m≡n <-irrefl refl <-irrefl
trichotomy zero (suc n) = m<n (s≤s z≤n) z≠suc λ{ () }
trichotomy (suc m) zero = m>n (λ()) (λ{ ()}) (s≤s z≤n)
trichotomy (suc m) (suc n)
  with trichotomy m n
... | m<n x y z = m<n (s≤s x)               (λ {refl → y refl}) λ { (s≤s x₁) → z x₁}
... | m≡n x y z = m≡n (λ{ (s≤s x₁) → x x₁}) (cong suc y)        λ{ (s≤s x₁) → z x₁}
... | m>n x y z = m>n (λ{ (s≤s x₁) → x x₁}) (λ{ refl → y refl}) (s≤s z)
```

## Classical vs. intuitionistic logic

In classical logic, we can prove some additional theorems.
* double negation elimination:  `¬ ¬ A → A` 
* law of excluded middle: `A ⊎ ¬ A`
* Peirce's law: `((A → B) → A) → A`
* Implication as disjunction: `(A → B) → ¬ A ⊎ B`
* De Morgan's law: `¬ (¬ A × ¬ B) → A ⊎ B`

Neither of those hold in intuitionistic logic. But, for example, the law of excluded middle cannot be refuted, either.
Hence, its adoption as a postulate does not affect the consistency of the logic.

```
postulate
  em : ∀ {A : Set} → A ⊎ ¬ A
```

```
em-irrefutable : ∀ {A : Set} → ¬ ¬ (A ⊎ ¬ A)
em-irrefutable k = k (inj₂ (λ x → k (inj₁ x)))
```

# Universals

Universal quantification corresponds to a *dependent function type*.
To prove a proposition `∀ (x : A) → B x` (where `B : A → Set` is a proposition depending on `x : A`),
we have to present evidence in the form of a function ` (x : A) → B x `.
This function is *dependent* because its range depends on the *value* of `A` provided as input.
Its shape is `λ (x : A) → N x`. This notation implies that `N` is a term that depedends on `x`.

∀ x. P
a
----------------
P[x/a]


t : (x : A). B
a : A
----------------
t a : B[x/a]



```
∀-elim : {B : A → Set} → (∀ (x : A) → B x) → (a : A) → B a
∀-elim f = f
```


alternative name: dependent product, Π-type, notation `Π[ x ∈ A ] B`


## Properties of universals

```
postulate
  ∀-distrib-× : ∀ {A : Set} {B C : A → Set} →
    (∀ (x : A) → B x × C x) ≃ (∀ (x : A) → B x) × (∀ (x : A) → C x)
```

```
⊎∀-implies-∀⊎ : ∀ {A : Set} {B C : A → Set} →
  (∀ (x : A) → B x) ⊎ (∀ (x : A) → C x) → ∀ (x : A) → B x ⊎ C x
⊎∀-implies-∀⊎ (inj₁ x) a = inj₁ (x a)
⊎∀-implies-∀⊎ (inj₂ y) a = inj₂ (y a)
```

# Existentials

The existentially quantified proposition `Σ[ x ∈ A ] B x` holds
if the proposition `B M` holds for some term `M` of type `A`.
Here `B M` stands for the proposition `B x` with each free occurrence of `x` in `B` replaced by `M`.
Variable `x` appears free in `B x` but bound in `Σ[ x ∈ A ] B x`.

We formalise existential quantification by declaring a suitable record type:
```
record Σ (A : Set) (B : A → Set) : Set where
  constructor ⟨_,_⟩
  field
    proj₁ : A
    proj₂ : B proj₁
```

We obtain a convenient *logical* syntax for existentials with a `syntax` declaration.


```
Σ-syntax : (A : Set) (B : A → Set) → Set
Σ-syntax = Σ
infix 2 Σ-syntax
syntax Σ-syntax A (λ x → Bx) = Σ[ x ∈ A ] Bx
```

This declaration introduces the new syntax on *the right side* and gives its definition on the left.
To import this syntax, we have to import `Σ-syntax`!


alternative name: dependent sum

Common notation for existentials (if the domain is easily inferrable).

```
∃ : ∀ {A : Set} (B : A → Set) → Set
∃ {A} B = Σ A B

∃-syntax = ∃
syntax ∃-syntax (λ x → B) = ∃[ x ] B
```

```
∃-elim : ∀{ B : A → Set} → Σ A B → (∀ (x : A) → B x → C) → C
∃-elim ⟨ a , ba ⟩ k = k a ba
```


```
postulate
  ∀∃-currying : ∀ {A : Set} {B : A → Set} {C : Set}
    → (∀ x → B x → C) ≃ (∃[ x ] B x → C)
```

```
postulate
  ∃-distrib-⊎ : ∀ {A : Set} {B C : A → Set} →
    ∃[ x ] (B x ⊎ C x) ≃ (∃[ x ] B x) ⊎ (∃[ x ] C x)
```

```
postulate
  ∃×-implies-×∃ : ∀ {A : Set} {B C : A → Set} →
    ∃[ x ] (B x × C x) → (∃[ x ] B x) × (∃[ x ] C x)
```

# Universals, Existentials, and Negation

```
postulate
  ¬∃≃∀¬ : ∀ {A : Set} {B : A → Set}
    → (¬ ∃[ x ] B x) ≃ ∀ x → ¬ B x
```

```
postulate
  ∃¬-implies-¬∀ : ∀ {A : Set} {B : A → Set}
    → ∃[ x ] (¬ B x)
      --------------
    → ¬ (∀ x → B x)
```

Some additional lemmas from the seminar.

```
lemma-c : (¬ B → ¬ A) → (A → ¬ ¬ B)
lemma-c imp a ¬b = imp ¬b a

lemma-d : ¬ ¬ ¬ A → ¬ A
lemma-d ¬¬¬a a = ¬¬¬a (λ ¬a → ¬a a)
```
