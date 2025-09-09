	-*- mode: agda2;-*-

```
module Lecture9 where
open import Data.Empty using (⊥)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Product using (_×_; proj₁; proj₂; _,_; ∃-syntax)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.String using (String; _≟_)
open import Data.Unit using (⊤; tt)
open import Function using (_∘_)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; cong; sym; trans)
open import Relation.Nullary using (¬_; contradiction)
open import Relation.Nullary.Decidable using (Dec; yes; no; False; toWitnessFalse; ¬?)

open import Lecture8

_⟶_ = _—→_

```

# Properties of Simply-Typed Lambda Calculus

We want to ensure that evaluation (i.e., reflexive-transitive reduction) of a lambda term
never results in a *stuck term*. A stuck term cannot reduce further, but it is not a value.

Examples of stuck terms:

```
stuck₁ : Term
stuck₁ = `zero · `zero

stuck₂ : Term
stuck₂ = case (ƛ "x" ⇒ (` "x")) [zero⇒ `zero |suc "x" ⇒ `zero ]

illegal₁ : Term  -- not stuck
illegal₁ = `suc (ƛ "x" ⇒ (` "x"))
```

In an implementation, a stuck term would lead to a misinterpretation of a value (i.e.,
confusing a pointer with a number) and, in the worst case, to a crash.
A type system avoids such situations by guaranteeing *type safety*.

We now work towards proving type safety using a standard approach, which structures the desired result
in two theorems.

1. Type preservation: If `∅ ⊢ M ⦂ A` and `M ⟶ M′`, then `∅ ⊢ M′ ⦂ A`.
2. Progress: If `∅ ⊢ M ⦂ A`, then either `Value M` or there exists some `M′` such that `M ⟶ M′`.

Taken together, an induction on `_—↠_` (multi-step reduction) yields the following:

Type safety: If `∅ ⊢ M ⦂ A` and `M —↠ M′`, then either `Value M′` or there some `M′` such that `M ⟶ M′`.

That is, each reduction sequence of a type term either ends in a value or it does not terminate.`
Thus, it will never reach a stuck term.


Note: to discuss a type theory like the one underlying Agda, we'd have to
consider reduction of open terms (i.e., with free variables) , too.
In this context, we want *subject reduction*, which is

Subject reduction: If `Γ ⊢ M ⦂ A` and `M ⟶ M′`, then `Γ ⊢ M′ ⦂ A`.


## Progress

Progress: If `∅ ⊢ M ⦂ A`, then either `Value M` or there exists some `N` such that `M ⟶ N`.

We encapsulate the progress property in a datatype.

```
data Progress (M : Term) : Set where

  step : ∀ {N}
    → M —→ N
      ----------
    → Progress M

  done :
      Value M
      ----------
    → Progress M
```

The proof of progress is by induction on the typing derivation for the term.

```
progress : ∅ ⊢ M ⦂ A → Progress M
progress (⊢ƛ ⊢M) = done (ƛ _ ⇒ _)
progress (⊢M · ⊢M₁)
  with progress ⊢M
... | step M⟶ = step (ξ-·₁ M⟶)
... | done val-M@ (ƛ _ ⇒ _)
  with progress ⊢M₁
... | step M₁⟶ = step (ξ-·₂ val-M M₁⟶)
... | done val-M₁ = step (β-ƛ val-M₁)
progress ⊢zero = done `zero
progress (⊢suc ⊢M)
  with progress ⊢M
... | step M⟶ = step (ξ-suc M⟶)
... | done val-M = done (`suc val-M)
progress (⊢case ⊢M ⊢M₁ ⊢M₂)
  with progress ⊢M
... | step M⟶ = step (ξ-case M⟶)
... | done `zero = step β-zero
... | done (`suc val-M) = step (β-suc val-M)
progress (⊢μ ⊢M) = step β-μ
```


## Preservation


```
preserve : ∅ ⊢ M ⦂ A  →  M ⟶ M′  →  ∅ ⊢ M′ ⦂ A

subst-preserves : ∀ {x Γ} →  Γ , x ⦂ A ⊢ N ⦂ B  →   ∅ ⊢ M ⦂ A   →  Γ ⊢ N [ x := M ] ⦂ B

preserve (⊢L · ⊢M) (ξ-·₁ L⟶) = (preserve ⊢L L⟶) · ⊢M
preserve (⊢V · ⊢M) (ξ-·₂ val-V M⟶) = ⊢V · (preserve ⊢M M⟶)
preserve (⊢ƛ ⊢N · ⊢M) (β-ƛ val-M) = subst-preserves ⊢N ⊢M
preserve (⊢suc ⊢M) (ξ-suc M⟶) = ⊢suc (preserve ⊢M M⟶)
preserve (⊢case ⊢M ⊢M₁ ⊢M₂) (ξ-case M⟶) = ⊢case (preserve ⊢M M⟶) ⊢M₁ ⊢M₂
preserve (⊢case ⊢M ⊢M₁ ⊢M₂) β-zero = ⊢M₁
preserve (⊢case (⊢suc ⊢M) ⊢M₁ ⊢M₂) (β-suc x) = subst-preserves ⊢M₂ ⊢M 
preserve ⊢μM@ (⊢μ ⊢M) β-μ = subst-preserves ⊢M ⊢μM 
```

Onward to substitution preserves typing!

```
Ren : Context → Context → Set

postulate
  weaken : ∀ {Γ} → ∅ ⊢ M ⦂ A → Γ ⊢ M ⦂ A

swap : ∀ {Γ}{x}{y} → x ≢ y → Ren (Γ , x ⦂ A , y ⦂ B) (Γ , y ⦂ B , x ⦂ A)


subst-preserves {x = x} (⊢` Z) ⊢M
  with x ≟ x
... | yes x≡x = weaken ⊢M
... | no x≢x  = contradiction refl x≢x
subst-preserves {x = x} (⊢` {x = y} (S y≢x y∈)) ⊢M
  with y ≟ x
... | yes y≡x = contradiction y≡x y≢x 
... | no y≢x  = ⊢` y∈
subst-preserves {x = y} (⊢ƛ {x = x} ⊢N) ⊢M
  with x ≟ y
... | yes refl = {!!}
... | no x≢y = ⊢ƛ (subst-preserves {!!} ⊢M)
subst-preserves (⊢N · ⊢N₁) ⊢M = (subst-preserves ⊢N ⊢M) · (subst-preserves ⊢N₁ ⊢M)
subst-preserves ⊢zero ⊢M = ⊢zero
subst-preserves (⊢suc ⊢N) ⊢M = ⊢suc (subst-preserves ⊢N ⊢M)
subst-preserves (⊢case ⊢N ⊢N₁ ⊢N₂) ⊢M = {!!}
subst-preserves (⊢μ ⊢N) ⊢M = {!!}

```

In the case for lambda, we need to be able to swap assumptions (bound to different variables).
The general solution for these problems are *renamings*.

A context Δ is a renaming of a context Γ if:
Whenever I can find a binding x⦂T in Γ, then I can also find x⦂T in Δ.

```
Ren Γ Δ = ∀ {x} {A} → Γ ∋ x ⦂ A → Δ ∋ x ⦂ A
```

Swapping is an example:

```
swap x≢y Z = S (x≢y ∘ sym) Z
swap x≢y (S x Z) = Z
swap x≢y (S x (S x₁ z∈Γ)) = S x₁ (S x z∈Γ)
```









## Type Safety


```
type-safety : ∅ ⊢ M ⦂ A  →  M —↠ M′  →  Progress M′
type-safety ⊢M (_ ∎) = progress ⊢M
type-safety ⊢M (_ —→⟨ M⟶ ⟩ M—↠) = type-safety (preserve ⊢M M⟶) M—↠
```


## Evaluation



```
record Gas : Set where
  constructor gas
  field
    amount : ℕ
```


```
data Finished (N : Term) : Set where

  done :
      Value N
      ----------
    → Finished N

  out-of-gas :
      ----------
      Finished N
```

```
data Steps (L : Term) : Set where

  steps : ∀ {N}
    → L —↠ N
    → Finished N
      ----------
    → Steps L
```

```
eval : ∀ {L A}
  → Gas
  → ∅ ⊢ L ⦂ A
    ---------
  → Steps L
eval g ⊢L = {!!}
```


