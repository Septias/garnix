```
module Lecture2 where
import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong₂; cong-app; subst)
open Eq.≡-Reasoning using (begin_; step-≡-∣; step-≡-⟩; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)
open import Data.Nat.Properties using (+-assoc)
```

## Notes for chapter "Induction"


Towards proving commutativity of addition, we need two lemmas.

Instead of using `rewrite` we write equality proofs using equational reasoning (a convenient notation).

```
+-identityʳ : ∀ m → m + zero ≡ m
+-identityʳ zero = refl
+-identityʳ (suc n) = cong suc (+-identityʳ n)
```

```
+-suc : ∀ m n → m + suc n ≡ suc m + n
+-suc zero n = refl
+-suc (suc m) n = cong suc (+-suc m n)
```

```
+-comm : ∀ m n → m + n ≡ n + m
+-comm zero n = sym (+-identityʳ n)
+-comm (suc m) n =
  begin
    suc m + n
  ≡⟨⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨ sym (+-suc n m) ⟩
    n + suc m
  ∎

trans' : ∀ {A : Set}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans' refl eq2 = eq2

+-comm' : ∀ m n → m + n ≡ n + m
+-comm' zero n = sym (+-identityʳ n)
+-comm' (suc m) n = trans (cong suc (+-comm m n)) (sym (+-suc n m))
```

Selected exercises

```
+-swap : ∀ m n p → m + (n + p) ≡ n + (m + p)
+-swap m n p =
  begin
    m + (n + p)
  ≡⟨ sym (+-assoc m n p) ⟩
    m + n + p
  ≡⟨ cong (_+ p) (+-comm m n) ⟩
    n + m + p
  ≡⟨ +-assoc n m p ⟩
    n + (m + p)
  ∎

+-swap' : ∀ m n p → m + (n + p) ≡ n + (m + p)
+-swap' m n p = trans (sym (+-assoc m n p))
                (trans (cong (_+ p) (+-comm m n))
                        (+-assoc n m p))

```

```
_*'_ : ℕ → ℕ → ℕ
zero *' n = zero
suc m *' n = (m *' n) + n
```


```
*-distrib-+ : ∀ m n p → (m + n) * p ≡ m * p + n * p
*-distrib-+ zero n p = refl
*-distrib-+ (suc m) n p =
  begin
    (suc m + n) * p
  ≡⟨⟩
    p + (m + n) * p
  ≡⟨ cong (p +_) (*-distrib-+ m n p) ⟩
    p + (m * p + n * p)
  ≡⟨ sym (+-assoc p (m * p) (n * p)) ⟩
    p + m * p + n * p
  ≡⟨⟩
    suc m * p + n * p
  ∎
```



## Look ahead to chapter "Relations"

Let's define `≤` on natural numbers!


