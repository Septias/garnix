	-*- mode: agda2;-*-

```
module Lecture11 where
open import Data.Empty using (⊥)
open import Data.Maybe using (Maybe; just; nothing)
open import Data.Nat using (ℕ; zero; suc)
open import Data.Product using (_×_; proj₁; proj₂; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.String using (String; _≟_)
open import Data.Unit using (⊤; tt)
open import Function using (_∘_)
open import Relation.Binary.PropositionalEquality using (_≡_; _≢_; refl; cong; sym; trans)
open import Relation.Nullary using (¬_; contradiction)
open import Relation.Nullary.Decidable using (Dec; yes; no; False; toWitnessFalse; ¬?)

```


# Alternative styles of semantics

So far, we considered *small-step reduction semantics*:

* a syntactic method (defined on the abstract syntax of expressions)
* breaks down computation into single steps that apply (mostly) local rewrites
* defined as a relation `_⟶_ : Term → Term → Set` and its reflexive-transitive closure
* scales to concurrent languages

Closest competitor: *big-step semantics*

* syntactic
* squeezes all computation in a single, structured step
* defined as a relation `_⇓_ : Term → Value → Set`
* here `Value` could be distinct from `Term`, but often `Value ⊆ Term`
* drawback: difficult to model non-terminating computations

Different beast: *denotational semantics*

* maps syntax to a *semantic domain* of our choosing
* no notion of reduction or computation, but a term is directly mapped to its value
* defined as a function `⟦_⟧ : Term → Domain`
* the definition is *compositional*, i.e., the semantics of a term is defined as a function of the semantics of its subterms.
* drawback: non-trivial domain construction required to model recursion and non-terminating computations

## Plan

We generally stick with the simply-typed lambda calculus.

* Define big-step semantics and relate to small-step semantics
* Define a terminating subset `λT` of our subject calculus
* Define denotational semantics for `λT` and relate



# Big step semantics

```
open import Lecture10 renaming (_—→_ to _⟶_; _—↠_ to _⟹_)

infix 2 _⇓_

data _⇓_ : ∅ ⊢ A → ∅ ⊢ A → Set where

  ⇓-ƛ : ∀ {N : ∅ , A ⊢ B} → (ƛ N) ⇓ (ƛ N)

  ⇓-· : ∀ {M : ∅ ⊢ A ⇒ B} {M′ : ∅ , A ⊢ B} {N W : ∅ ⊢ A} {V : ∅ ⊢ B} →
        M ⇓ ƛ M′
      → N ⇓ W
      → M′ [ W ] ⇓ V
        --------------------
      → M · N ⇓ V

  ⇓-zero : `zero ⇓ `zero

  ⇓-suc : ∀ {M V : ∅ ⊢ `ℕ}
      →  M ⇓ V
        --------------------
      → `suc M ⇓ `suc V

  ⇓-case-zero : ∀ {L : ∅ ⊢ `ℕ} {M W : ∅ ⊢ A} {N : ∅ , `ℕ ⊢ A}
      → L ⇓ `zero
      → M ⇓ W
        --------------------
      → case L M N ⇓ W

  ⇓-case-suc : ∀ {L V : ∅ ⊢ `ℕ} {M W : ∅ ⊢ A} {N : ∅ , `ℕ ⊢ A}
      → L ⇓ `suc V
      → N [ V ] ⇓ W
        --------------------
      → case L M N ⇓ W

  ⇓-μ : ∀ {M : ∅ , A ⊢ A} {V : ∅ ⊢ A}
      → M [ μ M ] ⇓ V
      → μ M ⇓ V

```

Let's show that each outcome of the big-step relation is a syntactic value,
which is similar to a type safety result.

```
⇓-value : ∀ {M V : ∅ ⊢ A} → M ⇓ V → Value V
⇓-value ⇓-ƛ = ƛ _
⇓-value (⇓-· ⇓ ⇓₁ ⇓₂) = ⇓-value ⇓₂
⇓-value ⇓-zero = `zero
⇓-value (⇓-suc ⇓) = `suc (⇓-value ⇓)
⇓-value (⇓-case-zero ⇓ ⇓₁) = ⇓-value ⇓₁
⇓-value (⇓-case-suc ⇓ ⇓₁) = ⇓-value ⇓₁
⇓-value (⇓-μ ⇓) = ⇓-value ⇓
```

Important: this lemma says nothing about nonterminating computations.
If (small-step) reduction of `M` does not terminate,
then there is no `V` such that there is a (finite) derivation of `M ⇓ V`.


One way of fixing this issue is gas.

```
data [_]_⇓_ : ℕ → ∅ ⊢ A → Maybe (∅ ⊢ A) → Set where

  out-of-gas : ∀ {M : ∅ ⊢ A}
             →  [ zero ] M ⇓ nothing

  ⇓-ƛ : ∀ {g}{N : ∅ , A ⊢ B} → [ g ] (ƛ N) ⇓ just (ƛ N)

  ⇓-·₁ : ∀ {g} {M : ∅ ⊢ A ⇒ B} {N : ∅ ⊢ A} →
        [ g ] M ⇓ nothing
        --------------------
      → [ suc g ] M · N ⇓ nothing

  ⇓-·₂ : ∀ {g}{M : ∅ ⊢ A ⇒ B} {M′ : ∅ , A ⊢ B} {N : ∅ ⊢ A} →
        [ g ] M ⇓ just (ƛ M′)
      → [ g ] N ⇓ nothing
        --------------------
      → [ suc g ] M · N ⇓ nothing

  ⇓-·₃ : ∀ {g} {M : ∅ ⊢ A ⇒ B} {M′ : ∅ , A ⊢ B} {N W : ∅ ⊢ A} →
        [ g ] M ⇓ just (ƛ M′)
      → [ g ] N ⇓ just W
      → [ g ] M′ [ W ] ⇓ nothing
        --------------------
      → [ suc g ] M · N ⇓ nothing

  ⇓-· : ∀ {g} {M : ∅ ⊢ A ⇒ B} {M′ : ∅ , A ⊢ B} {N W : ∅ ⊢ A} {V : ∅ ⊢ B} →
        [ g ] M ⇓ just (ƛ M′)
      → [ g ] N ⇓ just W
      → [ g ] (M′ [ W ]) ⇓ just V
        --------------------
      → [ suc g ] M · N ⇓ just V

  ⇓-zero : ∀ {g} → [ g ] `zero ⇓ just `zero

  -- ⇓-suc : ∀ {M V : ∅ ⊢ `ℕ}
  --     →  M ⇓ V
  --       --------------------
  --     → `suc M ⇓ `suc V

  -- ⇓-case-zero : ∀ {L : ∅ ⊢ `ℕ} {M W : ∅ ⊢ A} {N : ∅ , `ℕ ⊢ A}
  --     → L ⇓ `zero
  --     → M ⇓ W
  --       --------------------
  --     → case L M N ⇓ W

  -- ⇓-case-suc : ∀ {L V : ∅ ⊢ `ℕ} {M W : ∅ ⊢ A} {N : ∅ , `ℕ ⊢ A}
  --     → L ⇓ `suc V
  --     → N [ V ] ⇓ W
  --       --------------------
  --     → case L M N ⇓ W

  -- ⇓-μ : ∀ {M : ∅ , A ⊢ A} {V : ∅ ⊢ A}
  --     → M [ μ M ] ⇓ V
  --     → μ M ⇓ V

```

Then we can say:

```
⇓-value′ : ∀ {n}{M V : ∅ ⊢ A} → [ n ] M ⇓ just V → Value V
⇓-value′ ⇓-ƛ = ƛ _
⇓-value′ (⇓-· x x₁ x₂) = ⇓-value′ x₂
⇓-value′ ⇓-zero = `zero
```

Big-step semantics are quite similar to interpreters.
In fact, the big-step relation is a (partial) function:

```
⇓-functional : ∀ {M V V′ : ∅ ⊢ A} → M ⇓ V → M ⇓ V′ → V ≡ V′
⇓-functional ⇓-ƛ ⇓-ƛ = refl
⇓-functional (⇓-· ⇓ ⇓₁ ⇓₂) (⇓-· ⇓₃ ⇓₄ ⇓₅)
  with refl ← ⇓-functional (⇓) ⇓₃
  with refl ← ⇓-functional ⇓₁ ⇓₄
  = ⇓-functional ⇓₂ ⇓₅
⇓-functional ⇓-zero ⇓-zero = refl
⇓-functional (⇓-suc ⇓) (⇓-suc ⇓₁)
  with refl ← ⇓-functional (⇓) ⇓₁
  = refl
⇓-functional (⇓-case-zero ⇓ ⇓₁) (⇓-case-zero ⇓₂ ⇓₃) = ⇓-functional ⇓₁ ⇓₃
⇓-functional (⇓-case-zero ⇓ ⇓₁) (⇓-case-suc ⇓₂ ⇓₃)
  with ⇓-functional (⇓) ⇓₂
... | ()
⇓-functional (⇓-case-suc ⇓ ⇓₁) (⇓-case-zero ⇓₂ ⇓₃)
  with ⇓-functional (⇓) ⇓₂
... | ()
⇓-functional (⇓-case-suc ⇓ ⇓₁) (⇓-case-suc ⇓₂ ⇓₃)
  with refl ← ⇓-functional (⇓) ⇓₂
  = ⇓-functional ⇓₁ ⇓₃
⇓-functional (⇓-μ ⇓) (⇓-μ ⇓₁)
  with refl ← ⇓-functional (⇓) ⇓₁
  = refl
```

Hence, we could implement it as a function, too.
TODO?

## Relation to small-step semantics

If the big-step semantics yields a value, we can reach that value in
a finite number of small-step reduction steps.

It requires a bunch of lemmas about multi-step reduction.

```
ξ-suc* : ∀ {M N : ∅ ⊢ `ℕ } → M ⟹ N → `suc M ⟹ `suc N
ξ-suc* (_ ∎) = `suc _ ∎
ξ-suc* (_ —→⟨ x ⟩ M⟹N) = (`suc _ —→⟨ ξ-suc x ⟩ (ξ-suc* M⟹N))

ξ-·₁* : ∀ {M N : ∅ ⊢ A ⇒ B } { L : ∅ ⊢ A } → M ⟹ N → M · L ⟹ N · L
ξ-·₁* (_ ∎) = _ · _ ∎
ξ-·₁* (_ —→⟨ x ⟩ M⟹N) = step—→ (_ · _) (ξ-·₁* M⟹N) (ξ-·₁ x)

ξ-·₂* : ∀ {M N : ∅ ⊢ A } { L : ∅ ⊢ A ⇒ B } → M ⟹ N → Value L → L · M ⟹ L · N
ξ-·₂* (_ ∎) v = _ · _ ∎
ξ-·₂* (_ —→⟨ x ⟩ M⟹N) v = step—→ (_ · _) (ξ-·₂* M⟹N v) (ξ-·₂ v x)

ξ-case* : ∀ {L L′ : ∅ ⊢ `ℕ}{M : ∅ ⊢ A} {N : ∅ , `ℕ ⊢ A} → L ⟹ L′ → case L M N ⟹ case L′ M N
ξ-case* (_ ∎) = case _ _ _ ∎
ξ-case* (_ —→⟨ x ⟩ L⟹L′) = step—→ (case _ _ _) (ξ-case* L⟹L′) (ξ-case x)

_++_ : ∀ {L M N : ∅ ⊢ A} → L ⟹ M → M ⟹ N → L ⟹ N
(_ ∎) ++ M⟹N = M⟹N
(_ —→⟨ x ⟩ L⟹M) ++ M⟹N = (_ —→⟨ x ⟩ (L⟹M ++ M⟹N))

```

```
big⇒small : ∀ {M V : ∅ ⊢ A} → M ⇓ V → M ⟹ V
big⇒small ⇓-ƛ = ƛ _ ∎
big⇒small (⇓-· M⇓V M⇓V₁ M⇓V₂) = 
  let r1 = ξ-·₁* (big⇒small M⇓V) in
  let r2 = ξ-·₂* (big⇒small M⇓V₁) (⇓-value M⇓V) in
  let r3 = r1 ++ r2 in
  let r4 = big⇒small M⇓V₂ in
  let r5 = (_ —→⟨ β-ƛ (⇓-value M⇓V₁) ⟩ r4) in
  r3 ++ r5
big⇒small ⇓-zero = `zero ∎
big⇒small (⇓-suc M⇓V) = ξ-suc* (big⇒small M⇓V)
big⇒small (⇓-case-zero M⇓V M⇓V₁) =
  let r1 = ξ-case* (big⇒small M⇓V) in
  let r2 = (_ —→⟨ β-zero ⟩ big⇒small M⇓V₁) in
  r1 ++ r2
big⇒small (⇓-case-suc M⇓V M⇓V₁)
  with ξ-case* (big⇒small M⇓V) | ⇓-value M⇓V
... | r1 | `suc v =
  let r2 = (_ —→⟨ β-suc v ⟩ big⇒small M⇓V₁) in
  r1 ++ r2
big⇒small (⇓-μ M⇓V) = (_ —→⟨ β-μ ⟩ big⇒small M⇓V)
```

For the reverse direction a little more work is required.

First we observe that values evaluate to themselves.
Additionally, if a value `V` evaluates to some term, the outcome must equal to `V`.

```
V⇓V : ∀ {V : ∅ ⊢ A} → Value V → V ⇓ V
V⇓V (ƛ N) = ⇓-ƛ
V⇓V `zero = ⇓-zero
V⇓V (`suc v) = ⇓-suc (V⇓V v)

V⇓W : ∀ {V W : ∅ ⊢ A} → Value V → V ⇓ W → V ≡ W
V⇓W (ƛ _) ⇓-ƛ = refl
V⇓W `zero ⇓-zero = refl
V⇓W (`suc v) (⇓-suc V⇓W₁) = cong `suc_ (V⇓W v V⇓W₁)
```

We also need the following expansion lemma:
If M ⟶ N and N ⇓ V, then the latter evaluation
can be expanded to an evaluation of M ⇓ V.

```
M⟶⇓V : ∀ {M N V : ∅ ⊢ A} → M ⟶ N → N ⇓ V → M ⇓ V
M⟶⇓V (ξ-·₁ M⟶N) (⇓-· N⇓V N⇓V₁ N⇓V₂) = ⇓-· (M⟶⇓V M⟶N N⇓V) N⇓V₁ N⇓V₂
M⟶⇓V (ξ-·₂ v@(ƛ N) M⟶N) (⇓-· N⇓V N⇓V₁ N⇓V₂)
  rewrite V⇓W v N⇓V = ⇓-· (V⇓V (ƛ _)) (M⟶⇓V M⟶N N⇓V₁) N⇓V₂
M⟶⇓V (β-ƛ vx) N⇓V = ⇓-· (V⇓V (ƛ _)) (V⇓V vx) N⇓V
M⟶⇓V (ξ-suc M⟶N) (⇓-suc N⇓V) = ⇓-suc (M⟶⇓V M⟶N N⇓V)
M⟶⇓V (ξ-case M⟶N) (⇓-case-zero N⇓V N⇓V₁) = ⇓-case-zero (M⟶⇓V M⟶N N⇓V) N⇓V₁
M⟶⇓V (ξ-case M⟶N) (⇓-case-suc N⇓V N⇓V₁) = ⇓-case-suc (M⟶⇓V M⟶N N⇓V) N⇓V₁
M⟶⇓V β-zero N⇓V = ⇓-case-zero (V⇓V `zero) N⇓V
M⟶⇓V (β-suc vx) N⇓V = ⇓-case-suc (V⇓V (`suc vx)) N⇓V
M⟶⇓V β-μ N⇓V = ⇓-μ N⇓V
```

The previous lemmas cover the two inductive cases for the main theorem.


```
small⇒big : ∀ {M V : ∅ ⊢ A} → M ⟹ V → Value V → M ⇓ V
small⇒big (V ∎) val-V = V⇓V val-V
small⇒big (M —→⟨ M⟶N ⟩ N⟹V) val-V = M⟶⇓V M⟶N (small⇒big N⟹V val-V)
```


