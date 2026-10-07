== Gradual Calculus
> L2 with ★ read as the dynamic type: dynamic row ★ᵣ, consistent equivalence ≃, eliminators for ★, transient safety without casts
> Everything marked *claim* is unproven

l ∈ 𝓛  x ∈ 𝓧  𝓫 ∈ 𝓑  c ∈ 𝓒

e := c | x  | (x: e)  | e₁e₂ | (e₁ ‖ e₂) | e.l | { ξ } | let x = e₁ in e₂
| 'l | e₁.(e₂) | { \${e₁} = e₂ }
| e₁.l or e₂ | e ? l
ξ := ε | l = e | (ξ₁ | ξ₂)

k := l | α
τ := α | 𝓫 | ★ | τ -> τ | { ρ } | ⌊k⌋
ρ := ε | α | ★ᵣ | l: τ | \${k}: τ | (ρ₁ | ρ₂)
κ := Type | Row | Label

q := ⟨ρ.k ↓ δ⟩
Q := ∅ | q, Q
σ := ∀(ᾱ: κ̄). Q ⇒ τ
Γ := • | Γ·(x: σ) | Γ·(α: κ)

- New against L2: ★ᵣ, `or`, `?` (and 𝓫\_bool with tt, ff ∈ 𝓒 for `?`)
- ★ is the dynamic *type*, ★ᵣ the dynamic *row*: "some fields, unknown which"
- Nix sources: `args // {x = 1}` with args: ★ gives {x: 𝓫 | ★ᵣ}; `{x = 1} // args` gives {★ᵣ | x: 𝓫}


== Runtime
- Base: L2 runtime (lean/minimal.lean, Step / Value / Err): CBV application and
  let, lazy record fields, right-preferring concat
- New: wrong-tag eliminations are errors ↯tag, not stuck terms. Nix checks the
  tag of every eliminator operand itself:
  `(1).a`, `1 2`, `{} // 1` all raise a defined error (checked with nix eval)
- This is *transient* gradual typing (Vitousek et al.) with the checks already
  built into the language: no cast is ever inserted into a program

v := c | (x: e) | { b } | 'l
E := [] | E e | v E | E.l | E ‖ e | v ‖ E | let x = E in e
| E.(e) | v.(E) | { \${E} = e } | E.l or e | E ? l

tag(c) = 𝓫\_c   tag((x: e)) = fn   tag({ b }) = rcd   tag('l) = lab


=== Steps (unchanged from L2)

---------------------- R-β
(x: e) v ⟶ e[v/x]


-------------------------- R-let
let x = v in e ⟶ e[v/x]


b.l = e
---------------- R-sel
{ b }.l ⟶ e
// leftmost l-field wins; the field stays an unevaluated thunk


------------------------------ R-cat
{ b₁ } ‖ { b₂ } ⟶ { b₂ | b₁ }


b.l = e
-------------------- R-sel-dyn
{ b }.('l) ⟶ e


------------------------------ R-rcd-dyn
{ \${'l} = e } ⟶ { l = e }


e ⟶ e′
------------- R-ctx
E[e] ⟶ E[e′]


=== Observers (new)

b.l = e₁
---------------------- R-or-hit
{ b }.l or e₂ ⟶ e₁


l ∉ b
---------------------- R-or-miss
{ b }.l or e₂ ⟶ e₂


tag(v) ≠ rcd
------------------ R-or-tag
v.l or e₂ ⟶ e₂
// Nix: (1).a or 2 = 2


l ∈ b
---------------- R-has-tt
{ b } ? l ⟶ tt


l ∉ b
---------------- R-has-ff
{ b } ? l ⟶ ff


tag(v) ≠ rcd
------------- R-has-tag
v ? l ⟶ ff
// Nix: 1 ? a = false


=== Errors
- ↯ (lookup errors, L2): missing label, non-label key. Unchanged
- ↯tag (new): an eliminator meets the wrong tag

tag(v) ≠ fn
------------ X-app
v e ↯tag


tag(v) ≠ rcd
------------ X-sel
v.l ↯tag


tag(v) ≠ rcd
--------------- X-sel-dyn
v.(e) ↯tag


tag(v) ≠ rcd
--------------- X-cat-L
v ‖ e ↯tag


tag(v) ≠ rcd
------------------ X-cat-R
{ b } ‖ v ↯tag


r ↯tag
------------ X-ctx
E[r] ↯tag

- X-app fires before the argument is evaluated: Nix forces the function first
- `or` and `?` never raise ↯ or ↯tag on their own


== Sorts
- L2 sorts unchanged, plus:


------------- S-★ᵣ
Γ ⊢ ★ᵣ: Row


== Row-Lookup
- L2 rules unchanged (L-ε, L-hit, L-miss, L-α-free, L-conc-hit, L-conc-skip,
  L-conc-★), plus one rule:


------------- L-★ᵣ
★ᵣ.k ↓ ★
// any key, literal or variable

- The verdict is a *found* ★, definite and never ?. Nothing parks on ★ᵣ
- Scoped precedence then needs no further rule:
  - (l: τ | ★ᵣ).l ↓ τ by L-conc-hit
  - (m: τ | ★ᵣ).l ↓ ★ by L-conc-skip, L-★ᵣ
  - (★ᵣ | l: τ).l ↓ ★ by L-conc-hit: ★ᵣ may hold an l that shadows ours
- Found-via-★ᵣ is not presence: `{}` has type {★ᵣ} and `{}.l` still raises ↯.
  L2 already lets well-typed terms raise ↯, so progress is unaffected
- No lookup through ★ᵣ is ever ⊥
- Properties (*claim*, L-★ᵣ is closed and context-free): ↓ stays deterministic,
  total, and stable under substitution
- (★ᵣ | ρ) and ★ᵣ answer every lookup alike, but are NOT made ≈-equal:
  rowEquiv\_iff\_char would break, and they still differ for ≃ against a
  closed row with fewer fields


== Consistent equivalence  τ₁ ≃ τ₂
- One relation for "equal up to ≈ and consistent up to ★/★ᵣ" (Sekiyama-Igarashi's
  consistent equivalence). Using ≈ and a structural consistency one after the other
  would make the checker guess an intermediate type
- Their second reason (cast order becomes incoherent) does not apply: there are no casts
- Rows are compared as spines ⌈ρ⌉, peeling from the left by *row splitting*

=== Row splitting  s ⊲ₗ (τ, s′)
- Mirrors the lookup: the τ that s.l finds, and s with that field removed

---------------------- SP-hit
(l: τ)·s ⊲ₗ (τ, s)


l ≠ m   s ⊲ₗ (τ′, s′)
--------------------------------- SP-skip
(m: τ)·s ⊲ₗ (τ′, (m: τ)·s′)
// \${m}: τ counts as m: τ; a field keyed by a label variable blocks, like L-α-free


---------------------- SP-★ᵣ
★ᵣ·s ⊲ₗ (★, ★ᵣ·s)
// ★ᵣ can supply an l: ★ and stays, since it may hold more

- No rule for ⟨⟩ (the label is missing) and none for α·s (the var is rigid here)


=== Types

-------- CE-★-L
★ ≃ τ


-------- CE-★-R
τ ≃ ★


-------- CE-var
α ≃ α


-------- CE-base
𝓫 ≃ 𝓫


------------ CE-lab
⌊k⌋ ≃ ⌊k⌋


τ₁ ≃ τ₁′   τ₂ ≃ τ₂′
-------------------------- CE-fn
τ₁ -> τ₂ ≃ τ₁′ -> τ₂′


⌈ρ₁⌉ ≃ᵣ ⌈ρ₂⌉
---------------- CE-rcd
{ρ₁} ≃ {ρ₂}


=== Rows  s₁ ≃ᵣ s₂

----------- CE-ε
⟨⟩ ≃ᵣ ⟨⟩


s ≃ᵣ t
--------------- CE-ρ-var
α·s ≃ᵣ α·t


t ⊲ₗ (τ′, t′)   τ ≃ τ′   s ≃ᵣ t′
---------------------------------- CE-field-L
(l: τ)·s ≃ᵣ t


s ⊲ₗ (τ′, s′)   τ′ ≃ τ   s′ ≃ᵣ t
---------------------------------- CE-field-R
s ≃ᵣ (l: τ)·t


t ≈ t₁·t₂   s ≃ᵣ t₂
---------------------- CE-★ᵣ-L
★ᵣ·s ≃ᵣ t
// ★ᵣ absorbs any prefix t₁, also one with vars


s ≈ s₁·s₂   s₂ ≃ᵣ t
---------------------- CE-★ᵣ-R
s ≃ᵣ ★ᵣ·t

- Examples
  - (l: 𝓫 | m: 𝓫) ≃ᵣ (m: 𝓫 | l: 𝓫): SP-skip is ≈-comm
  - (l: 𝓫 | l: 𝓫 -> 𝓫) ≄ᵣ (l: 𝓫 -> 𝓫 | l: 𝓫): equal labels keep their order
  - (l: 𝓫 | ★ᵣ) ≃ᵣ (m: 𝓫 | ★ᵣ): split m:𝓫·★ᵣ at l through SP-★ᵣ, then CE-★ᵣ-L
  - (l: 𝓫 | ★ᵣ) ≄ᵣ (l: 𝓫 -> 𝓫 | ★ᵣ)
  - (l: 𝓫 | α) ≄ᵣ (α | l: 𝓫): nothing crosses a var, as in ≈
  - (★ᵣ | l: 𝓫) ≄ᵣ ε: the record has l
- Properties (*claim*)
  - reflexive, symmetric, not transitive
  - on ★-free and ★ᵣ-free types, ≃ coincides with ≈
  - τ₁ ≃ τ₂ iff τ₁ ≈ τ₁′, τ₁′ ∼ τ₂′, τ₂′ ≈ τ₂ for a structural consistency ∼
    (Sekiyama-Igarashi Thm 4.1; new here: several vars per row, mid-row ★ᵣ)
  - τ₁ ≃ τ₂ iff they have a common ⊑-lower bound up to ≈


== Matching  τ ▷ τ′
- Exposes the shape an eliminator needs. The ★-arms are the *eliminators for ★*


--------------------------- M-fn
τ₁ -> τ₂ ▷ τ₁ -> τ₂


----------------- M-fn-★
★ ▷ ★ -> ★


------------- M-rcd
{ρ} ▷ {ρ}


------------- M-rcd-★
★ ▷ {★ᵣ}


------------- M-lab
⌊k⌋ ▷ ⌊k⌋
// no ★-arm for keys yet, see FC-labels below

- A type variable does not match declaratively: types in a derivation are chosen,
  so pick the shape. Algorithmically α ▷ … binds α (see Inference)
- An elimination typed through a ★-arm is *dynamic*; the soft-typing report
  gets a warning there, like T-sel-⊥


== Precision
- L2 rules unchanged, plus:


-------- ⊑-★ᵣ
ρ ⊑ ★ᵣ

- Lookup monotonicity (*claim*): with res(τ) = τ and res(⊥) = res(?) = ★,
  ρ′ ⊑ ρ ⟹ res(ρ′.l) ⊑ res(ρ.l)
- ≃ and ▷ monotone (*claim*): τ₁′ ⊑ τ₁, τ₂′ ⊑ τ₂, τ₁′ ≃ τ₂′ ⟹ τ₁ ≃ τ₂;
  τ′ ⊑ τ, τ′ ▷ μ′ ⟹ τ ▷ μ with μ′ ⊑ μ


== Declarative
- L2 rules unchanged except the eliminators, which go through ▷, and
  application, which goes through ≃
- *T-★-intro is dropped.* Consistency at the use site does its job: a value
  is passed where ★ is expected instead of being blurred to ★ first
- T-sel-★ and T-sel-⊥ stay: ★ still enters through the lookup verdicts, and ⊥
  stays soft (typed ★ + W) rather than a static error
- Instantiation and discharge are unchanged. D-? and D-⊥ still demand θδ = ★,
  so a blurred result can only be eliminated through a ★-arm


Γ ⊢ e₁: τ   τ ▷ τ₁ -> τ₂   Γ ⊢ e₂: τ₁′   τ₁′ ≃ τ₁
------------------------------------------------------ T-λ-E
Γ ⊢ e₁e₂: τ₂


Γ ⊢ e₁: τ₁   τ₁ ▷ {ρ₁}   Γ ⊢ e₂: τ₂   τ₂ ▷ {ρ₂}
---------------------------------------------------- T-conc
Γ ⊢ e₁ ‖ e₂: { ρ₂ | ρ₁ }


Γ ⊢ e: τ   τ ▷ {ρ}   ρ.l ↓ τ′
-------------------------------- T-sel
Γ ⊢ e.l: τ′


Γ ⊢ e: τ   τ ▷ {ρ}   ρ.l ↓ ?
-------------------------------- T-sel-★
Γ ⊢ e.l: ★


Γ ⊢ e: τ   τ ▷ {ρ}   ρ.l ↓ ⊥
-------------------------------- T-sel-⊥
Γ ⊢ e.l: ★


Γ ⊢ e₁: τ₁   τ₁ ▷ {ρ}   Γ ⊢ e₂: τ₂   τ₂ ▷ ⌊k⌋   ρ.k ↓ τ′
------------------------------------------------------------- T-sel-dyn
Γ ⊢ e₁.(e₂): τ′
// T-sel-dyn-★ / T-sel-dyn-⊥ as T-sel-★ / T-sel-⊥

- Examples
  - `(λx. x.l) d` with d: ★: x: ★, then x.l: ★ via M-rcd-★ and L-★ᵣ; or
    x: {l: 𝓫 | ★ᵣ} and ★ ≃ {l: 𝓫 | ★ᵣ} at the application
  - `args // {x = c}` with args: ★: {x: 𝓫 | ★ᵣ}, and `.x` gives 𝓫
  - `{x = c} // args`: {★ᵣ | x: 𝓫}, and `.x` gives ★
  - The "★ has no eliminator" witness (incompleteness.md): the dynamic
    selection gives ★, and `.m` on it is typed ★ through M-rcd-★


== Safety
- Problem: `(λx. x.l) d` with d ⟶ c: 𝓫 reaches `c.l`. In L2 that is stuck,
  here it is ↯tag. Plain preservation still fails: `c.l` has no type
- Fix: transient checks, which exist *only in the proof*. The elaboration
  e ⇝ e⁺ inserts tag checks; erasing them gives back e, so no program is changed

e⁺ := … | ⦇T⦈ e⁺          T := 𝓫 | fn | rcd | lab

tag(v) = T
--------------- R-chk
⦇T⦈ v ⟶ v


tag(v) ≠ T
--------------- X-chk
⦇T⦈ v ↯chk


=== Where checks go
- C-arg: in T-λ-E, if τ₁′ = ★ and τ₁ has a tag, the argument becomes ⦇tag(τ₁)⦈ e₂⁺
- C-res: every elimination (application, selection, dynamic selection, `or`) whose
  result type has a tag is wrapped in ⦇tag(τ)⦈
  - needed because a function or record that came through ★ may return any value
- tag(𝓫) = 𝓫, tag(τ -> τ′) = fn, tag({ρ}) = rcd, tag(⌊k⌋) = lab; ★ and α have none
- ★-arm eliminations get no check: Nix's own ↯tag there plays the role of ↯chk

=== Theorems (*claim*)
- Tag progress: ∅ ⊢ e: τ ⇝ e⁺ ⟹ e⁺ steps, is a value, ↯, ↯chk, or ↯tag at a
  ★-arm elimination. Never ↯tag at a static elimination
- Tag preservation: for a tag-level typing ⊢ₜ of e⁺ (types collapsed to tags,
  ★ and α to "any"), in the style of Vitousek et al.
- Erasure: ⌊e⁺⌋ = e, and
  - e⁺ ⟶\* v ⟹ e ⟶\* ⌊v⌋
  - e ⟶\* E[r] with r ↯tag ⟹ e⁺ ⟶\* ↯chk, or ↯tag at a ★-arm
- Reading: every Nix type error in a well-typed program is caused by ★ flowing
  into a typed position, or by eliminating ★. The analysis knows all of those sites
- The check bit is never run. An optional *checked mode* would emit the checks
  as Nix (`assert builtins.isAttrs x; …`) to fail earlier; that is the only
  place R2 would be loosened
- Remaining gap, as in transient: a ★-value that flows into a typed position and
  is never eliminated keeps its wrong tag silently. Only entry checks close it
  (pattern lambdas do this natively, see Later)


== ★-safe observers
- Nix has selection forms that never fail; they consume ⊥, ? and ★ without ↯


Γ ⊢ e₁: τ   τ ▷ {ρ}   ρ.l ↓ τ₁   Γ ⊢ e₂: τ₂
------------------------------------------------ T-or-hit
Γ ⊢ e₁.l or e₂: τ₁
// τ₁ = ★ covers both outcomes. τ₁ static: e₂ is dead unless the record came
// through ★ without l, which C-res catches (or is an elimination there)


Γ ⊢ e₁: τ   τ ▷ {ρ}   ρ.l ↓ ⊥   Γ ⊢ e₂: τ₂
------------------------------------------------ T-or-⊥
Γ ⊢ e₁.l or e₂: τ₂


Γ ⊢ e₁: τ   τ ▷ {ρ}   ρ.l ↓ ?   Γ ⊢ e₂: τ₂
------------------------------------------------ T-or-?
Γ ⊢ e₁.l or e₂: ★
// algorithmically a stump with a default: hit gives the field, ⊥ gives τ₂


Γ ⊢ e₁: τ   head(τ) ∈ {𝓫, ->, ⌊·⌋}   Γ ⊢ e₂: τ₂
--------------------------------------------------- T-or-tag
Γ ⊢ e₁.l or e₂: τ₂


Γ ⊢ e: τ
------------------ T-has
Γ ⊢ e ? l: 𝓫\_bool

- No ★-arm is needed: these forms are total on every tag
- With occurrence typing, `e ? l` and `builtins.isAttrs` would refine ★ in a branch (Later)


== Static properties (*claim*)
- Conservative extension: if Γ, e and τ mention neither ★ nor ★ᵣ, then
  Γ ⊢ e: τ without T-sel-★, T-sel-⊥ and ★-arms iff the same holds in L2
  without T-★-intro, T-sel-★, T-sel-⊥ (up to ≈)
  - template: Sekiyama-Igarashi Thm 5.3
- L2 embeds up to precision: Γ ⊢\_L2 e: τ ⟹ ∃ τ′ ⊑ τ. Γ ⊢ e: τ′
  - dropping T-★-intro costs nothing: each use of it either feeds ≃ at an
    application or was the blur ★ ⊒ τ′
- Static gradual guarantee, without annotations: Γ′ ⊑ Γ and Γ′ ⊢ e: τ′
  ⟹ Γ ⊢ e: τ with τ′ ⊑ τ
  - in Nix the only source of precision is Γ (builtins, imports) and the lookups
  - needs: monotonicity of ↓, ≃, ▷ (Precision)
- Witnesses
  - incompleteness §11, `λg. {a = g {l=c}; b = g {m=c}}`: typed without
    T-★-intro, at ★ -> {a: ★ | b: ★} (g: ★) or (★ -> 𝓫) -> {a: 𝓫 | b: 𝓫}
    (g: ★ -> 𝓫, both arguments ≃ ★). The two are ⊑-incomparable (see Principality)
  - `(λx. x.l) d`: typed; e⁺ = (λx. ⦇…⦈ x.l) (⦇rcd⦈ d) when x: {l: 𝓫 | ★ᵣ}, and
    d ⟶ c gives ↯chk, matching Nix's ↯tag on `c.l`
  - the key-blocked spent promise `λr. λa. r.(a) c`: declaratively r.(a): ★ and
    the application goes through M-fn-★. Only the algorithm spends δ (Inference)


// ===================================================================
// Algorithmic part: design notes only, no rules yet
// ===================================================================

== Unification (open)
- Choice still open:
  - ★ consistent, never bound (Siek-Vachharajani): ★ ≐ τ ⇝ ∅. Closest to the current ≐
  - ★ as a fresh gradual variable (Garcia-Cimini): cleaner principal schemes
  - Miyazaki-Sekiyama-Igarashi (bib: gradual\_dti): variables left open by ★ are
    instantiated at run time. Needs casts, so only their analysis carries over
- Rows: ★ᵣ against a spine absorbs a prefix (CE-★ᵣ-L), never bound. Splitting
  through ★ᵣ is SP-★ᵣ, so U-field-L gets a ★ᵣ case that pairs the field with ★
- ★ does not remove the irreducible kinds (Wand, Levi, Shift): none of them
  mentions ★. Only join-on-clash (incompleteness §11 Fix B) would
- The mgu statements become "mgu up to ≃" for ★-free solutions


== Inference (open)
- A-app, A-sel, A-conc use ▷: on ★ take the ★-arm; on α bind α to the shape as in HM
- Argument passing unifies up to ≃ instead of =
- Spent promises: A-app writes δ ≔ τ₂ -> β into a parked result. Declaratively
  δ stays ★ and the application is dynamic. Proposal: record *match constraints*
  δ ▷ β₁ -> β₂ instead of equations; F-★ then turns them into ★-arms, K-hit
  into ordinary unification. F-hit and `LetSpent` may disappear
- `or`: a stump with a default ⟨ρ.l ↓ δ ∥ τ₂⟩; K-hit as before, K-⊥ unifies δ
  with τ₂, F-★ sets δ ≔ ★
- A-let premises: re-check each against ≃


== Principality (open)
- ★ is no longer rigid, so the §11 witness has ⊑-incomparable typings
- Candidate order: ⊴⊑ (covering up to precision), already used for L2
- Literature: Garcia-Cimini, principal type schemes for gradual programs


== FC-labels (open)
- Keyed lookup through ★ᵣ: L-★ᵣ already covers literal and variable keys
- A key of type ★ (`r.(e)` with e: ★): needs a ★-arm for ⌊·⌋. Candidate ρ.(★) ↓ ★;
  a non-label key is ↯ at run time as in L2
- `{ \${e₁} = e₂ }` with e₁: ★: the field's name is unknown, so {★ᵣ}


== Later
- Pattern lambdas `{a, b ? d, ...}:` are entry checks in Nix (strict, exact
  fields unless `...`): closed and open rows checked at function entry
- Occurrence typing: `isAttrs`, `isFunction`, `? l`, `typeOf` refine ★ in branches
- Checked mode: emit the C-arg/C-res checks as Nix asserts
- lib.types / mkOption types as annotations at module boundaries
- Lean, once the static side and safety are stable on paper
