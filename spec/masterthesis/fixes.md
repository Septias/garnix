

## 00 Introduction

- [~] **E1 G3** [cf E; cr A11]. `00:22` → "The analysis rejects a program only for a definite type error; a lookup it cannot decide is typed at ★." The current "report uncertainty instead of failing" contradicts the fact that a stuck verdict rejects the program. "Totality" is a misnomer. The slogan "never reject working code" is success typing (Lindahl & Sagonas 2006); it is in the bib as `success_typing` but never cited. ★ buys permissiveness only for values that are never eliminated. Measure 09 against the new G3. ("soft" → E2 in 10.)

- [ ] **K P&X once** [cf K; cr B]: `01:13` (absorbs the `01:16` placeholder `[…](more precision?)`), `01:38`.
  - P&X have two different "tail" mechanisms: the declarative tail check ▶* on instantiation (their Fig. 7), and the [Δ₂]ρ = [Δ₁]ρ condition in (Rfield), which is Leijen's termination condition. Name which one you replace (`00:62`, `01:14`, `04:357`).


## 02 Declarative

-  In your own semantics `(x: c) ({}.l)` errors.
- [ ] Small precision errors:
  - `02:133` "every row shape matches exactly one rule": concatenation matches three.
  - `02:141` "θ acts on no part of the row it inspects": it acts on field types.


## 03 Metatheory

- [ ] `03:26` precision order ⊑: cite gradual precision (Siek et al. 2015, already in the bib) [cr B].
- [ ] **Subst Discharge is not closed under substitution** [cr A3]. D-? pins δ to ★. A later θ′ can make the lookup hit, and the instance is gone. Jones's qualified types require entailment to be closed under substitution; this is the real reason @lem-subst needs "θ maps schemes". Say this, and say which θ satisfy the hypothesis. Otherwise @lem-subst rests on an assumption that is never discharged.


## 04 Unification

- [ ] Blocked: Adjust mentioning of "forced"

## 05 Inference

- [ ] **F Opening** `05:17` [cf F]: pipeline and why its order is forced (your comment `05:285`). Uses "forced" → Forced.
- [ ] **J Vocabulary and Solver State** [cf J; cr C]. Vocabulary drifts: stump / pending lookup / promise (never defined) / field demand / row demand / qualifier. Fix one set. Heading "Solver State" at `05:18` defines stump, parked, blocker, stale, quiescent, saturation, spent (spent is used at `05:175`, also a placeholder, before it is defined). Drop promise, field demand, qualifier (PROF `05:288`). "Promise", "Spent" and "Stuck" appear in titles before they are defined: rename heading `06:55` and the example "A spent promise".
- [ ] `05:167` "No equation mentioning l is ever emitted" → Forced. `05:167` `@[cite]` placeholder [cr C].
- [ ] Notation [cr C]: ↝^{*!} is undefined (`05:66`); Δ₁ \\ Δ in the text vs Δ₁ ∖ Δ_q in the figure (`05:171`); [γ → ε] vs ≔.
- [ ] `05:293` PROF → C. `05:294` "not a forced move" → Forced.

## 06 Incompleteness

- [ ] **C6 Completeness and "classify"** [cf C6; cr A8]. Contribution 4 `00:65-66`, 06 intro `06:6`, conclusion `11:6` ("the reason is delimited") assert a classification, but no exhaustiveness result exists.
  - The ★ gap is not a delimited class (every join that is never eliminated falls into it), and gx shows a non-principal answer from a run that succeeds.
  - Other cases outside the three classes: `08:186` reports an occurrence next to a keyed field as stuck because "the proof does not extend", yet α ≐ (${β}:τ | α) has no unifier by the same length argument.
  - Soundness plus termination is also satisfied by an algorithm that rejects everything. Either define completeness as "typable ⇒ run succeeds" (then the classification survives), or give at least one relative completeness result, e.g. HM-completeness on the record-free fragment, or "Fix A" from feature-state.
- [ ] **Unif Incompleteness terminology** [cr A7].
  - `06:10` "prime example of undecidability": nothing here is undecidable. Use Baader-Snyder unification types: Wand is finitary (n incomparable unifiers, a finite complete set); Shift is infinitary. Shift is Plotkin's 1972 x·a = a·x, which should be cited.
  - `06:21` "Every algorithm that decides this problem has to backtrack" is false. One can return the complete set, as Wand did.
  - Expect the question whether satisfiability modulo ≈ is decidable. Word equations in trace monoids are (Diekert, Matiyasevich, Muscholl 1999); the types inside your letters complicate this, so say that.
  - (`00:65` "single unifier" → 00. ≈ counting shadowed fields → Adm in 09.)
- [ ] **C2** `06:66` [cf C2]: "inference that joins incompatible lower bounds at ★" does not close the gap: `g: x: {a = g x; b = g {l=c};}` never clashes.
- [ ] `06:29` lacks-predicates: cite Rémy's presence/absence flags as the classical source of negative information (also `11:8`). The cross-ref to @sec-extensions points nowhere; 08 has no lacks-predicates [cr B, C].
- [ ] `06:53` "inherent to let-polymorphism": the ownership condition is specific to your algorithm. `06:53`/`06:56` use first-class labels before §8 introduces them [cr C].
- [ ] `06:55` heading rename → J.
- [ ] `06:58` "incompleteness rather than rejection" → "rather than unsoundness" [cr C].
- [ ] `06:38` your comment on the trailing host (kept on purpose) [cf].
- [ ] Typos [cr C]: "limmited" (`06:6`); "can not" (`06:10`, `06:21`); `06:21` has an unfinished sentence.

## 07 Mechanization

- [ ] **Mech The mechanized system is not the presented one** [cr A9] (`07:6`).
  - The sorts are absent from the Lean declarative relation. Safety transfers if paper ⊆ Lean, but the no-plain and selQ principality results depend on exactly which typings exist, so they do not transfer automatically. State per theorem what transfers.
  - §7 has three paragraphs for roughly 36k lines of Lean, the headline contribution of the abstract. Add size, structure, trusted base, a paper↔Lean table, what is executable, and the fuzzing.
- [ ] Wording [cr C]: `07:4` "trivial features" → "routine" (as `12:6` already says); "Mininix", "conditionnals" (`07:4`); `07:6` "differentiate" → "deviate".

## 08 Extensions

- [~] **D Computed string keys** [cf D; cr A13] (`08:8` PROF). Nix has no labels, only strings. Computed keys give ★, so `r // { ${n} = v; }` is rejected, and `08:252` "interact as NixLang programmers expect" holds only for literal keys. Fix: type a string key as a fresh, never-solved key variable instead of ★ (needs metatheory check). Minimum: state the limitation in the FC-labels intro and in G3 (E1).
- [ ] **↯-key** [cr A13]: ↯-key makes {ξ}.${c} with a string constant a runtime error in MiniNix (`08:131`), while it succeeds in Nix. Progress up to ↯ therefore absorbs non-errors.
- [ ] **Order A-sel-dyn-𝓫 order dependence** [cr A13], check in Lean whether A-ξ-conc infers fields left to right. `a: { x = a + 1; y = {l = c}.${a}; }` types (with ★); with the fields swapped, it clashes. This is the same order dependence `08:248` uses to argue against the alternative.
- [ ] **C1** `08:144` "These two cases rely on T-★-intro being unrestricted" [cf C1; cr A13]: those cases cannot happen (the preservation cases are vacuous).
- [ ] `08:186` "the proof does not extend" → C6.
- [ ] Cross-refs: @sec-extensions has no lacks-predicates (`06:29`, `11:8`) and no occurrence typing (`10:43`; only §12 mentions it). Add them or retarget the refs [cr C].
- [ ] Wording [cr C]: `08:10` is garbled ("x.${} selects an arbitrary string expression", "bind variables") and has "can not"; "otherways", "the definite answer are" (`08:59`); "of₂'s type" (`08:140`).

## 09 Discussion

- [ ] **Adm Admissions section** `09:57` (TODO) [cf Adm; cr A7]. ★ has no eliminators (lazy gradual typing as outlook). ≈ keeps shadowed fields, unlike Nix's set-like records, and the faithful relation is not cancellative. Shift and U-ground's counting depend on ≈ counting shadowed fields; under observational equality all n ≥ 1 collapse, so also admit in §2 that ≈ is intensional.
- [ ] **C3** `09:60` [cf C3]: consistency "would allow inference to join incompatible uses" and "would complete the conditional": consistency never relates two static types.
- [ ] **C4** `09:70` [cf C4]: "whether qualified schemes remain principal under consistency is open": principality already fails in ⊢ today.
- [ ] **Ex Discussion examples** [cr C].
  - The idioms are altered: hello vs fields/foo, and `app args.buildInputs` instead of `++ [extra]`. Say so.
  - Say what (★ → β) means in practice: `app` can only be a function that ignores its argument or passes it on unused (e.g. `y: y`); `xs: xs ++ [extra]` clashes.
  - `09:42` "type in agreement": give N/M.
- [ ] Measure 09 against the new G3 → E1.
- [ ] `09:18` PROF → C. `09:32` optionalAttrs → 00. `09:35` PROF → B. `09:40`, `09:55` → 01.

## 10 Related Work

- [~] **E2 "Soft"** [cf E; cr A12]. "soft" is still at `10:46` ("Gradual and soft typing") and `10:49-51` (+ PROF `10:48`: narrower sense than Cartwright-Fagan, define it or drop it). By your own account, the system has neither defining feature: it does not accept every program and does not insert checks. G2 is Bracha's pluggable-types lineage. Drop "soft" (also `02:91`).
- [ ] **C9** `10:46` marker `[…](check!)` on "NixLang programs cannot be instrumented" [cf C9; cr A12]: they can. G2 rules out instrumentation; the programs themselves could be instrumented.
- [ ] **C5** `10:40` "subtyping-free" [cf C5; cr A5]: T-★-intro is subsumption into a top type. Say instead that the Cardelli-Mitchell conflict is avoided because ★ is not a record and so cannot forget a field. Also `10:40` calls ≈ an "order"; it is an equivalence [cr C].
- [ ] `10:53` "the same pragmatic choice of an unsound any" contradicts your own safety theorem → ★.
- [ ] `10:37` "dual to HM(X)" → Forced.
- [ ] `10:28` [cr B]: "set-or-replace is out of reach" for P&X contradicts `00:16`. "Leijen later added first-class labels": the 2004 TR predates the 2005 scoped-labels paper.
- [ ] `10:43` "types are not reconstructed": Castagna, Laurent, Nguyễn (POPL 2024, "Polymorphic Type Inference for Dynamic Languages") reconstruct types without annotations. Not in the bib [cr B].
- [ ] Missing related work [cr B]:
  - Théophane Hufschmitt's 2017 work on typing Nix (with Castagna; NixCon "Typing Nix", the prototype tix). Verify first. If it holds, `00:14` is wrong.
  - Rémy's presence/absence flags (→ `06:29`, `11:8`).
  - Gradual precision, Siek et al. 2015 (→ `03:26`).

## 11 Conclusion

- [ ] `11:6` "forced" → S7; "the reason is delimited" → C6.
- [ ] `11:8` "gradual reading without casts is the natural next step": your own finding says it becomes vacuous with implicit ★ (`(x: x 4) 3` types and gets stuck). Tone it down [cr C]. Lacks-predicates cross-ref and Rémy → `06:29` item.

## 12 Appendices

- [ ] **C7** `12:30` and `figures/nix-parity.typ:55` [cf C7]: `==` also fails on atoms of different kinds (`1 == true`), which Nix evaluates to false.
- [ ] `12:15` "sound signature" has no δ-progress law, so a builtin failing on well-typed arguments counts as an allowed error (open item in feature-state) [cr C].
