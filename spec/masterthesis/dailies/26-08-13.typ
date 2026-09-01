
./26-08-18.typ
== Todo
- Let Claude compare Taros paper to my work
- Kann ich ★ wieder reduzieren?
- Look at dem new examples ../examples/examples.typ
- *T-sel-⊥* ist kompletter Quatsch
  - Actually nein
- Weirdness von Proofs checken
  - Instance-closed Let
    - Wird gebraucht um renaming-machinery zu dodgen
  - T-sel-★ and T-sel-⊥
    - Currently needs principled types¿
  - Relation von ≈ and ⊑
  - Variablen immernoch strings
    - Laut AI erstmal noch lassen
  - Const types C → B
  - Progress can fail even if type ≠ ★


== Fragen
- Was ist das Problem mit ★ elimination und warum löst Principality das?
- Ist let-poly generally broken mit meinen Relations und AI sagts mir nur nicht?
  - wahrscheinlich nicht
- Why does ★ have to be outside of ≈ and can I "fix" that?
  - Hard to fix, it apparently collapses the typesystem
  - *Do ⊑ and ≈ fit together conceptually?*
- Recursive row-lookups due to recursive functions?
- Wofür Canonical Forms?


== Fäden: Formalisierung
- Progress-proof anpassen
- Eliminators for ★
- FC-Labels hinzufügen
- Define the algorithmic version


== Problem
- T-★-intro + T-app-★ makes the declarative system a universal sink (blur `3`, apply it: `3 4` types at ★)
- The up-to-⊑ form alone would in fact be UNPROVABLE at T-λ-E: function domain and argument may
  refine differently, and no rule lifts one refined type to another.
