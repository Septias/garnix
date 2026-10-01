
./26-09-29.typ
./26-10-01.typ


== State: Fäden
- Remove as much incompleteness as possible
- Slowly expand the typesystem with features
- Write the sections one after another


== Misc
- Sadly, the deciding example got longer now
- Yannik für peer-review fragen
- Put thesis into ai analysis


== Cost of FC-Labels
- Costs:
  - a non-label key is a type error (`{foo = c}.(c)`, `{${c} = c}` clash), was ★ + W-flag
  - keyed fields are barriers: `(foo: τ) ≐ᵣ (${α}: τ′)` and different unknown keys are stuck; keyed Fuzz universe 67% stuck
- U-key: same unknown key at the head/tail of both spines → `τ ≐ τ′`, continue (`matchL` arm, `RowEquiv.dsing_cancel_left`)
