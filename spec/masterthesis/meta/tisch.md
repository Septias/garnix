
## Depth
- [ ] Still only on *closed terms*
- [x] Remove L-α-lookup
- [x] Instance-closed Let
- [x] U-expand removed

## Related Work: open checks
- [ ] Soft typing: verify "Cartwright and Fagan accept every program and insert run-time checks" against @soft_typing and @practical_soft_typing (ACM paywall, add PDFs to bib/plaintext/)
- [ ] AC-unification NP-complete: verify against Kapur-Narendran @ac_unification (Springer paywall)
- [ ] Sulzmann @designing_record_systems (Yale RR-1128): check what the report actually provides; Leijen says it gives "an effective type inference method"
- [ ] Wells @undecidable and Jones @qualified_types: not checked against the source
- [ ] TypeScript @typescript and Flow @flow: claim about unsound `any` rests on general knowledge
- [ ] Own claim, no source: "NixLang programs cannot be instrumented, so there are no casts, no blame, and no runtime monitoring" (still marked check!)


## Advancements
- Port our testsuite from Mini to Macro-Nix


## Fortführende Ideen
- Kann man die Providenz von ★ besser tracken?
- Automatische Instrumentation?
- Verbindung von ★ und bi-unification?
