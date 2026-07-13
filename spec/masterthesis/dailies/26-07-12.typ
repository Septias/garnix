
./26-07-13.typ
== Todo
- [x] No T-var und T-rec
- [x] Lookup miss explizit machen
- [x] Lookup von α fehlt
- [x] Row-equality definieren
- [x] Normal progress does not work (★-typed can get stuck)
  - resolved in minimal.lean: added `Err` (lookup-error + eval contexts), progress = step ∨ value ∨ err; all proofs sorry-free

== Fragen
- Soll ich einmal meine Novelties im Vergleich zu Tabulardata aufschreiben?
  - Joa, passt
- No subtyping so especially no record depth-subtyping
  - Width subtyping provided by row poly


== Claude comments
- You should distinguish two situations: in a plain extensible-row position, LUtail-style solving is sound and gives you real row polymorphism; ★ should be reserved for the genuinely ambiguous case — a variable in shadowing position ahead of a needed label. Right now minimal.typ conflates them and loses precision the paper's algorithm gets for free.

- They have a restriction operator \, you don't. Their scoped labels stay ergonomic because rejected programs can be rewritten with explicit restriction (f l x = (x\l).foo), and shadowed fields remain reachable. Nix does have removeAttrs, so adding restriction to your calculus is both faithful to the language and would give your users the same escape hatch when ∈-solving fails.
