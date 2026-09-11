./26-09-11.typ


== Todo
- Check (`occurs_allVar_hasMgu`, `stuck_masks_mgu`, `terminal_masks_mgu`)
- Document unification quirks
- Mechanize the inference part


== Read
- [x] Mechanized Examples
- [x] Algorithm.typ


== Fragen
- Soll ich das fuel outcome aus dem Thesis-text raus lassen?
  - Ist erstmal fine


== Claude Output
(a) types every program, (b) requires no source or semantic change, (c) runs at repo scale, (d) is honest about where it gave up. Every design decision follows from one of those four: scoped rows from (a) — totality of \/\/; ★ from (a)+(d); no subtyping from (a) — width subtyping destroys the precedence information concat needs; unification-only from (c); soft rather than gradual from (b). If §1 said this and §2 proved it, the two sections would be done.
