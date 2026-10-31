
./26-10-28.typ
13 Tage
./26-10-30.typ

== Todo
- [x] Remove Materialize, allow top-level schemes
- [x] Check featurestate


== Fragen
- How does F-hit kill principality atm?
  - Materializate and Finalize just close the program with a guess
- Soft typing: True errors don't fail but only give a warning!
- Would "★ only has lookup errors" be a better framing? I'm not sure
  - pro: Closer error-case
  - con: Less programs typable?
- How does ★ fuck with principality?
  - I think because T-★-intro it becomes an order
  - Under this order, we need to prove it
- [x] Where can we note that generalized works well with file-imports?


== Misc
- The problem is that the later parts of my thesis are blocked by the former
- I have not thoroughly checked the second half (Incompleteness, Discussion, Etc.)


== Uses of ★
- As top of hirarchy (for unions aswell by declarative system) (by T-sel-★)
- As lazyness-filler (by T-sel-⊥)
- For uncertainty (by T-sel-★)


== Unrestricted T-★-intro
- Should I drop it?
- Pro: Split one use out of ★
- Just blur, it can act as a join then
- Feels kind of unneccessary
- Better remove it for the sake of principaled top-schemes
- The algorithmic system does not use it (it just adds more typings declaratively, that "abuse" ★)
