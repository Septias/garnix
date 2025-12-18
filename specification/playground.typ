#import "functions.typ": *

${∅, …} ~ overline(d) arrow.squiggly ∅$


$#b[let]/k {overline(#b[nonrce])} #b[in] e ->_μ e[x := k d | x := d ∈ overline(d)]$

== shorthands


#derive(
  "",
  (
    $#b[let]/k {overline(#b[nonrce])} #b[in] e ->_μ e[x := k d | x := d ∈ overline(d)]$,
  ),
  $#b[with]$,
)


== Matchings


== Structure of the introduction
- What is nix? Why is it relevant?
  - [x] Package Manager
- Features of nix / inner workings
  - [x] Reproducibility
  - [x] Upgradability
  - [x] Isolation
  - [x] Rollbacks
  - [x] Garbage Collection
- Overcomming Problems
  - [x] Dependency tracking
- Nix features → Language features
  - [x] Special Constructs
  - [ ] Special primitives (path)
  - [ ] Dynamic Access ({})
  - [x] Meta-properties


#page[
  #bibliography(("bib/misc.bib", "bib/parreaux.bib", "bib/nix.bib"))
]
