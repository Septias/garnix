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
${∅, …} ~ overline(d) arrow.squiggly ∅$
#derive(
  "",
  (
    ${oi(e?), …} ~ overline(d) arrow.squiggly overline(α)$,
    $x ∉ "dom"(oi(e?))$,
    $x ∉ "dom"(overline(d))$,
  ),
  ${oi(e?<x := e^?>), …} ~ overline(d)<x := d> arrow.squiggly overline(α)<x := "nonrec" d>$,
)
#derive(
  "",
  (
    ${oi(e?), …} ~ overline(d) arrow.squiggly overline(α)$,
    $x ∉ "dom"(oi(e?))$,
    $x ∉ "dom"(overline(d))$,
  ),
  ${oi(e?)<x := "Some"(e)^?>, …} ~ overline(d) arrow.squiggly overline(α)<x := "rec" e>$,
)
#derive(
  "",
  (
    ${oi(e?), …} ~ overline(d) arrow.squiggly overline(α)$,
    $"dom"(overline(d)) subset.eq "dom"(oi(e?))$,
  ),
  ${oi(e?<x := e^?>), …} ~ overline(d)<x := d> arrow.squiggly overline(α)<x := "nonrec" d>$,
)


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
