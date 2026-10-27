#import "./functions.typ": *
#set document(
  title: "A Records Calculus with Asymmetric Concatenation for Nix",
  description: "Master thesis about a Soft-Typing Records Calculus with Asymmetric Concatenation for Nix",
  author: "Sebastian Klähn",
  keywords: ("Nix", "Type inference", "Laziness", "Records"),
)

#let title = [A Records Calculus with Asymmetric Concatenation for Nix]

#page(footer: align(
  center,
  "Department of Computer Science - University Freiburg",
))[
  #align(center, text(25pt)[
    #image("assets/logo.png", width: 30%)
    *#title*
    #set text(16pt)

    \

    *Master Thesis* - Sebastian Klähn\
    #text(12pt)[*sebastian.klaehn\@email.uni-freiburg.de*]

    #set text(12pt)

    \

    #text(tracking: 0.5pt)[*Examiner*]: #h(2pt) Prof. Dr. Peter Thiemann \
    #text(tracking: 0.5pt)[*Second Examiner*]: #h(2pt) Prof. Podelski \
    #text(tracking: 0.5pt)[*Advisor*]: #h(2pt) Prof. Dr. Peter Thiemann

    \
  ])
  #align(center)[
    #set par(justify: true)
    #pad(x: 15pt, top: 10pt, bottom: 20pt)[
      = Abstract
      Asymmetric record concatenation with right-precedence is a _set-or-replace operation_ that, given two records, extends the fields of the first record with every unique field of the second and overwrites fields that collide. This operation is a trivial operation in the Nix programming language and admits a canonical example that has no principal type in a row calculus that must decide where a field comes from: the expression `a: b: (a ‖ b).l` concatenates two records of unknown shape, and because of field-precedence and shadowing behaviour, whether l is taken from a or from b is only known once b is instantiated.
      We propose a novel type system for Nix, based upon the work of Paszke and Xie with scoped records, row-variables, asymmetric record concatenation, let-polymorphism, first-class labels, row equivalence and an unknown type ★ that marks the places the analysis cannot decide. We mechanically prove _type safety_ of the declarative system with qualified types in Lean, together with soundness and termination of an incomplete, unification-based inference algorithm.
    ]
  ]
]

#show: template
#show: rule_refs
#set figure(placement: auto)
#set raw(lang: "nix")

// -------------- Sections -------------------

#include "sections/00_introduction.typ"
#include "sections/01_overview.typ"
#include "sections/02_declarative.typ"
#include "sections/03_metatheory.typ"
#include "sections/04_unification.typ"
#include "sections/05_inference.typ"
#include "sections/06_incompleteness.typ"
#include "sections/07_mechanization.typ"
#include "sections/08_extensions.typ"
#include "sections/09_discussion.typ"
#include "sections/10_related-work.typ"
#include "sections/11_conclusion.typ"


// -------------- Bibliography ----------------
#pagebreak()
#bib

// -------------- Appendix --------------------

#set figure(placement: none)
// #outline(target: heading.where(supplement: [Appendix]), title: [Appendix])
#show: appendix
#show figure: set block(breakable: true)

#include "sections/12_appendices.typ"

#page[
  #text(size: 15pt)[*Declaration*]

  I hereby declare that I am the sole author and composer of my thesis and
  that no other sources or learning aids other than those listed have been used.
  Furthermore, I declare that I have acknowledged the work of others by providing
  detailed references of said work.
  I also hereby declare that my thesis has not been prepared for another examination or assignment, either in its entirety or excerpts thereof.

  \
  \

  #grid(
    columns: (1fr, 1fr),
    gutter: 2cm,
    grid(
      rows: 0.3cm,
      columns: 1fr,
      gutter: 7pt,
      "Freiburg den 11.11.2026",
      grid.hline(),
      [
        #v(7pt)
        Place, Date
      ]
    ),
    grid(
      rows: 0.3cm,
      columns: 1fr,
      gutter: 7pt,
      "",
      grid.hline(),
      [
        #v(7pt)
        Signature
      ],
    ),
  )
]
