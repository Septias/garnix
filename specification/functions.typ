#import "@preview/biceps:0.0.1": flexwrap

// ------------- Colors
#let red_700 = rgb(185, 28, 28)
#let red_700 = rgb(185, 28, 28)
#let orange_500 = rgb(249, 115, 22)
#let red = oklch(42.1%, 0.095, 57.708deg);
#let zink_700 = oklch(44.2%, 0.017, 285.786deg)
#let zink_900 = oklch(21%, 0.006, 285.885deg)


// ------------- Shorthands
#let oi(body) = $overline(body)^i$
#let oj(body) = $overline(body)^j$
#let b(body) = text(weight: "bold", body)
#let rotate(..body) = $attach(tr: diamond.small, ..body)$
#let todo(..body) = rect(stroke: orange_500, radius: 2pt, ..body)


// -------------  Syntax Shorthands
#let str = "str"
#let null = "null"
#let drv = "drv"
#let path = "path"
#let bool = "bool"
#let int = "int"
#let float = "float"
#let number = "float"
#let openPat = $⦃oi(l_i : τ_i)⦄^◌$
#let recordType = ${oi(l_i : τ_i)}$
#let record = ${oi(l_i \= t_i\;)}$
#let label = $l$
#let manyTypes = $[oi(τ_i)]$
#let oα = $overline(α)$


// -------------  Bigger Constructs

// For example: Boolean
#let type_name(name) = [
  #text(fill: zink_700, style: "italic", size: 9pt, name)
  #v(10pt)
]

// For example: T-Rec
#let rule_name(name) = [
  #text(fill: zink_900, size: 9pt, smallcaps(name))
  #v(10pt)
]


#let boxed_type_rules(..body) = block(
  inset: 20pt,
  flexwrap(
    main-spacing: 20pt,
    cross-spacing: 10pt,
    ..body,
  ),
)

// Logical unit in a bigger figure
#let subbox(caption: "", ..body) = stack(
  spacing: 10pt,
  align(left, text(weight: "bold", smallcaps(caption))),
  ..body,
)

// Rules underlying one syntax
#let subrules(caption: math, ..body) = block(breakable: true, stack(
  spacing: 10pt,
  align(left, rect()[#caption]),
  ..body,
))

#let many_wrapping_derives(caption: "", ..body) = stack(
  spacing: 10pt,
  align(left, text(
    weight: "bold",
    smallcaps(caption),
  )),
  flexwrap(main-spacing: 20pt, cross-spacing: 10pt, ..body),
)


#let derive(name, prem, conclusion) = [
  #table(
    stroke: none,
    inset: (x: 0pt, y: 5pt),
    align: center,
    table.cell(align: start, rule_name(name)),
    table.cell(inset: (y: 5pt), prem.join("     ")),
    table.hline(),
    table.cell(inset: (y: 10pt), conclusion),
  )
]

// ------------------- BIB
#let bib = page(bibliography(
  (
    "./bib/misc.bib",
    "./bib/parreaux.bib",
    "./bib/nix.bib",
    "./bib/castagna.bib",
    "./bib/gradual.bib",
  ),
  style: "association-for-computing-machinery",
))
