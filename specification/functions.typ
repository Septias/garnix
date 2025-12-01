#import "@preview/biceps:0.0.1": flexwrap

// colors
#let red_700 = rgb(185, 28, 28)
#let red = oklch(42.1%, 0.095, 57.708deg);
#let zink_700 = oklch(44.2%, 0.017, 285.786deg)
#let zink_900 = oklch(21%, 0.006, 285.885deg)

// shorthands
#let oi(body) = $overline(body)^i$
#let oj(body) = $overline(body)^j$
#let b(body) = text(weight: "bold", body)
#let rotate(..body) = $attach(tr: diamond.small, ..body)$
#let todo(..body) = rect(fill: red_700, ..body)

// bigger constructs
#let boxed_type_rules(..body) = rect(inset: 20pt, flexwrap(
  main-spacing: 20pt,
  cross-spacing: 10pt,
  ..body,
))

#let subbox(caption: "", ..body) = stack(
  spacing: 10pt,
  align(left, text(weight: "bold", smallcaps(caption))),
  ..body,
)

#let subrules(caption: math, ..body) = box(width: 100%, stack(
  spacing: 10pt,
  align(left, rect()[#caption]),
  ..body,
))

#let sub_typing_rules(caption: "", ..body) = stack(
  spacing: 10pt,
  box(width: 100%, align(left, text(weight: "bold", smallcaps(caption)))),
  flexwrap(main-spacing: 20pt, cross-spacing: 10pt, ..body),
)

#let type_name(name) = [
  #text(fill: zink_700, style: "italic", size: 9pt, font: "NotoSerif NF", name)
  #v(10pt)
]
#let rule_name(name) = [
  #text(fill: zink_900, size: 9pt, font: "NotoSerif NF", smallcaps(name))
  #v(10pt)
]


#let derive(name, prem, conclusion) = [
  #table(
    stroke: none,
    inset: (x: 0pt, y: 5pt),
    align: center,
    table.cell(align: start, rule_name(name)),
    table.cell(inset: (y: 5pt), [#prem.join("     ")]),
    table.hline(),
    table.cell(inset: (y: 10pt), [#conclusion]),
  )
]

