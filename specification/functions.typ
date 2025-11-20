#import "@preview/biceps:0.0.1": flexwrap

// colors
#let red_700 = rgb(185, 28, 28)
#let red = oklch(42.1%, 0.095, 57.708deg);

// shorthands
#let oi(body) = $overline(body)^i$
#let oj(body) = $overline(body)^j$
#let b(body) = text(weight: "bold", body)

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

#let sub_typing_rules(caption: "", ..body) = stack(
  spacing: 10pt,

  box(width: 100%, align(left, text(weight: "bold", smallcaps(caption)))),
  flexwrap(main-spacing: 20pt, cross-spacing: 10pt, ..body),
)

#let type_name(name) = text(size: 9pt, fill: black, weight: "bold", name)
#let rule_name(name) = [
  #text(
    // fill: if name.starts-with("T") { maroon } else if name.starts-with("S") {
    //   purple
    // } else if name.starts-with("C") { orange } else if name.starts-with("R") {
    //   blue
    // } else { black },
    weight: "bold",
    smallcaps(name),
  )
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

