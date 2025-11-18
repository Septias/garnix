// #colored_box(title: "Evaluation Rules", color: blue)[
//   #stack(
//     dir: ltr,
//     spacing: 1cm,
//     derive(
//       "F-Force-Step",
//       ($⟨a, H[a -> e]⟩$, $⟨e, H⟩ -> ⟨e', H'⟩$),
//       $⟨a, H⟩ -> ⟨a, H'[a -> e']⟩$,
//     ),
//     derive("F-Force-Value", ($H[a -> v]$, $v: "Value"$), $⟨a, H⟩ -> ⟨v, H⟩$),
//   )
// ]
