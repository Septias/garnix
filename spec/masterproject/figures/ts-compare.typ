

#let ts-compare = table(
  columns: (auto, 1fr, 1fr, 1fr),
  table.header([*Property*], [*Castagna*], [*Parreaux*], [*Dolan*]),
  [Extensibility], [●], [ ], [●],
  [Principality], [ ], [●], [●],
  [Effectivity], [ ], [●], [●],

  table.cell(colspan: 4)[ Connectives ],
  [Negation Types], [●], [●], [ ],
  [Union], [●], [●], [●],
  [Intersection], [●], [●], [●],
  [Polar Restriction], [ ], [ ], [●],

  table.cell(colspan: 4)[ Advanced Features ],
  [Occurrence], [●], [●], [ ],
  [Gradual], [●], [ ], [ ],
  [First class labels], [●], [ ], [ ],
  [Reflection], [●], [ ], [ ],
  [Singleton types], [●], [ ], [ ],

  table.cell(colspan: 4)[Records],
  [Untagged records], [●], [ ], [●],
  [Record delete], [●], [●], [ ],
  [Record add], [●], [●], [ ],
  [Record concat], [●], [●], [ ],

  table.cell(colspan: 4)[Polymorphism],
  [Subtyping poly.], [●], [●], [●],
  [Parametric poly.], [●], [●], [ ],

  table.cell(colspan: 4)[Misc],
  [Lazy], [ ], [ ], [ ],
  [Recursive Types], [●], [●], [ ],
)
