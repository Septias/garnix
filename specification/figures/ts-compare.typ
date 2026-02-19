

#let ts-compare = table(
  columns: (auto, 1fr, 1fr, 1fr),
  align: (left, center, center, center),
  table.header([*Property*] , [*Castagna*]  , [*Parreaux*]  , [*Dolan*],),
  [*Extensible*]            , [●]           , [ ]           , [●]      ,
  [*Principal*]             , [ ]           , [●]           , [●]      ,
  [*Effective*]             , [ ]           , [●]           , [●]      ,

  table.cell(colspan: 4)[*Connectives*],
  [*Negation types*]        , [●]           , [●]           , [ ]      ,
  [*Union*]                 , [●]           , [●]           , [●]      ,
  [*Intersection*]          , [●]           , [●]           , [●]      ,
  [*Polar Restriction*]     , [ ]           , [ ]           , [●]      ,

  table.cell(colspan: 4)[*Advanced Features*],
  [*Occurrence*]            , [●]           , [●]           , [ ]      ,
  [*Gradual*]               , [●]           , [ ]           , [ ]      ,
  [*First class labels*]    , [●]           , [ ]           , [ ]      ,
  [*Reflection*]            , [●]           , [ ]           , [ ]      , 
  [*Null-safety*]           , [ ]           , [ ]           , [ ]      ,

  table.cell(colspan: 4)[*Records*],
  [*Record delete*]         , [●]           , [●]           , [ ]      ,
  [*Record add*]            , [●]           , [●]           , [ ]      ,
  [*Record concat*]         , [●]           , [●]           , [ ]      ,

  table.cell(colspan: 4)[*Polymorphism*],
  [*Subtyping poly.*]       , [●]           , [●]           , [●]      ,
  [*Parametric poly.*]      , [●]           , [●]           , [ ]      ,

  table.cell(colspan: 4)[*Misc*],
  [*Lazy*]                  , [ ]           , [ ]           , [ ]      ,
  [*Recursive Types*]       , [●]           , [●]           , [ ]      ,
)
