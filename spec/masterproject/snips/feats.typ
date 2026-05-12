== Reasons for Features:
- FC-Labels: `{a = 2;}.${a}`
- Lazyness: Big structure
- Gradual: Big structure & Impurities
- Record extension: `a // b`
- Reflection: `isBool`
- Intensional poly: reflection
- Ad-hoc poly: operators, reflection
- ml-typinference: Big structure


== Binding types
Dynamic bindig: `${e} = x` \<- this one is more tricky, because it changes the context
Dynamic vars: `t = ${e}` \<- this one is easier because its just a lookup
