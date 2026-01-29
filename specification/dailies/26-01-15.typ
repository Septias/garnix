
== First class values
Commonly used representations include De Bruijn indices [15], nominal syntax [21], higher-orderabstract syntax [48] and locally nameless [13]. The strength of these representations is that they


== Weirdness
- appendContext is not in builtin-list
- builtins.elemAt
- \@ args does not include any default values specified with ? in the function's set pattern.
- __toString
- An attribute set also interpolates to the value of its outPath attribute.
- Attribute sets and lists are compared recursively, and therefore are fully evaluated.
