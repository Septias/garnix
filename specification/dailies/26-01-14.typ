
== Lazy & Recursive


















Laziness is an existential nix feature since without it, the package manager would be unpractically slow. This is why it existists in plenty of positions: record-fields, let-bindings, arrays, and patterns.

#figure(
  ```nix
  {
    pattern = { x ? y, y }: x;
    arr = [ [throw a] 1 2 3 ];
    b = (let a = (throw "hi"); b = 2; in b);
    b = rec {a = 2; b = c; c = 2;};
  }
  ```,
  caption: [Examples of Laziness],
)

#figure(
  ```nix
  rec { x = { x = x;};}.x;       # → { x = «repeated»; }
  let x = {x = y;}; y = x; in x  # → { x = «repeated»; }
  ```,
  caption: [Examples of recursive patterns from the nix repl],
)

