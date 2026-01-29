`let x = 2; f = { ${"x"} = 2;}.x; g = (let x = 3; in f); in g`
upward-funarg: `(z: let x = 2; f = y: (x + y + z); in f) 2 2`
downward-funarg: `let f = (x: x 2); y = (y: y + 1); in f y`


