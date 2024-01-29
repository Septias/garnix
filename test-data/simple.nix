# Set lambda
x: let
  fun = x: x;
  arithm = x: y: (x + y) * 3;
  ifs =
    if true
    then 1
    else 2;

  ifxlambad = x: y: z:
    if y
    then x
    else z;

  setfun = {}: {};
  setIdentFn = x: {y}: x + y;

  bracketing = (1 + 2) * 3;
in {
  inherit setfun setIdentFn bracketing ifs ifxlambad;
  fun = fun;
  arithm = arithm;
}
