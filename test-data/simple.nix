# Set lambda

{path }: let
  fun = x: x;
  arithm = x: y: (x + y) * 3;
  ifs =
    if true
    then 1
    else 2;

  ifxlamba = x: y: z:
    if y
    then x else z;

  setfun = {}: {};
  setIdentFn = x: {y}: x + y;
  set = x: x.0 <- 0;
  string = "
  
  ";
  stringMulti = ''
  aaaa**->aa🙈
  bbbb
  '';
  t = .0;
  /* */
  bracketing = (1 + 2) * 3;
in {
  inherit setfun setIdentFn bracketing ifs ifxlamba string stringMulti t;
  fun = fun;
  arithm = arithm;
}
