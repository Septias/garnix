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

  x = {};
  setfun = {...}: {};
  setInherit = {}: {inherit x;};
  setIdentFn = x: {y}: x + y;
  settest = x.${x.y or ""};
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
  application = fun fun builtins.map  (1 + 1) 2+2;
  let_in = with 1+1; let x = 1; in 1+1;
in {
  inherit setfun setIdentFn bracketing ifs ifxlamba string stringMulti t x set setInherit settest;
  fun = fun;
  arithm = arithm;
}
