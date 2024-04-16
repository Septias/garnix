
{ }: let
  fun = x: x;
  arithm = x: y: (x + y) * 3;
  ifs =
    if true
    then 1
    else 2;

  ifxlamba = x: y: z:
    if y
    then x else z;

  x = { a = 1;};
  setfun = {...}: {};
  setInherit = {}: {inherit x;};
  setIdentFn = x: {y}: x + y;
  set = x: x.x < 1;
  string = "
  aa
  ";
  stringMulti = ''
  aaaa
  bbbb
  '';
  t = .0;
  bracketing = (1 + 2) * 3;
  assertion = x: assert 1; 1;
in {
  inherit setfun setIdentFn bracketing ifs ifxlamba string stringMulti t x set setInherit application let_in2;
  inherit let_in let_in3 assertion;
  fun = fun;
  arithm = arithm;
};
