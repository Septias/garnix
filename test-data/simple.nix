
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

  x = {};
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
  application = map (x: x*x) (map (x: x*x) [1 2 3]);
  let_in = with 1+1; let inherit bracketing; x = 1; in 1+1;

  let_in2 = let a = a + 1; b = 1; in {y = x ; inherit x;};
in {
  inherit setfun setIdentFn bracketing ifs ifxlamba string stringMulti t x set setInherit application let_in2;
  fun = fun;
  arithm = arithm;
}
