
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
  application = map (x: x*x) (map (x: x*x) [1 2 3]);
  let_in = with x; let inherit bracketing; in a + bracketing;
  let_in2 = let a = a + 1; b = 1; in {y = x ; inherit x; c = a + b;};
  let_in3 = let a = x: {inherit setfun;}; in a;
  let_in4 = let inherit x; in {};
  z = let x = {y = 1;}; in with { a = 1;}; [y];
in {
  inherit setfun setIdentFn bracketing ifs ifxlamba string stringMulti t x set setInherit application let_in2;
  inherit let_in let_in3 assertion;
  fun = fun;
  arithm = arithm;
}
