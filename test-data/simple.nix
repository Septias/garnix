let 
  fun = x: x;
  arithm = x: y: (x + 1);
  ifs =
    if true
    then "hi"
    else 2;
  ifxlamba = x: y: z:
    if y
    then x else z;
  access = x: x.y;
  arr1 = [1 1 6];
  arr2 = [1 "h1" ./.];
  player = 12;
  setfun = {...}: null;
  position = 12 * 11; 
  name = "bob"; 
  pos = {x = 1; y = 2;}; 
  setInherit = {}: {inherit pos;};
  setIdentFn = x: {y}: x + y;
  set = x: x.x < 1;
  assertion = x: assert 1; 1;
  f = { x, ... }: x;
  res = f {x = 1; y = 2;};
  w1 = with {y = 1;}; {z = y;};
  opt = x: x ? y;
  in {  }