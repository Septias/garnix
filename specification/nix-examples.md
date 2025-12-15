### eval-fail-assert-equal-function-direct.nix
```nix
# Note: functions in nested structures, e.g. attributes, may be optimized away by pointer identity optimization.
# This only compares a direct comparison and makes no claims about functions in nested structures.
assert
  (x: x)
  ==
  (x: x);
abort "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '((x: x) == (x: x))'
         at /pwd/lang/eval-fail-assert-equal-function-direct.nix:3:1:
            2| # This only compares a direct comparison and makes no claims about functions in nested structures.
            3| assert
             | ^
            4|   (x: x)

       error: distinct functions and immediate comparisons of identical functions compare as unequal

```

### eval-fail-bad-string-interpolation-2.nix

```nix
"${./fnord}"
```

#### Expected 

```text
error: path '/pwd/lang/fnord' does not exist

```
### eval-fail-bad-string-interpolation-4.nix

```nix
let
  # Basically a "billion laughs" attack, but toned down to simulated `pkgs`.
  ha = x: y: { a = x y; b = x y; c = x y; d = x y; e = x y; f = x y; g = x y; h = x y; j = x y; };
  has = ha (ha (ha (ha (x: x)))) "ha";
  # A large structure that has already been evaluated.
  pkgs = builtins.deepSeq has has;
in
# The error message should not be too long.
''${pkgs}''
```

#### Expected 

```text
error:
       … while evaluating a path segment
         at /pwd/lang/eval-fail-bad-string-interpolation-4.nix:9:3:
            8| # The error message should not be too long.
            9| ''${pkgs}''
             |   ^
           10|

       error: cannot coerce a set to a string: { a = { a = { a = { a = "ha"; b = "ha"; c = "ha"; d = "ha"; e = "ha"; f = "ha"; g = "ha"; h = "ha"; j = "ha"; }; «8 attributes elided» }; «8 attributes elided» }; «8 attributes elided» }

```
### eval-fail-blackhole.nix

```nix
let {
  body = x;
  x = y;
  y = x;
}
```

#### Expected 

```text
error:
       … while evaluating the attribute 'body'
         at /pwd/lang/eval-fail-blackhole.nix:2:3:
            1| let {
            2|   body = x;
             |   ^
            3|   x = y;

       error: infinite recursion encountered
       at /pwd/lang/eval-fail-blackhole.nix:3:7:
            2|   body = x;
            3|   x = y;
             |       ^
            4|   y = x;

```
### eval-fail-foldlStrict-strict-op-application.nix

```nix
# Tests that the result of applying op is forced even if the value is never used
builtins.foldl'
  (_: f: f null)
  null
  [ (_: throw "Not the final value, but is still forced!") (_: 23) ]
```

#### Expected 

```text
error:
       … while calling the 'foldl'' builtin
         at /pwd/lang/eval-fail-foldlStrict-strict-op-application.nix:2:1:
            1| # Tests that the result of applying op is forced even if the value is never used
            2| builtins.foldl'
             | ^
            3|   (_: f: f null)

       … while calling anonymous lambda
         at /pwd/lang/eval-fail-foldlStrict-strict-op-application.nix:3:7:
            2| builtins.foldl'
            3|   (_: f: f null)
             |       ^
            4|   null

       … from call site
         at /pwd/lang/eval-fail-foldlStrict-strict-op-application.nix:3:10:
            2| builtins.foldl'
            3|   (_: f: f null)
             |          ^
            4|   null

       … while calling anonymous lambda
         at /pwd/lang/eval-fail-foldlStrict-strict-op-application.nix:5:6:
            4|   null
            5|   [ (_: throw "Not the final value, but is still forced!") (_: 23) ]
             |      ^
            6|

       … while calling the 'throw' builtin
         at /pwd/lang/eval-fail-foldlStrict-strict-op-application.nix:5:9:
            4|   null
            5|   [ (_: throw "Not the final value, but is still forced!") (_: 23) ]
             |         ^
            6|

       error: Not the final value, but is still forced!

```

### eval-fail-infinite-recursion-lambda.nix

```nix
(x: x x) (x: x x)
```

### eval-fail-recursion.nix

```nix
let a = {} // a; in a.foo
```

#### Expected 

```text
error:
       … in the right operand of the update (//) operator
         at /pwd/lang/eval-fail-recursion.nix:1:12:
            1| let a = {} // a; in a.foo
             |            ^
            2|

       error: infinite recursion encountered
       at /pwd/lang/eval-fail-recursion.nix:1:15:
            1| let a = {} // a; in a.foo
             |               ^
            2|

```

### eval-fail-scope-5.nix

f = {x ? y, y ? x}: x + y;


### eval-okay-attrnames.nix

```nix
with import ./lib.nix;

let

  attrs = {y = "y"; x = "x"; foo = "foo";} // rec {x = "newx"; bar = x;};

  names = builtins.attrNames attrs;

  values = map (name: builtins.getAttr name attrs) names;

in assert values == builtins.attrValues attrs; concat values
```

#### Expected 

```text
"newxfoonewxy"

```

### eval-okay-attrs6.nix

```nix
rec {
  "${"foo"}" = "bar";
   __overrides = { bar = "qux"; };
}
```

#### Expected 

```text
{ __overrides = { bar = "qux"; }; bar = "qux"; foo = "bar"; }

```

### eval-okay-backslash-newline-1.nix

```nix
"a\
b"
```

#### Expected 

```text
"a\nb"

```

### eval-okay-callable-attrs.nix

```nix
({ __functor = self: x: self.foo && x; foo = false; } // { foo = true; }) true
```

#### Expected 

```text
true

```
### eval-okay-deepseq.nix

```nix
builtins.deepSeq (let as = { x = 123; y = as; }; in as) 456
```

#### Expected 

```text
456

```
### eval-okay-delayed-with.nix

```nix
let

  pkgs_ = with pkgs; {
    a = derivation {
      name = "a";
      system = builtins.currentSystem;
      builder = "/bin/sh";
      args = [ "-c" "touch $out" ];
      inherit b;
    };

    b = derivation {
      name = "b";
      system = builtins.currentSystem;
      builder = "/bin/sh";
      args = [ "-c" "touch $out" ];
      inherit a;
    };

    c = b;
  };

  packageOverrides = pkgs: with pkgs; {
    b = derivation (b.drvAttrs // { name = "${b.name}-overridden"; });
  };

  pkgs = pkgs_ // (packageOverrides pkgs_);

in "${pkgs.a.b.name} ${pkgs.c.name} ${pkgs.b.a.name}"
```

#### Expected 

```text
"b-overridden b-overridden a"

```
### eval-okay-dynamic-attrs-bare.nix

```nix
let
  aString = "a";

  bString = "b";
in {
  hasAttrs = { a.b = null; } ? ${aString}.b;

  selectAttrs = { a.b = true; }.a.${bString};

  selectOrAttrs = { }.${aString} or true;

  binds = { ${aString}."${bString}c" = true; }.a.bc;

  recBinds = rec { ${bString} = a; a = true; }.b;

  multiAttrs = { ${aString} = true; ${bString} = false; }.a;
}
```

#### Expected 

```text
{ binds = true; hasAttrs = true; multiAttrs = true; recBinds = true; selectAttrs = true; selectOrAttrs = true; }

```
### eval-okay-dynamic-attrs.nix

```nix
let
  aString = "a";

  bString = "b";
in {
  hasAttrs = { a.b = null; } ? "${aString}".b;

  selectAttrs = { a.b = true; }.a."${bString}";

  selectOrAttrs = { }."${aString}" or true;

  binds = { "${aString}"."${bString}c" = true; }.a.bc;

  recBinds = rec { "${bString}" = a; a = true; }.b;

  multiAttrs = { "${aString}" = true; "${bString}" = false; }.a;
}
```

#### Expected 

```text
{ binds = true; hasAttrs = true; multiAttrs = true; recBinds = true; selectAttrs = true; selectOrAttrs = true; }

```

### eval-okay-eq.nix

```nix
["foobar" (rec {x = 1; y = x;})]
==
[("foo" + "bar") ({x = 1; y = 1;})]
```


### eval-okay-if.nix
```nix
if "foo" != "f" + "oo" then 1 else if false then 2 else 3
```

#### Expected 

```text
3

```

### eval-okay-null-dynamic-attrs.nix

```nix
{ ${null} = true; } == {}
```

#### Expected 

```text
true

```


### eval-okay-path-string-interpolation.nix

```nix
let
  foo = "foo";
in
{
  simple = ./${foo};
  surrounded = ./a-${foo}-b;
  absolute = /${foo};
  expr = ./${foo + "/bar"};
  home = ~/${foo};
  notfirst = ./bar/${foo};
  slashes = /${foo}/${"bar"};
}
```


### eval-okay-patterns.nix

```nix
let

  f = args@{x, y, z}: x + args.y + z;

  g = {x, y, z}@args: f args;

  h = {x ? "d", y ? x, z ? args.x}@args: x + y + z;

  j = {x, y, z, ...}: x + y + z;

in
  f {x = "a"; y = "b"; z = "c";} +
  g {x = "x"; y = "y"; z = "z";} +
  h {x = "D";} +
  h {x = "D"; y = "E"; z = "F";} +
  j {x = "i"; y = "j"; z = "k"; bla = "bla"; foo = "bar";}
```

#### Expected 

```text
"abcxyzDDDDEFijk"

```
### eval-okay-redefine-builtin.nix

```nix
let
  throw = abort "Error!";
in (builtins.tryEval <foobaz>).success
```

#### Expected 

```text
false

```
### eval-okay-scope-3.nix

```nix
((x: as: {x}:
  rec {
    inherit (as) x;
    y = x;
  }
) 2 {x = 4;} {x = 3;}).y
```

#### Expected 

```text
4

```
### eval-okay-scope-6.nix

```nix
let {

  f = {x ? y, y ? x}: x + y;

  body = f {x = "c";} + f {y = "d";};

}
```

#### Expected 

```text
"ccdd"

```

### eval-okay-scope-7.nix

```nix
rec {
  inherit (x) y;
  x = {
    y = 1;
  };
}.y
```

#### Expected 

```text
1

```

### parse-fail-dup-attrs-7.nix

```nix
rec {

  x = 1;

  as = {
    inherit x;
    inherit x;
  };
}
```

#### Expected 

```text
error: attribute 'x' already defined at «stdin»:6:13
       at «stdin»:7:13:
            6|     inherit x;
            7|     inherit x;
             |             ^
            8|   };

```
```nix
{x, y, x}: x
```

#### Expected 

```text
error: duplicate formal function argument 'x'
       at «stdin»:1:8:
            1| {x, y, x}: x
             |        ^

```



### Extra
nix-repl> { x.y = 3; x = {z = 3;}; }.x
{
  y = 3;
  z = 3;
}


### parse-fail-patterns-1.nix

```nix
args@{args, x, y, z}: x
```

#### Expected 

```text
error: duplicate formal function argument 'args'
       at «stdin»:1:1:
            1| args@{args, x, y, z}: x
             | ^
            2|

```

