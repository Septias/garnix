# Sammlung der .nix-Dateien

### eval-fail-abort.nix

```nix
if true then abort "this should fail" else 1
```

#### Expected 

```text
error:
       … while calling the 'abort' builtin
         at /pwd/lang/eval-fail-abort.nix:1:14:
            1| if true then abort "this should fail" else 1
             |              ^
            2|

       error: evaluation aborted with the following error message: 'this should fail'

```

### eval-fail-addDrvOutputDependencies-empty-context.nix

```nix
builtins.addDrvOutputDependencies ""
```

#### Expected 

```text
error:
       … while calling the 'addDrvOutputDependencies' builtin
         at /pwd/lang/eval-fail-addDrvOutputDependencies-empty-context.nix:1:1:
            1| builtins.addDrvOutputDependencies ""
             | ^
            2|

       error: context of string '' must have exactly one element, but has 0

```

### eval-fail-addDrvOutputDependencies-multi-elem-context.nix

```nix
let
  drv0 = derivation {
    name = "fail";
    builder = "/bin/false";
    system = "x86_64-linux";
    outputs = [ "out" "foo" ];
  };

  drv1 = derivation {
    name = "fail-2";
    builder = "/bin/false";
    system = "x86_64-linux";
    outputs = [ "out" "foo" ];
  };

  combo-path = "${drv0.drvPath}${drv1.drvPath}";- ``

in builtins.addDrvOutputDependencies combo-path
```

#### Expected 

```text
error:
       … while calling the 'addDrvOutputDependencies' builtin
         at /pwd/lang/eval-fail-addDrvOutputDependencies-multi-elem-context.nix:18:4:
           17|
           18| in builtins.addDrvOutputDependencies combo-path
             |    ^
           19|

       error: context of string '/nix/store/pg9yqs4yd85yhdm3f4i5dyaqp5jahrsz-fail.drv/nix/store/2dxd5frb715z451vbf7s8birlf3argbk-fail-2.drv' must have exactly one element, but has 2

```

### eval-fail-addDrvOutputDependencies-wrong-element-kind.nix

```nix
let
  drv = derivation {
    name = "fail";
    builder = "/bin/false";
    system = "x86_64-linux";
    outputs = [ "out" "foo" ];
  };

in builtins.addDrvOutputDependencies drv.outPath
```

#### Expected 

```text
error:
       … while calling the 'addDrvOutputDependencies' builtin
         at /pwd/lang/eval-fail-addDrvOutputDependencies-wrong-element-kind.nix:9:4:
            8|
            9| in builtins.addDrvOutputDependencies drv.outPath
             |    ^
           10|

       error: `addDrvOutputDependencies` can only act on derivations, not on a derivation output such as 'out'

```

### eval-fail-addErrorContext-example.nix

```nix
let
  countDown = n:
    if n == 0
    then throw "kaboom"
    else
      builtins.addErrorContext
        "while counting down; n = ${toString n}"
        ("x" + countDown (n - 1));
in countDown 10
```

#### Expected 

```text
error:
       … while counting down; n = 10

       … while counting down; n = 9

       … while counting down; n = 8

       … while counting down; n = 7

       … while counting down; n = 6

       … while counting down; n = 5

       … while counting down; n = 4

       … while counting down; n = 3

       … while counting down; n = 2

       … while counting down; n = 1

       (stack trace truncated; use '--show-trace' to show the full, detailed trace)

       error: kaboom

```

### eval-fail-assert-equal-attrs-names-2.nix

```nix
assert { a = true; } == { a = true; b = true; };
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ a = true; } == { a = true; b = true; })'
         at /pwd/lang/eval-fail-assert-equal-attrs-names-2.nix:1:1:
            1| assert { a = true; } == { a = true; b = true; };
             | ^
            2| throw "unreachable"

       error: attribute names of attribute set '{ a = true; }' differs from attribute set '{ a = true; b = true; }'

```

### eval-fail-assert-equal-attrs-names.nix

```nix
assert { a = true; b = true; } == { a = true; };
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ a = true; b = true; } == { a = true; })'
         at /pwd/lang/eval-fail-assert-equal-attrs-names.nix:1:1:
            1| assert { a = true; b = true; } == { a = true; };
             | ^
            2| throw "unreachable"

       error: attribute names of attribute set '{ a = true; b = true; }' differs from attribute set '{ a = true; }'

```

### eval-fail-assert-equal-derivations-extra.nix

```nix
assert
  { foo = { type = "derivation"; outPath = "/nix/store/0"; }; }
  ==
  { foo = { type = "derivation"; outPath = "/nix/store/1"; devious = true; }; };
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ foo = { outPath = "/nix/store/0"; type = "derivation"; }; } == { foo = { devious = true; outPath = "/nix/store/1"; type = "derivation"; }; })'
         at /pwd/lang/eval-fail-assert-equal-derivations-extra.nix:1:1:
            1| assert
             | ^
            2|   { foo = { type = "derivation"; outPath = "/nix/store/0"; }; }

       … while comparing attribute 'foo'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-equal-derivations-extra.nix:2:5:
            1| assert
            2|   { foo = { type = "derivation"; outPath = "/nix/store/0"; }; }
             |     ^
            3|   ==

       … where right hand side is
         at /pwd/lang/eval-fail-assert-equal-derivations-extra.nix:4:5:
            3|   ==
            4|   { foo = { type = "derivation"; outPath = "/nix/store/1"; devious = true; }; };
             |     ^
            5| throw "unreachable"

       … while comparing a derivation by its 'outPath' attribute

       error: string '"/nix/store/0"' is not equal to string '"/nix/store/1"'

```

### eval-fail-assert-equal-derivations.nix

```nix
assert
  { foo = { type = "derivation"; outPath = "/nix/store/0"; ignored = abort "not ignored"; }; }
  ==
  { foo = { type = "derivation"; outPath = "/nix/store/1"; ignored = abort "not ignored"; }; };
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ foo = { ignored = (abort "not ignored"); outPath = "/nix/store/0"; type = "derivation"; }; } == { foo = { ignored = (abort "not ignored"); outPath = "/nix/store/1"; type = "derivation"; }; })'
         at /pwd/lang/eval-fail-assert-equal-derivations.nix:1:1:
            1| assert
             | ^
            2|   { foo = { type = "derivation"; outPath = "/nix/store/0"; ignored = abort "not ignored"; }; }

       … while comparing attribute 'foo'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-equal-derivations.nix:2:5:
            1| assert
            2|   { foo = { type = "derivation"; outPath = "/nix/store/0"; ignored = abort "not ignored"; }; }
             |     ^
            3|   ==

       … where right hand side is
         at /pwd/lang/eval-fail-assert-equal-derivations.nix:4:5:
            3|   ==
            4|   { foo = { type = "derivation"; outPath = "/nix/store/1"; ignored = abort "not ignored"; }; };
             |     ^
            5| throw "unreachable"

       … while comparing a derivation by its 'outPath' attribute

       error: string '"/nix/store/0"' is not equal to string '"/nix/store/1"'

```

### eval-fail-assert-equal-floats.nix

```nix
assert { b = 1.0; } == { b = 1.01; };
abort "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ b = 1; } == { b = 1.01; })'
         at /pwd/lang/eval-fail-assert-equal-floats.nix:1:1:
            1| assert { b = 1.0; } == { b = 1.01; };
             | ^
            2| abort "unreachable"

       … while comparing attribute 'b'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-equal-floats.nix:1:10:
            1| assert { b = 1.0; } == { b = 1.01; };
             |          ^
            2| abort "unreachable"

       … where right hand side is
         at /pwd/lang/eval-fail-assert-equal-floats.nix:1:26:
            1| assert { b = 1.0; } == { b = 1.01; };
             |                          ^
            2| abort "unreachable"

       error: a float with value '1' is not equal to a float with value '1.01'

```

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

### eval-fail-assert-equal-int-float.nix

```nix
assert 1 == 1.1;
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '(1 == 1.1)'
         at /pwd/lang/eval-fail-assert-equal-int-float.nix:1:1:
            1| assert 1 == 1.1;
             | ^
            2| throw "unreachable"

       error: an integer with value '1' is not equal to a float with value '1.1'

```

### eval-fail-assert-equal-ints.nix

```nix
assert { b = 1; } == { b = 2; };
abort "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ b = 1; } == { b = 2; })'
         at /pwd/lang/eval-fail-assert-equal-ints.nix:1:1:
            1| assert { b = 1; } == { b = 2; };
             | ^
            2| abort "unreachable"

       … while comparing attribute 'b'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-equal-ints.nix:1:10:
            1| assert { b = 1; } == { b = 2; };
             |          ^
            2| abort "unreachable"

       … where right hand side is
         at /pwd/lang/eval-fail-assert-equal-ints.nix:1:24:
            1| assert { b = 1; } == { b = 2; };
             |                        ^
            2| abort "unreachable"

       error: an integer with value '1' is not equal to an integer with value '2'

```

### eval-fail-assert-equal-list-length.nix

```nix
assert [ 1 0 ] == [ 10 ];
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '([ (1) (0) ] == [ (10) ])'
         at /pwd/lang/eval-fail-assert-equal-list-length.nix:1:1:
            1| assert [ 1 0 ] == [ 10 ];
             | ^
            2| throw "unreachable"

       error: list of size '2' is not equal to list of size '1', left hand side is '[ 1 0 ]', right hand side is '[ 10 ]'

```

### eval-fail-assert-equal-paths.nix

```nix
assert ./foo == ./bar;
throw "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '(/pwd/lang/foo == /pwd/lang/bar)'
         at /pwd/lang/eval-fail-assert-equal-paths.nix:1:1:
            1| assert ./foo == ./bar;
             | ^
            2| throw "unreachable"

       error: path '/pwd/lang/foo' is not equal to path '/pwd/lang/bar'

```

### eval-fail-assert-equal-type-nested.nix

```nix
assert { ding = false; } == { ding = null; };
abort "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ ding = false; } == { ding = null; })'
         at /pwd/lang/eval-fail-assert-equal-type-nested.nix:1:1:
            1| assert { ding = false; } == { ding = null; };
             | ^
            2| abort "unreachable"

       … while comparing attribute 'ding'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-equal-type-nested.nix:1:10:
            1| assert { ding = false; } == { ding = null; };
             |          ^
            2| abort "unreachable"

       … where right hand side is
         at /pwd/lang/eval-fail-assert-equal-type-nested.nix:1:31:
            1| assert { ding = false; } == { ding = null; };
             |                               ^
            2| abort "unreachable"

       error: a Boolean of value 'false' is not equal to null of value 'null'

```

### eval-fail-assert-equal-type.nix

```nix
assert false == null;
abort "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ ding = false; } == { ding = null; })'
         at /pwd/lang/eval-fail-assert-equal-type-nested.nix:1:1:
            1| assert { ding = false; } == { ding = null; };
             | ^
            2| abort "unreachable"

       … while comparing attribute 'ding'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-equal-type-nested.nix:1:10:
            1| assert { ding = false; } == { ding = null; };
             |          ^
            2| abort "unreachable"

       … where right hand side is
         at /pwd/lang/eval-fail-assert-equal-type-nested.nix:1:31:
            1| assert { ding = false; } == { ding = null; };
             |                               ^
            2| abort "unreachable"

       error: a Boolean of value 'false' is not equal to null of value 'null'

```

### eval-fail-assert-nested-bool.nix

```nix
assert
  { a.b = [ { c.d = true; } ]; }
  ==
  { a.b = [ { c.d = false; } ]; };

abort "unreachable"
```

#### Expected 

```text
error:
       … while evaluating the condition of the assertion '({ a = { b = [ ({ c = { d = true; }; }) ]; }; } == { a = { b = [ ({ c = { d = false; }; }) ]; }; })'
         at /pwd/lang/eval-fail-assert-nested-bool.nix:1:1:
            1| assert
             | ^
            2|   { a.b = [ { c.d = true; } ]; }

       … while comparing attribute 'a'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:2:5:
            1| assert
            2|   { a.b = [ { c.d = true; } ]; }
             |     ^
            3|   ==

       … where right hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:4:5:
            3|   ==
            4|   { a.b = [ { c.d = false; } ]; };
             |     ^
            5|

       … while comparing attribute 'b'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:2:5:
            1| assert
            2|   { a.b = [ { c.d = true; } ]; }
             |     ^
            3|   ==

       … where right hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:4:5:
            3|   ==
            4|   { a.b = [ { c.d = false; } ]; };
             |     ^
            5|

       … while comparing list element 0

       … while comparing attribute 'c'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:2:15:
            1| assert
            2|   { a.b = [ { c.d = true; } ]; }
             |               ^
            3|   ==

       … where right hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:4:15:
            3|   ==
            4|   { a.b = [ { c.d = false; } ]; };
             |               ^
            5|

       … while comparing attribute 'd'

       … where left hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:2:15:
            1| assert
            2|   { a.b = [ { c.d = true; } ]; }
             |               ^
            3|   ==

       … where right hand side is
         at /pwd/lang/eval-fail-assert-nested-bool.nix:4:15:
            3|   ==
            4|   { a.b = [ { c.d = false; } ]; };
             |               ^
            5|

       error: boolean 'true' is not equal to boolean 'false'

```

### eval-fail-assert.nix

```nix
let {
  x = arg: assert arg == "y"; 123;

  body = x "x";
}
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

### eval-fail-attr-name-type.nix

```nix
let
  attrs = {
    puppy.doggy = {};
  };
  key = 1;
in
  attrs.puppy.${key}
```

#### Expected 

```text
error:
       … while evaluating the attribute 'puppy."${key}"'
         at /pwd/lang/eval-fail-attr-name-type.nix:3:5:
            2|   attrs = {
            3|     puppy.doggy = {};
             |     ^
            4|   };

       … while evaluating an attribute name
         at /pwd/lang/eval-fail-attr-name-type.nix:7:17:
            6| in
            7|   attrs.puppy.${key}
             |                 ^
            8|

       error: expected a string but found an integer: 1
       at /pwd/lang/eval-fail-attr-name-type.nix:7:17:
            6| in
            7|   attrs.puppy.${key}
             |                 ^
            8|

```

### eval-fail-bad-string-interpolation-1.nix

```nix
"${x: x}"
```

#### Expected 

```text
error:
       … while evaluating a path segment
         at /pwd/lang/eval-fail-bad-string-interpolation-1.nix:1:2:
            1| "${x: x}"
             |  ^
            2|

       error: cannot coerce a function to a string: «lambda @ /pwd/lang/eval-fail-bad-string-interpolation-1.nix:1:4»

```

### eval-fail-bad-string-interpolation-2.nix

```nix
"${./fnord}"
```

#### Expected 

```text
error: path '/pwd/lang/fnord' does not exist

```

### eval-fail-bad-string-interpolation-3.nix

```nix
''${x: x}''
```

#### Expected 

```text
error:
       … while evaluating a path segment
         at /pwd/lang/eval-fail-bad-string-interpolation-3.nix:1:3:
            1| ''${x: x}''
             |   ^
            2|

       error: cannot coerce a function to a string: «lambda @ /pwd/lang/eval-fail-bad-string-interpolation-3.nix:1:5»

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

### eval-fail-call-primop.nix

```nix
builtins.length 1
```

#### Expected 

```text
error:
       … while calling the 'length' builtin
         at /pwd/lang/eval-fail-call-primop.nix:1:1:
            1| builtins.length 1
             | ^
            2|

       … while evaluating the first argument passed to builtins.length

       error: expected a list but found an integer: 1

```

### eval-fail-deepseq.nix

```nix
builtins.deepSeq { x = abort "foo"; } 456
```

#### Expected 

```text
error:
       … while calling the 'deepSeq' builtin
         at /pwd/lang/eval-fail-deepseq.nix:1:1:
            1| builtins.deepSeq { x = abort "foo"; } 456
             | ^
            2|

       … while evaluating the attribute 'x'
         at /pwd/lang/eval-fail-deepseq.nix:1:20:
            1| builtins.deepSeq { x = abort "foo"; } 456
             |                    ^
            2|

       … while calling the 'abort' builtin
         at /pwd/lang/eval-fail-deepseq.nix:1:24:
            1| builtins.deepSeq { x = abort "foo"; } 456
             |                        ^
            2|

       error: evaluation aborted with the following error message: 'foo'

```

### eval-fail-derivation-name.nix

```nix
derivation {
  name = "~jiggle~";
  system = "some-system";
  builder = "/dontcare";
}
```

#### Expected 

```text
error:
       … while evaluating the attribute 'outPath'
         at <nix/derivation-internal.nix>:<number>:<number>:
     <number>|       value = commonAttrs // {
     <number>|         outPath = builtins.getAttr outputName strict;
             |         ^
     <number>|         drvPath = strict.drvPath;

       … while calling the 'getAttr' builtin
         at <nix/derivation-internal.nix>:<number>:<number>:
     <number>|       value = commonAttrs // {
     <number>|         outPath = builtins.getAttr outputName strict;
             |                   ^
     <number>|         drvPath = strict.drvPath;

       … while calling the 'derivationStrict' builtin
         at <nix/derivation-internal.nix>:<number>:<number>:
     <number>|
     <number>|   strict = derivationStrict drvAttrs;
             |            ^
     <number>|

       … while evaluating derivation '~jiggle~'
         whose name attribute is located at /pwd/lang/eval-fail-derivation-name.nix:<number>:<number>

       error: invalid derivation name: name '~jiggle~' contains illegal character '~'. Please pass a different 'name'.

```

### eval-fail-dup-dynamic-attrs.nix #bookmark

```nix
{
  set = { "${"" + "b"}" = 1; };
  set = { "${"b" + ""}" = 2; };
}
```

#### Expected 

```text
error:
       … while evaluating the attribute 'set'
         at /pwd/lang/eval-fail-dup-dynamic-attrs.nix:2:3:
            1| {
            2|   set = { "${"" + "b"}" = 1; };
             |   ^
            3|   set = { "${"b" + ""}" = 2; };

       error: dynamic attribute 'b' already defined at /pwd/lang/eval-fail-dup-dynamic-attrs.nix:2:11
       at /pwd/lang/eval-fail-dup-dynamic-attrs.nix:3:11:
            2|   set = { "${"" + "b"}" = 1; };
            3|   set = { "${"b" + ""}" = 2; };
             |           ^
            4| }

```

### eval-fail-duplicate-traces.nix

```nix
# Check that we only omit duplicate stack traces when there's a bunch of them.
# Here, there's only a couple duplicate entries, so we output them all.
let
  throwAfter = n:
    if n > 0
    then throwAfter (n - 1)
    else throw "Uh oh!";
in
  throwAfter 2
```

#### Expected 

```text
error:
       … from call site
         at /pwd/lang/eval-fail-duplicate-traces.nix:9:3:
            8| in
            9|   throwAfter 2
             |   ^
           10|

       … while calling 'throwAfter'
         at /pwd/lang/eval-fail-duplicate-traces.nix:4:16:
            3| let
            4|   throwAfter = n:
             |                ^
            5|     if n > 0

       … from call site
         at /pwd/lang/eval-fail-duplicate-traces.nix:6:10:
            5|     if n > 0
            6|     then throwAfter (n - 1)
             |          ^
            7|     else throw "Uh oh!";

       … while calling 'throwAfter'
         at /pwd/lang/eval-fail-duplicate-traces.nix:4:16:
            3| let
            4|   throwAfter = n:
             |                ^
            5|     if n > 0

       … from call site
         at /pwd/lang/eval-fail-duplicate-traces.nix:6:10:
            5|     if n > 0
            6|     then throwAfter (n - 1)
             |          ^
            7|     else throw "Uh oh!";

       … while calling 'throwAfter'
         at /pwd/lang/eval-fail-duplicate-traces.nix:4:16:
            3| let
            4|   throwAfter = n:
             |                ^
            5|     if n > 0

       … while calling the 'throw' builtin
         at /pwd/lang/eval-fail-duplicate-traces.nix:7:10:
            6|     then throwAfter (n - 1)
            7|     else throw "Uh oh!";
             |          ^
            8| in

       error: Uh oh!

```

### eval-fail-eol-1.nix

```nix
# foo
invalid
# bar
```

#### Expected 

```text
error: undefined variable 'invalid'
       at /pwd/lang/eval-fail-eol-1.nix:2:1:
            1| # foo
            2| invalid
             | ^
            3| # bar

```

### eval-fail-eol-2.nix

```nix
# fooinvalid
# bar
```

#### Expected 

```text
error: undefined variable 'invalid'
       at /pwd/lang/eval-fail-eol-2.nix:2:1:
            1| # foo
            2| invalid
             | ^
            3| # bar

```

### eval-fail-eol-3.nix

```nix
# foo
invalid
# bar
```

#### Expected 

```text
error: undefined variable 'invalid'
       at /pwd/lang/eval-fail-eol-3.nix:2:1:
            1| # foo
            2| invalid
             | ^
            3| # bar

```

### eval-fail-fetchTree-negative.nix

```nix
builtins.fetchTree {
  type = "file";
  url = "file://eval-fail-fetchTree-negative.nix";
  owner = -1;
}
```

#### Expected 

```text
error:
       … while calling the 'fetchTree' builtin
         at /pwd/lang/eval-fail-fetchTree-negative.nix:1:1:
            1| builtins.fetchTree {
             | ^
            2|   type = "file";

       error: negative value given for fetchTree attr owner: -1

```

### eval-fail-fetchurl-baseName-attrs-name.nix

```nix
builtins.fetchurl { url = "https://example.com/foo.tar.gz"; name = "~wobble~"; }
```

#### Expected 

```text
error:
       … while calling the 'fetchurl' builtin
         at /pwd/lang/eval-fail-fetchurl-baseName-attrs-name.nix:1:1:
            1| builtins.fetchurl { url = "https://example.com/foo.tar.gz"; name = "~wobble~"; }
             | ^
            2|

       error: invalid store path name when fetching URL 'https://example.com/foo.tar.gz': name '~wobble~' contains illegal character '~'. Please change the value for the 'name' attribute passed to 'fetchurl', so that it can create a valid store path.

```

### eval-fail-fetchurl-baseName-attrs.nix

```nix
builtins.fetchurl { url = "https://example.com/~wiggle~"; }
```

#### Expected 

```text
error:
       … while calling the 'fetchurl' builtin
         at /pwd/lang/eval-fail-fetchurl-baseName-attrs-name.nix:1:1:
            1| builtins.fetchurl { url = "https://example.com/foo.tar.gz"; name = "~wobble~"; }
             | ^
            2|

       error: invalid store path name when fetching URL 'https://example.com/foo.tar.gz': name '~wobble~' contains illegal character '~'. Please change the value for the 'name' attribute passed to 'fetchurl', so that it can create a valid store path.

```

### eval-fail-fetchurl-baseName.nix

```nix
builtins.fetchurl "https://example.com/~wiggle~"
```

#### Expected 

```text
error:
       … while calling the 'fetchurl' builtin
         at /pwd/lang/eval-fail-fetchurl-baseName-attrs-name.nix:1:1:
            1| builtins.fetchurl { url = "https://example.com/foo.tar.gz"; name = "~wobble~"; }
             | ^
            2|

       error: invalid store path name when fetching URL 'https://example.com/foo.tar.gz': name '~wobble~' contains illegal character '~'. Please change the value for the 'name' attribute passed to 'fetchurl', so that it can create a valid store path.

```

### eval-fail-flake-ref-to-string-negative-integer.nix

```nix
let n = -1; in builtins.seq n (builtins.flakeRefToString {
  type  = "github";
  owner = "NixOS";
  repo  = n;
  ref   = "23.05";
  dir   = "lib";
})
```

#### Expected 

```text
error:
       … while calling the 'seq' builtin
         at /pwd/lang/eval-fail-flake-ref-to-string-negative-integer.nix:1:16:
            1| let n = -1; in builtins.seq n (builtins.flakeRefToString {
             |                ^
            2|   type  = "github";

       … while calling the 'flakeRefToString' builtin
         at /pwd/lang/eval-fail-flake-ref-to-string-negative-integer.nix:1:32:
            1| let n = -1; in builtins.seq n (builtins.flakeRefToString {
             |                                ^
            2|   type  = "github";

       error: negative value given for flake ref attr repo: -1

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

### eval-fail-fromJSON-overflowing.nix

```nix
builtins.fromJSON ''{"attr": 18446744073709551615}''
```

#### Expected 

```text
error:
       … while calling the 'fromJSON' builtin
         at /pwd/lang/eval-fail-fromJSON-overflowing.nix:1:1:
            1| builtins.fromJSON ''{"attr": 18446744073709551615}''
             | ^
            2|

       error: unsigned json number 18446744073709551615 outside of Nix integer range

```

### eval-fail-fromTOML-timestamps.nix

```nix
builtins.fromTOML ''
  key = "value"
  bare_key = "value"
  bare-key = "value"
  1234 = "value"

  "127.0.0.1" = "value"
  "character encoding" = "value"
  "ʎǝʞ" = "value"
  'key2' = "value"
  'quoted "value"' = "value"

  name = "Orange"

  physical.color = "orange"
  physical.shape = "round"
  site."google.com" = true

  # This is legal according to the spec, but cpptoml doesn't handle it.
  #a.b.c = 1
  #a.d = 2

  str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."

  int1 = +99
  int2 = 42
  int3 = 0
  int4 = -17
  int5 = 1_000
  int6 = 5_349_221
  int7 = 1_2_3_4_5

  hex1 = 0xDEADBEEF
  hex2 = 0xdeadbeef
  hex3 = 0xdead_beef

  oct1 = 0o01234567
  oct2 = 0o755

  bin1 = 0b11010110

  flt1 = +1.0
  flt2 = 3.1415
  flt3 = -0.01
  flt4 = 5e+22
  flt5 = 1e6
  flt6 = -2E-2
  flt7 = 6.626e-34
  flt8 = 9_224_617.445_991_228_313

  bool1 = true
  bool2 = false

  odt1 = 1979-05-27T07:32:00Z
  odt2 = 1979-05-27T00:32:00-07:00
  odt3 = 1979-05-27T00:32:00.999999-07:00
  odt4 = 1979-05-27 07:32:00Z
  ldt1 = 1979-05-27T07:32:00
  ldt2 = 1979-05-27T00:32:00.999999
  ld1 = 1979-05-27
  lt1 = 07:32:00
  lt2 = 00:32:00.999999

  arr1 = [ 1, 2, 3 ]
  arr2 = [ "red", "yellow", "green" ]
  arr3 = [ [ 1, 2 ], [3, 4, 5] ]
  arr4 = [ "all", 'strings', """are the same""", ''''type'''']
  arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]

  arr7 = [
    1, 2, 3
  ]

  arr8 = [
    1,
    2, # this is ok
  ]

  [table-1]
  key1 = "some string"
  key2 = 123


  [table-2]
  key1 = "another string"
  key2 = 456

  [dog."tater.man"]
  type.name = "pug"

  [a.b.c]
  [ d.e.f ]
  [ g .  h  . i ]
  [ j . "ʞ" . 'l' ]
  [x.y.z.w]

  name = { first = "Tom", last = "Preston-Werner" }
  point = { x = 1, y = 2 }
  animal = { type.name = "pug" }

  [[products]]
  name = "Hammer"
  sku = 738594937

  [[products]]

  [[products]]
  name = "Nail"
  sku = 284758393
  color = "gray"

  [[fruit]]
    name = "apple"

    [fruit.physical]
      color = "red"
      shape = "round"

    [[fruit.variety]]
      name = "red delicious"

    [[fruit.variety]]
      name = "granny smith"

  [[fruit]]
    name = "banana"

    [[fruit.variety]]
      name = "plantain"
''
```

#### Expected 

```text
error:
       … while calling the 'fromTOML' builtin
         at /pwd/lang/eval-fail-fromTOML-timestamps.nix:1:1:
            1| builtins.fromTOML ''
             | ^
            2|   key = "value"

       error: while parsing TOML: Dates and times are not supported

```

### eval-fail-hashfile-missing.nix

```nix
let
  paths = [ ./this-file-is-definitely-not-there-7392097 "/and/neither/is/this/37293620" ];
in
  toString (builtins.concatLists (map (hash: map (builtins.hashFile hash) paths) ["md5" "sha1" "sha256" "sha512"]))

```

#### Expected 

```text
error:
       … while calling the 'toString' builtin
         at /pwd/lang/eval-fail-hashfile-missing.nix:4:3:
            3| in
            4|   toString (builtins.concatLists (map (hash: map (builtins.hashFile hash) paths) ["md5" "sha1" "sha256" "sha512"]))
             |   ^
            5|

       … while evaluating the first argument passed to builtins.toString

       … while calling the 'hashFile' builtin

       error: opening file '/pwd/lang/this-file-is-definitely-not-there-7392097': No such file or directory

```

### eval-fail-infinite-recursion-lambda.nix

```nix
(x: x x) (x: x x)
```

#### Expected 

```text
error:
       … from call site
         at /pwd/lang/eval-fail-infinite-recursion-lambda.nix:1:1:
            1| (x: x x) (x: x x)
             | ^
            2|

       … while calling anonymous lambda
         at /pwd/lang/eval-fail-infinite-recursion-lambda.nix:1:2:
            1| (x: x x) (x: x x)
             |  ^
            2|

       … from call site
         at /pwd/lang/eval-fail-infinite-recursion-lambda.nix:1:5:
            1| (x: x x) (x: x x)
             |     ^
            2|

       … while calling anonymous lambda
         at /pwd/lang/eval-fail-infinite-recursion-lambda.nix:1:11:
            1| (x: x x) (x: x x)
             |           ^
            2|

       … from call site
         at /pwd/lang/eval-fail-infinite-recursion-lambda.nix:1:14:
            1| (x: x x) (x: x x)
             |              ^
            2|

       (197 duplicate frames omitted)

       error: stack overflow; max-call-depth exceeded
       at /pwd/lang/eval-fail-infinite-recursion-lambda.nix:1:14:
            1| (x: x x) (x: x x)
             |              ^
            2|

```

### eval-fail-list.nix

```nix
8++1
```

#### Expected 

```text
error:
       … while evaluating one of the elements to concatenate
         at /pwd/lang/eval-fail-list.nix:1:2:
            1| 8++1
             |  ^
            2|

       error: expected a list but found an integer: 8

```

### eval-fail-missing-arg.nix

```nix
({x, y, z}: x + y + z) {x = "foo"; z = "bar";}
```

#### Expected 

```text
error:
       … from call site
         at /pwd/lang/eval-fail-missing-arg.nix:1:1:
            1| ({x, y, z}: x + y + z) {x = "foo"; z = "bar";}
             | ^
            2|

       error: function 'anonymous lambda' called without required argument 'y'
       at /pwd/lang/eval-fail-missing-arg.nix:1:2:
            1| ({x, y, z}: x + y + z) {x = "foo"; z = "bar";}
             |  ^
            2|

```

### eval-fail-mutual-recursion.nix

```nix
# Check that stack frame deduplication only affects consecutive intervals, and
# that they are reported independently of any preceding sections, even if
# they're indistinguishable.
#
# In terms of the current implementation, we check that we clear the set of
# "seen frames" after eliding a group of frames.
#
# Suppose we have:
# - 10 frames in a function A
# - 10 frames in a function B
# - 10 frames in a function A
#
# We want to output:
# - a few frames of A (skip the rest)
# - a few frames of B (skip the rest)
# - a few frames of A (skip the rest)
#
# If we implemented this in the naive manner, we'd instead get:
# - a few frames of A (skip the rest)
# - a few frames of B (skip the rest, _and_ skip the remaining frames of A)
let
  throwAfterB = recurse: n:
    if n > 0
    then throwAfterB recurse (n - 1)
    else if recurse
    then throwAfterA false 10
    else throw "Uh oh!";

  throwAfterA = recurse: n:
    if n > 0
    then throwAfterA recurse (n - 1)
    else if recurse
    then throwAfterB true 10
    else throw "Uh oh!";
in
  throwAfterA true 10
```

#### Expected 

```text
error:
       … from call site
         at /pwd/lang/eval-fail-mutual-recursion.nix:36:3:
           35| in
           36|   throwAfterA true 10
             |   ^
           37|

       … while calling 'throwAfterA'
         at /pwd/lang/eval-fail-mutual-recursion.nix:29:26:
           28|
           29|   throwAfterA = recurse: n:
             |                          ^
           30|     if n > 0

       … from call site
         at /pwd/lang/eval-fail-mutual-recursion.nix:31:10:
           30|     if n > 0
           31|     then throwAfterA recurse (n - 1)
             |          ^
           32|     else if recurse

       (19 duplicate frames omitted)

       … from call site
         at /pwd/lang/eval-fail-mutual-recursion.nix:33:10:
           32|     else if recurse
           33|     then throwAfterB true 10
             |          ^
           34|     else throw "Uh oh!";

       … while calling 'throwAfterB'
         at /pwd/lang/eval-fail-mutual-recursion.nix:22:26:
           21| let
           22|   throwAfterB = recurse: n:
             |                          ^
           23|     if n > 0

       … from call site
         at /pwd/lang/eval-fail-mutual-recursion.nix:24:10:
           23|     if n > 0
           24|     then throwAfterB recurse (n - 1)
             |          ^
           25|     else if recurse

       (19 duplicate frames omitted)

       … from call site
         at /pwd/lang/eval-fail-mutual-recursion.nix:26:10:
           25|     else if recurse
           26|     then throwAfterA false 10
             |          ^
           27|     else throw "Uh oh!";

       (21 duplicate frames omitted)

       … while calling the 'throw' builtin
         at /pwd/lang/eval-fail-mutual-recursion.nix:34:10:
           33|     then throwAfterB true 10
           34|     else throw "Uh oh!";
             |          ^
           35| in

       error: Uh oh!

```

### eval-fail-nested-list-items.nix

```nix
# This reproduces https://github.com/NixOS/nix/issues/10993, for lists
# $ nix run nix/2.23.1 -- eval --expr '"" + (let v = [ [ 1 2 3 4 5 6 7 8 ] [1 2 3 4]]; in builtins.deepSeq v v)'
# error:
#        … while evaluating a path segment
#          at «string»:1:6:
#             1| "" + (let v = [ [ 1 2 3 4 5 6 7 8 ] [1 2 3 4]]; in builtins.deepSeq v v)
#              |      ^
#
#        error: cannot coerce a list to a string: [ [ 1 2 3 4 5 6 7 8 ] [ 1 «4294967290 items elided» ] ]

"" + (let v = [ [ 1 2 3 4 5 6 7 8 ] [1 2 3 4]]; in builtins.deepSeq v v)
```

#### Expected 

```text
error:
       … while evaluating a path segment
         at /pwd/lang/eval-fail-nested-list-items.nix:11:6:
           10|
           11| "" + (let v = [ [ 1 2 3 4 5 6 7 8 ] [1 2 3 4]]; in builtins.deepSeq v v)
             |      ^
           12|

       error: cannot coerce a list to a string: [ [ 1 2 3 4 5 6 7 8 ] [ 1 «3 items elided» ] ]

```

### eval-fail-nonexist-path.nix

```nix
# This must fail to evaluate, since ./fnord doesn't exist.  If it did
# exist, it would produce "/nix/store/<hash>-fnord/xyzzy" (with an
# appropriate context).
"${./fnord}/xyzzy"
```

#### Expected 

```text
error: path '/pwd/lang/fnord' does not exist

```

### eval-fail-not-throws.nix

```nix
! (throw "uh oh!")
```

#### Expected 

```text
error:
       … in the argument of the not operator
         at /pwd/lang/eval-fail-not-throws.nix:1:4:
            1| ! (throw "uh oh!")
             |    ^
            2|

       … while calling the 'throw' builtin
         at /pwd/lang/eval-fail-not-throws.nix:1:4:
            1| ! (throw "uh oh!")
             |    ^
            2|

       error: uh oh!

```

### eval-fail-overflowing-add.nix

```nix
let
  a = 9223372036854775807;
  b = 1;
in a + b
```

#### Expected 

```text
error: integer overflow in adding 9223372036854775807 + 1
       at /pwd/lang/eval-fail-overflowing-add.nix:4:8:
            3|   b = 1;
            4| in a + b
             |        ^
            5|

```

### eval-fail-overflowing-div.nix

```nix
let
  # lol, this has to be written as an expression like this because negative
  # numbers use unary negation rather than parsing directly, and 2**63 is out
  # of range
  intMin = -9223372036854775807 - 1;
  b = -1;
in builtins.seq intMin (builtins.seq b (intMin / b))
```

#### Expected 

```text
error:
       … while calling the 'seq' builtin
         at /pwd/lang/eval-fail-overflowing-div.nix:7:4:
            6|   b = -1;
            7| in builtins.seq intMin (builtins.seq b (intMin / b))
             |    ^
            8|

       … while calling the 'seq' builtin
         at /pwd/lang/eval-fail-overflowing-div.nix:7:25:
            6|   b = -1;
            7| in builtins.seq intMin (builtins.seq b (intMin / b))
             |                         ^
            8|

       … while calling the 'div' builtin
         at /pwd/lang/eval-fail-overflowing-div.nix:7:48:
            6|   b = -1;
            7| in builtins.seq intMin (builtins.seq b (intMin / b))
             |                                                ^
            8|

       error: integer overflow in dividing -9223372036854775808 / -1

```

### eval-fail-overflowing-mul.nix

```nix
let
  a = 4294967297;
in a * a * a
```

#### Expected 

```text
error:
       … while calling the 'mul' builtin
         at /pwd/lang/eval-fail-overflowing-mul.nix:3:10:
            2|   a = 4294967297;
            3| in a * a * a
             |          ^
            4|

       … while calling the 'mul' builtin
         at /pwd/lang/eval-fail-overflowing-mul.nix:3:6:
            2|   a = 4294967297;
            3| in a * a * a
             |      ^
            4|

       error: integer overflow in multiplying 4294967297 * 4294967297

```

### eval-fail-overflowing-sub.nix

```nix
let
  a = -9223372036854775807;
  b = 2;
in a - b
```

#### Expected 

```text
error:
       … while calling the 'sub' builtin
         at /pwd/lang/eval-fail-overflowing-sub.nix:4:6:
            3|   b = 2;
            4| in a - b
             |      ^
            5|

       error: integer overflow in subtracting -9223372036854775807 - 2

```

### eval-fail-path-slash.nix

```nix
# Trailing slashes in paths are not allowed.
# This restriction could be lifted sometime,
# for example if we make '/' a path concatenation operator.
# See https://github.com/NixOS/nix/issues/1138
# and https://nixos.org/nix-dev/2016-June/020829.html
/nix/store/
```

#### Expected 

```text
error: path has a trailing slash
       at /pwd/lang/eval-fail-path-slash.nix:6:12:
            5| # and https://nixos.org/nix-dev/2016-June/020829.html
            6| /nix/store/
             |            ^
            7|

```

### eval-fail-pipe-operators.nix

```nix
1 |> 2
```

#### Expected 

```text
error: experimental Nix feature 'pipe-operators' is disabled; add '--extra-experimental-features pipe-operators' to enable it
       at /pwd/lang/eval-fail-pipe-operators.nix:1:3:
            1| 1 |> 2
             |   ^
            2|

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

### eval-fail-remove.nix

```nix
let {
  attrs = {x = 123; y = 456;};

  body = (removeAttrs attrs ["x"]).x;
}
```

#### Expected 

```text
error:
       … while evaluating the attribute 'body'
         at /pwd/lang/eval-fail-remove.nix:4:3:
            3|
            4|   body = (removeAttrs attrs ["x"]).x;
             |   ^
            5| }

       error: attribute 'x' missing
       at /pwd/lang/eval-fail-remove.nix:4:10:
            3|
            4|   body = (removeAttrs attrs ["x"]).x;
             |          ^
            5| }
       Did you mean y?

```

### eval-fail-scope-5.nix

```nix
let {

  x = "a";
  y = "b";

  f = {x ? y, y ? x}: x + y;

  body = f {};

}
```

#### Expected 

```text
error:
       … while evaluating the attribute 'body'
         at /pwd/lang/eval-fail-scope-5.nix:8:3:
            7|
            8|   body = f {};
             |   ^
            9|

       … from call site
         at /pwd/lang/eval-fail-scope-5.nix:8:10:
            7|
            8|   body = f {};
             |          ^
            9|

       … while calling 'f'
         at /pwd/lang/eval-fail-scope-5.nix:6:7:
            5|
            6|   f = {x ? y, y ? x}: x + y;
             |       ^
            7|

       error: infinite recursion encountered
       at /pwd/lang/eval-fail-scope-5.nix:6:12:
            5|
            6|   f = {x ? y, y ? x}: x + y;
             |            ^
            7|

```

### eval-fail-seq.nix

```nix
builtins.seq (abort "foo") 2
```

#### Expected 

```text
error:
       … while calling the 'seq' builtin
         at /pwd/lang/eval-fail-seq.nix:1:1:
            1| builtins.seq (abort "foo") 2
             | ^
            2|

       … while calling the 'abort' builtin
         at /pwd/lang/eval-fail-seq.nix:1:15:
            1| builtins.seq (abort "foo") 2
             |               ^
            2|

       error: evaluation aborted with the following error message: 'foo'

```

### eval-fail-set-override.nix

```nix
rec { __overrides = 1; }
```

#### Expected 

```text
error:
       … while evaluating the `__overrides` attribute

       error: expected a set but found an integer: 1

```

### eval-fail-set.nix

```nix
8.x
```

#### Expected 

```text
error: undefined variable 'x'
       at /pwd/lang/eval-fail-set.nix:1:3:
            1| 8.x
             |   ^
            2|

```

### eval-fail-substring.nix

```nix
builtins.substring (builtins.sub 0 1) 1 "x"
```

#### Expected 

```text
error:
       … while calling the 'substring' builtin
         at /pwd/lang/eval-fail-substring.nix:1:1:
            1| builtins.substring (builtins.sub 0 1) 1 "x"
             | ^
            2|

       error: negative start position in 'substring'

```

### eval-fail-to-path.nix

```nix
builtins.toPath "foo/bar"
```

#### Expected 

```text
error:
       … while calling the 'toPath' builtin
         at /pwd/lang/eval-fail-to-path.nix:1:1:
            1| builtins.toPath "foo/bar"
             | ^
            2|

       … while evaluating the first argument passed to builtins.toPath

       error: string 'foo/bar' doesn't represent an absolute path

```

### eval-fail-toJSON.nix

```nix
builtins.toJSON {
  a.b = [
    true
    false
    "it's a bird"
    {
      c.d = throw "hah no";
    }
  ];
}
```

#### Expected 

```text
error:
       … while calling the 'toJSON' builtin
         at /pwd/lang/eval-fail-toJSON.nix:1:1:
            1| builtins.toJSON {
             | ^
            2|   a.b = [

       … while evaluating attribute 'a'
         at /pwd/lang/eval-fail-toJSON.nix:2:3:
            1| builtins.toJSON {
            2|   a.b = [
             |   ^
            3|     true

       … while evaluating attribute 'b'
         at /pwd/lang/eval-fail-toJSON.nix:2:3:
            1| builtins.toJSON {
            2|   a.b = [
             |   ^
            3|     true

       … while evaluating list element at index 3
         at /pwd/lang/eval-fail-toJSON.nix:2:3:
            1| builtins.toJSON {
            2|   a.b = [
             |   ^
            3|     true

       … while evaluating attribute 'c'
         at /pwd/lang/eval-fail-toJSON.nix:7:7:
            6|     {
            7|       c.d = throw "hah no";
             |       ^
            8|     }

       … while evaluating attribute 'd'
         at /pwd/lang/eval-fail-toJSON.nix:7:7:
            6|     {
            7|       c.d = throw "hah no";
             |       ^
            8|     }

       … while calling the 'throw' builtin
         at /pwd/lang/eval-fail-toJSON.nix:7:13:
            6|     {
            7|       c.d = throw "hah no";
             |             ^
            8|     }

       error: hah no

```

### eval-fail-undeclared-arg.nix

```nix
({x, z}: x + z) {x = "foo"; y = "bla"; z = "bar";}
```

#### Expected 

```text
error:
       … from call site
         at /pwd/lang/eval-fail-undeclared-arg.nix:1:1:
            1| ({x, z}: x + z) {x = "foo"; y = "bla"; z = "bar";}
             | ^
            2|

       error: function 'anonymous lambda' called with unexpected argument 'y'
       at /pwd/lang/eval-fail-undeclared-arg.nix:1:2:
            1| ({x, z}: x + z) {x = "foo"; y = "bla"; z = "bar";}
             |  ^
            2|
       Did you mean one of x or z?

```

### eval-fail-using-set-as-attr-name.nix

```nix
let
  attr = {foo = "bar";};
  key = {};
in
  attr.${key}
```

#### Expected 

```text
error:
       … while evaluating an attribute name
         at /pwd/lang/eval-fail-using-set-as-attr-name.nix:5:10:
            4| in
            5|   attr.${key}
             |          ^
            6|

       error: expected a string but found a set: { }
       at /pwd/lang/eval-fail-using-set-as-attr-name.nix:5:10:
            4| in
            5|   attr.${key}
             |          ^
            6|

```

### eval-okay-any-all.nix

```nix
with builtins;

[ (any (x: x == 1) [])
  (any (x: x == 1) [2 3 4])
  (any (x: x == 1) [1 2 3 4])
  (any (x: x == 1) [4 3 2 1])
  (all (x: x == 1) [])
  (all (x: x == 1) [1])
  (all (x: x == 1) [1 2 3])
  (all (x: x == 1) [1 1 1])
]
```

#### Expected 

```text
[ false false true true true true false true ]

```

### eval-okay-arithmetic.nix

```nix
with import ./lib.nix;

let {

  /* Supposedly tail recursive version:

  range_ = accum: first: last:
    if first == last then ([first] ++ accum)
    else range_ ([first] ++ accum) (builtins.add first 1) last;

  range = range_ [];
  */

  x = 12;

  err = abort "urgh";

  body = sum
    [ (sum (range 1 50))
      (123 + 456)
      (0 + -10 + -(-11) + -x)
      (10 - 7 - -2)
      (10 - (6 - -1))
      (10 - 1 + 2)
      (3 * 4 * 5)
      (56088 / 123 / 2)
      (3 + 4 * const 5 0 - 6 / id 2)

      (builtins.bitAnd 12 10) # 0b1100 & 0b1010 =  8
      (builtins.bitOr  12 10) # 0b1100 | 0b1010 = 14
      (builtins.bitXor 12 10) # 0b1100 ^ 0b1010 =  6

      (if 3 < 7 then 1 else err)
      (if 7 < 3 then err else 1)
      (if 3 < 3 then err else 1)

      (if 3 <= 7 then 1 else err)
      (if 7 <= 3 then err else 1)
      (if 3 <= 3 then 1 else err)

      (if 3 > 7 then err else 1)
      (if 7 > 3 then 1 else err)
      (if 3 > 3 then err else 1)

      (if 3 >= 7 then err else 1)
      (if 7 >= 3 then 1 else err)
      (if 3 >= 3 then 1 else err)

      (if 2 > 1 == 1 < 2 then 1 else err)
      (if 1 + 2 * 3 >= 7 then 1 else err)
      (if 1 + 2 * 3 < 7 then err else 1)

      # Not integer, but so what.
      (if "aa" < "ab" then 1 else err)
      (if "aa" < "aa" then err else 1)
      (if "foo" < "foobar" then 1 else err)
    ];

}
```

#### Expected 

```text
2216

```

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

### eval-okay-attrs.nix

```nix
let {
  as = { x = 123; y = 456; } // { z = 789; } // { z = 987; };

  body = if as ? a then as.a else assert as ? z; as.z;
}
```

#### Expected 

```text
987

```

### eval-okay-attrs2.nix

```nix
let {
  as = { x = 123; y = 456; } // { z = 789; } // { z = 987; };

  A = "a";
  Z = "z";

  body = if builtins.hasAttr A as
         then builtins.getAttr A as
         else assert builtins.hasAttr Z as; builtins.getAttr Z as;
}
```

#### Expected 

```text
987

```

### eval-okay-attrs3.nix

```nix
let

  config = 
    {
      services.sshd.enable = true;
      services.sshd.port = 22;
      services.httpd.port = 80;
      hostName = "itchy";
      a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z = "x";
      foo = {
        a = "a";
        b.c = "c";
      };
    };

in
  if config.services.sshd.enable
  then "foo ${toString config.services.sshd.port} ${toString config.services.httpd.port} ${config.hostName}"
       + "${config.a.b.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z}"
       + "${config.foo.a}"
       + "${config.foo.b.c}"
  else "bar"
```

#### Expected 

```text
"foo 22 80 itchyxac"

```

### eval-okay-attrs4.nix

```nix
let

  as = { x.y.z = 123; a.b.c = 456; };

  bs = null;

in [ (as ? x) (as ? y) (as ? x.y.z) (as ? x.y.z.a) (as ? x.y.a) (as ? a.b.c) (bs ? x) (bs ? x.y.z) ]
```

#### Expected 

```text
[ true false true false false true false false ]

```

### eval-okay-attrs5.nix

```nix
with import ./lib.nix;

let

  as = { x.y.z = 123; a.b.c = 456; };

  bs = { f-o-o.bar = "foo"; };

  or = x: y: x || y;
  
in
  [ as.x.y.z
    as.foo or "foo"
    as.x.y.bla or as.a.b.c
    as.a.b.c or as.x.y.z
    as.x.y.bla or bs.f-o-o.bar or "xyzzy"
    as.x.y.bla or bs.bar.foo or "xyzzy"
    (123).bla or null.foo or "xyzzy"
    # Backwards compatibility test.
    (fold or [] [true false false])
  ]
```

#### Expected 

```text
[ 123 "foo" 456 456 "foo" "xyzzy" "xyzzy" true ]

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

### eval-okay-autoargs.nix

```nix
let

  foobar = "foobar";

in

{ xyzzy2 ? xyzzy # mutually recursive args
, xyzzy ? "blaat" # will be overridden by --argstr
, fb ? foobar
, lib # will be set by --arg
}:

{
  result = lib.concat [xyzzy xyzzy2 fb];
}
```

#### Expected 

```text
"xyzzy!xyzzy!foobar"

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

### eval-okay-backslash-newline-2.nix

```nix
''a''\
b''
```

#### Expected 

```text
"a\nb"

```

### eval-okay-baseNameOf.nix

```nix
assert baseNameOf "" == "";
assert baseNameOf "." == ".";
assert baseNameOf ".." == "..";
assert baseNameOf "a" == "a";
assert baseNameOf "a." == "a.";
assert baseNameOf "a.." == "a..";
assert baseNameOf "a.b" == "a.b";
assert baseNameOf "a.b." == "a.b.";
assert baseNameOf "a.b.." == "a.b..";
assert baseNameOf "a/" == "a";
assert baseNameOf "a/." == ".";
assert baseNameOf "a/.." == "..";
assert baseNameOf "a/b" == "b";
assert baseNameOf "a/b." == "b.";
assert baseNameOf "a/b.." == "b..";
assert baseNameOf "a/b/c" == "c";
assert baseNameOf "a/b/c." == "c.";
assert baseNameOf "a/b/c.." == "c..";
assert baseNameOf "a/b/c/d" == "d";
assert baseNameOf "a/b/c/d." == "d.";
assert baseNameOf "a\\b" == "a\\b";
assert baseNameOf "C:a" == "C:a";
assert baseNameOf "a//b" == "b";

# It's been like this for close to a decade. We ought to commit to it.
# https://github.com/NixOS/nix/pull/582#issuecomment-121014450
assert baseNameOf "a//" == "";

assert baseNameOf ./foo == "foo";
assert baseNameOf ./foo/bar == "bar";

"ok"
```

#### Expected 

```text
"ok"

```

### eval-okay-builtins-add.nix

```nix
[
(builtins.add 2 3)
(builtins.add 2 2)
(builtins.typeOf (builtins.add 2  2))
("t" + "t")
(builtins.typeOf (builtins.add 2.0 2))
(builtins.add 2.0 2)
]
```

#### Expected 

```text
[ 5 4 "int" "tt" "float" 4 ]

```

### eval-okay-builtins.nix

```nix
assert builtins ? currentSystem;
assert !builtins ? __currentSystem;

let {

  x = if builtins ? dirOf then builtins.dirOf /foo/bar else "";

  y = if builtins ? fnord then builtins.fnord "foo" else "";

  body = x + y;
  
}
```

#### Expected 

```text
[ 5 4 "int" "tt" "float" 4 ]

```

### eval-okay-callable-attrs.nix

```nix
({ __functor = self: x: self.foo && x; foo = false; } // { foo = true; }) true
```

#### Expected 

```text
true

```

### eval-okay-catattrs.nix

```nix
builtins.catAttrs "a" [ { a = 1; } { b = 0; } { a = 2; } ]
```

#### Expected 

```text
[ 1 2 ]

```

### eval-okay-closure.nix

```nix
let

  closure = builtins.genericClosure {
    startSet = [{key = 80;}];
    operator = {key, foo ? false}:
      if builtins.lessThan key 0
      then []
      else [{key = builtins.sub key 9;} {key = builtins.sub key 13; foo = true;}];
  };

  sort = (import ./lib.nix).sortBy (a: b: builtins.lessThan a.key b.key);

in sort closure
```

#### Expected 

```text
[ { foo = true; key = -13; } { foo = true; key = -12; } { foo = true; key = -11; } { foo = true; key = -9; } { foo = true; key = -8; } { foo = true; key = -7; } { foo = true; key = -5; } { foo = true; key = -4; } { foo = true; key = -3; } { key = -1; } { foo = true; key = 0; } { foo = true; key = 1; } { foo = true; key = 2; } { foo = true; key = 4; } { foo = true; key = 5; } { foo = true; key = 6; } { key = 8; } { foo = true; key = 9; } { foo = true; key = 10; } { foo = true; key = 13; } { foo = true; key = 14; } { foo = true; key = 15; } { key = 17; } { foo = true; key = 18; } { foo = true; key = 19; } { foo = true; key = 22; } { foo = true; key = 23; } { key = 26; } { foo = true; key = 27; } { foo = true; key = 28; } { foo = true; key = 31; } { foo = true; key = 32; } { key = 35; } { foo = true; key = 36; } { foo = true; key = 40; } { foo = true; key = 41; } { key = 44; } { foo = true; key = 45; } { foo = true; key = 49; } { key = 53; } { foo = true; key = 54; } { foo = true; key = 58; } { key = 62; } { foo = true; key = 67; } { key = 71; } { key = 80; } ]

```

### eval-okay-comments.nix

```nix
# A simple comment
"a"+ # And another
## A double comment
"b"+  ## And another
# Nested # comments #
"c"+   # and # some # other #
# An empty line, following here:

"d"+      # and a comment not starting the line !

"e"+
/* multiline comments */
"f" +
/* multiline
   comments,
   on
   multiple
   lines
*/
"g" +
# Small, tricky comments
/**/ "h"+ /*/*/ "i"+ /***/ "j"+ /* /*/ "k"+ /*/* /*/ "l"+
# Comments with an even number of ending '*' used to fail:
"m"+
/* */ /* **/ /* ***/ /* ****/ "n"+
/* */ /** */ /*** */ /**** */ "o"+
/** **/ /*** ***/ /**** ****/ "p"+
/* * ** *** **** ***** */     "q"+
# Random comments
/* ***** ////// * / * / /* */ "r"+
# Mixed comments
/* # */
"s"+
# /* #
"t"+
# /* # */
"u"+
# /*********/
"v"+
## */*
"w"+
/*
 * Multiline, decorated comments
 * # This ain't a nest'd comm'nt
 */
"x"+
''${/** with **/"y"
  # real
  /* comments
     inside ! # */

  # (and empty lines)

}''+          /* And a multiline comment,
                 on the same line,
                 after some spaces
*/             # followed by a one-line comment
"z"
/* EOF */
```

#### Expected 

```text
"abcdefghijklmnopqrstuvwxyz"

```

### eval-okay-concat.nix

```nix
[1 2 3] ++ [4 5 6] ++ [7 8 9]
```

#### Expected 

```text
[ 1 2 3 4 5 6 7 8 9 ]

```

### eval-okay-concatmap.nix

```nix
with import ./lib.nix;

[ (builtins.concatMap (x: if x / 2 * 2 == x then [] else [ x ]) (range 0 10))
  (builtins.concatMap (x: [x] ++ ["z"]) ["a" "b"])
]
```

#### Expected 

```text
[ [ 1 3 5 7 9 ] [ "a" "z" "b" "z" ] ]

```

### eval-okay-concatstringssep.nix

```nix
with builtins;

[
  (concatStringsSep "" [])
  (concatStringsSep "" ["foo" "bar" "xyzzy"])
  (concatStringsSep ", " ["foo" "bar" "xyzzy"])
  (concatStringsSep ", " ["foo"])
  (concatStringsSep ", " [])
]
```

#### Expected 

```text
[ "" "foobarxyzzy" "foo, bar, xyzzy" "foo" "" ]

```

### eval-okay-context-introspection.nix

```nix
let
  drv = derivation {
    name = "fail";
    builder = "/bin/false";
    system = "x86_64-linux";
    outputs = [ "out" "foo" ];
  };

  path = "${./eval-okay-context-introspection.nix}";

  desired-context = {
    "${builtins.unsafeDiscardStringContext path}" = {
      path = true;
    };
    "${builtins.unsafeDiscardStringContext drv.drvPath}" = {
      outputs = [ "foo" "out" ];
      allOutputs = true;
    };
  };

  combo-path = "${path}${drv.outPath}${drv.foo.outPath}${drv.drvPath}";
  legit-context = builtins.getContext combo-path;

  reconstructed-path = builtins.appendContext
    (builtins.unsafeDiscardStringContext combo-path)
    desired-context;

  # Eta rule for strings with context.
  etaRule = str:
    str == builtins.appendContext
      (builtins.unsafeDiscardStringContext str)
      (builtins.getContext str);

  # Only holds true if string context contains both a `DrvDeep` and
  # `Opaque` element.
  almostEtaRule = str:
    str == builtins.addDrvOutputDependencies
      (builtins.unsafeDiscardOutputDependency str);

  addDrvOutputDependencies_idempotent = str:
    builtins.addDrvOutputDependencies str ==
    builtins.addDrvOutputDependencies (builtins.addDrvOutputDependencies str);

  rules = str: [
    (etaRule str)
    (almostEtaRule str)
    (addDrvOutputDependencies_idempotent str)
  ];

in [
  (legit-context == desired-context)
  (reconstructed-path == combo-path)
  (etaRule "foo")
  (etaRule drv.foo.outPath)
] ++ builtins.concatMap rules [
  drv.drvPath
  (builtins.addDrvOutputDependencies drv.drvPath)
  (builtins.unsafeDiscardOutputDependency drv.drvPath)
]
```

#### Expected 

```text
[ true true true true true true true true true true true true true ]

```

### eval-okay-context.nix

```nix
let s = "foo ${builtins.substring 33 100 (baseNameOf "${./eval-okay-context.nix}")} bar";
in
  if s != "foo eval-okay-context.nix bar"
  then abort "context not discarded"
  else builtins.unsafeDiscardStringContext s

```

#### Expected 

```text
"foo eval-okay-context.nix bar"

```

### eval-okay-convertHash.nix

```nix
let
  hashAlgos = [ "md5" "md5" "md5" "sha1" "sha1" "sha1" "sha256" "sha256" "sha256" "sha512" "sha512" "sha512" ];
  hashesBase16 = import ./eval-okay-hashstring.exp;
  map2 = f: { fsts, snds }: if fsts == [ ] then [ ] else [ (f (builtins.head fsts) (builtins.head snds)) ] ++ map2 f { fsts = builtins.tail fsts; snds = builtins.tail snds; };
  map2' = f: fsts: snds: map2 f { inherit fsts snds; };
  getOutputHashes = hashes: {
    hashesBase16 = map2' (hashAlgo: hash: builtins.convertHash { inherit hash hashAlgo; toHashFormat = "base16";}) hashAlgos hashes;
    hashesNix32 = map2' (hashAlgo: hash: builtins.convertHash { inherit hash hashAlgo; toHashFormat = "nix32";}) hashAlgos hashes;
    hashesBase32 = map2' (hashAlgo: hash: builtins.convertHash { inherit hash hashAlgo; toHashFormat = "base32";}) hashAlgos hashes;
    hashesBase64 = map2' (hashAlgo: hash: builtins.convertHash { inherit hash hashAlgo; toHashFormat = "base64";}) hashAlgos hashes;
    hashesSRI    = map2' (hashAlgo: hash: builtins.convertHash { inherit hash hashAlgo; toHashFormat = "sri"   ;}) hashAlgos hashes;
  };
  getOutputHashesColon = hashes: {
    hashesBase16 = map2' (hashAlgo: hashBody: builtins.convertHash { hash = hashAlgo + ":" + hashBody; toHashFormat = "base16";}) hashAlgos hashes;
    hashesNix32 = map2' (hashAlgo: hashBody: builtins.convertHash { hash = hashAlgo + ":" + hashBody; toHashFormat = "nix32";}) hashAlgos hashes;
    hashesBase32 = map2' (hashAlgo: hashBody: builtins.convertHash { hash = hashAlgo + ":" + hashBody; toHashFormat = "base32";}) hashAlgos hashes;
    hashesBase64 = map2' (hashAlgo: hashBody: builtins.convertHash { hash = hashAlgo + ":" + hashBody; toHashFormat = "base64";}) hashAlgos hashes;
    hashesSRI    = map2' (hashAlgo: hashBody: builtins.convertHash { hash = hashAlgo + ":" + hashBody; toHashFormat = "sri"   ;}) hashAlgos hashes;
  };
  outputHashes = getOutputHashes hashesBase16;
in
# map2'`
assert map2' (s1: s2: s1 + s2) [ "a" "b" ] [ "c" "d" ] == [ "ac" "bd" ];
# hashesBase16
assert outputHashes.hashesBase16 == hashesBase16;
# standard SRI hashes
assert outputHashes.hashesSRI == (map2' (hashAlgo: hashBody: hashAlgo + "-" + hashBody) hashAlgos outputHashes.hashesBase64);
# without prefix
assert builtins.all (x: getOutputHashes x == outputHashes) (builtins.attrValues outputHashes);
# colon-separated.
# Note that colon prefix must not be applied to the standard SRI. e.g. "sha256:sha256-..." is illegal.
assert builtins.all (x: getOutputHashesColon x == outputHashes) (with outputHashes; [ hashesBase16 hashesBase32 hashesBase64 ]);
outputHashes
```

#### Expected 

```text
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".
warning: "base32" is a deprecated alias for hash format "nix32".

```

### eval-okay-curpos.nix

```nix
# Bla
let
  x = __curPos;
    y = __curPos;
in [ x.line x.column y.line y.column ]
```

#### Expected 

```text
[ 3 7 4 9 ]

```

### eval-okay-deepseq.nix

```nix
builtins.deepSeq (let as = { x = 123; y = as; }; in as) 456
```

#### Expected 

```text
456

```

### eval-okay-delayed-with-inherit.nix

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

    inherit b;
  };

  packageOverrides = p: {
    b = derivation {
      name = "b-overridden";
      system = builtins.currentSystem;
      builder = "/bin/sh";
      args = [ "-c" "touch $out" ];
    };
  };

  pkgs = pkgs_ // (packageOverrides pkgs_);
in pkgs.a.b.name
```

#### Expected 

```text
"b-overridden"

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

### eval-okay-derivation-legacy.nix

```nix
(builtins.derivationStrict {
  name = "eval-okay-derivation-legacy";
  system = "x86_64-linux";
  builder = "/dontcare";
  __structuredAttrs = true;
  allowedReferences = [ ];
  disallowedReferences = [ ];
  allowedRequisites = [ ];
  disallowedRequisites = [ ];
  maxSize = 1234;
  maxClosureSize = 12345;
}).out
```

#### Expected 

```text
"/nix/store/mzgwvrjjir216ra58mwwizi8wj6y9ddr-eval-okay-derivation-legacy"

```

### eval-okay-dynamic-attrs-2.nix

```nix
{ a."${"b"}" = true; a."${"c"}" = false; }.a.b
```

#### Expected 

```text
true

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

### eval-okay-elem.nix

```nix
with import ./lib.nix;

let xs = range 10 40; in

[ (builtins.elem 23 xs) (builtins.elem 42 xs) (builtins.elemAt xs 20) ]

```

#### Expected 

```text
[ true false 30 ]

```

### eval-okay-empty-args.nix

```nix
({}: {x,y,}: "${x}${y}") {} {x = "a"; y = "b";}
```

#### Expected 

```text
"ab"

```

### eval-okay-eq-derivations.nix

```nix
let

  drvA1 = derivation { name = "a"; builder = "/foo"; system = "i686-linux"; };
  drvA2 = derivation { name = "a"; builder = "/foo"; system = "i686-linux"; };
  drvA3 = derivation { name = "a"; builder = "/foo"; system = "i686-linux"; } // { dummy = 1; };
  
  drvC1 = derivation { name = "c"; builder = "/foo"; system = "i686-linux"; };
  drvC2 = derivation { name = "c"; builder = "/bar"; system = "i686-linux"; };

in [ (drvA1 == drvA1) (drvA1 == drvA2) (drvA1 == drvA3) (drvC1 == drvC2) ]
```

#### Expected 

```text
[ true true true false ]

```

### eval-okay-eq.nix

```nix
["foobar" (rec {x = 1; y = x;})]
==
[("foo" + "bar") ({x = 1; y = 1;})]
```

#### Expected 

```text
[ true true true false ]

```

### eval-okay-filter.nix

```nix
with import ./lib.nix;

builtins.filter
  (x: x / 2 * 2 == x)
  (builtins.concatLists [ (range 0 10) (range 100 110) ])
```

#### Expected 

```text
[ 0 2 4 6 8 10 100 102 104 106 108 110 ]

```

### eval-okay-flake-ref-to-string.nix

```nix
builtins.flakeRefToString {
  type  = "github";
  owner = "NixOS";
  repo  = "nixpkgs";
  ref   = "23.05";
  dir   = "lib";
}
```

#### Expected 

```text
"github:NixOS/nixpkgs/23.05?dir=lib"

```

### eval-okay-flatten.nix

```nix
with import ./lib.nix;

let {

  l = ["1" "2" ["3" ["4"] ["5" "6"]] "7"];

  body = concat (flatten l);
}
```

#### Expected 

```text
"1234567"

```

### eval-okay-float.nix

```nix
[
  (1.1 + 2.3)
  (builtins.add (0.5 + 0.5) (2.0 + 0.5))
  ((0.5 + 0.5) * (2.0 + 0.5))
  ((1.5 + 1.5) / (0.5 * 4.0))
]
```

#### Expected 

```text
[ 3.4 3.5 2.5 1.5 ]

```

### eval-okay-floor-ceil.nix

```nix
with import ./lib.nix;

let
  n1 = builtins.floor 23.5;
  n2 = builtins.ceil 23.5;
  n3 = builtins.floor 23;
  n4 = builtins.ceil 23;
in
  builtins.concatStringsSep ";" (map toString [ n1 n2 n3 n4 ])
```

#### Expected 

```text
"23;24;23;23"

```

### eval-okay-foldlStrict-lazy-elements.nix

```nix
# Tests that the rhs argument of op is not forced unconditionally
let
  lst = builtins.foldl'
    (acc: x: acc ++ [ x ])
    [ ]
    [ 42 (throw "this shouldn't be evaluated") ];
in

builtins.head lst
```

#### Expected 

```text
42

```

### eval-okay-foldlStrict-lazy-initial-accumulator.nix

```nix
# Checks that the null value for the accumulator is not forced unconditionally.
# Some languages provide a foldl' that is strict in this argument, but Nix does not.
builtins.foldl'
  (_: x: x)
  (throw "This is never forced")
  [ "but the results of applying op are" 42 ]
```

#### Expected 

```text
42

```

### eval-okay-foldlStrict.nix

```nix
with import ./lib.nix;

builtins.foldl' (x: y: x + y) 0 (range 1 1000)
```

#### Expected 

```text
42

```

### eval-okay-fromTOML-timestamps.nix

```nix
builtins.fromTOML ''
  key = "value"
  bare_key = "value"
  bare-key = "value"
  1234 = "value"

  "127.0.0.1" = "value"
  "character encoding" = "value"
  "ʎǝʞ" = "value"
  'key2' = "value"
  'quoted "value"' = "value"

  name = "Orange"

  physical.color = "orange"
  physical.shape = "round"
  site."google.com" = true

  # This is legal according to the spec, but cpptoml doesn't handle it.
  #a.b.c = 1
  #a.d = 2

  str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."

  int1 = +99
  int2 = 42
  int3 = 0
  int4 = -17
  int5 = 1_000
  int6 = 5_349_221
  int7 = 1_2_3_4_5

  hex1 = 0xDEADBEEF
  hex2 = 0xdeadbeef
  hex3 = 0xdead_beef

  oct1 = 0o01234567
  oct2 = 0o755

  bin1 = 0b11010110

  flt1 = +1.0
  flt2 = 3.1415
  flt3 = -0.01
  flt4 = 5e+22
  flt5 = 1e6
  flt6 = -2E-2
  flt7 = 6.626e-34
  flt8 = 9_224_617.445_991_228_313

  bool1 = true
  bool2 = false

  odt1 = 1979-05-27T07:32:00Z
  odt2 = 1979-05-27T00:32:00-07:00
  odt3 = 1979-05-27T00:32:00.999999-07:00
  odt4 = 1979-05-27 07:32:00Z
  ldt1 = 1979-05-27T07:32:00
  ldt2 = 1979-05-27T00:32:00.999999
  ld1 = 1979-05-27
  lt1 = 07:32:00
  lt2 = 00:32:00.999999

  arr1 = [ 1, 2, 3 ]
  arr2 = [ "red", "yellow", "green" ]
  arr3 = [ [ 1, 2 ], [3, 4, 5] ]
  arr4 = [ "all", 'strings', """are the same""", ''''type'''']
  arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]

  arr7 = [
    1, 2, 3
  ]

  arr8 = [
    1,
    2, # this is ok
  ]

  [table-1]
  key1 = "some string"
  key2 = 123


  [table-2]
  key1 = "another string"
  key2 = 456

  [dog."tater.man"]
  type.name = "pug"

  [a.b.c]
  [ d.e.f ]
  [ g .  h  . i ]
  [ j . "ʞ" . 'l' ]
  [x.y.z.w]

  name = { first = "Tom", last = "Preston-Werner" }
  point = { x = 1, y = 2 }
  animal = { type.name = "pug" }

  [[products]]
  name = "Hammer"
  sku = 738594937

  [[products]]

  [[products]]
  name = "Nail"
  sku = 284758393
  color = "gray"

  [[fruit]]
    name = "apple"

    [fruit.physical]
      color = "red"
      shape = "round"

    [[fruit.variety]]
      name = "red delicious"

    [[fruit.variety]]
      name = "granny smith"

  [[fruit]]
    name = "banana"

    [[fruit.variety]]
      name = "plantain"
''
```

#### Expected 

```text
{ "1234" = "value"; "127.0.0.1" = "value"; a = { b = { c = { }; }; }; arr1 = [ 1 2 3 ]; arr2 = [ "red" "yellow" "green" ]; arr3 = [ [ 1 2 ] [ 3 4 5 ] ]; arr4 = [ "all" "strings" "are the same" "type" ]; arr5 = [ [ 1 2 ] [ "a" "b" "c" ] ]; arr7 = [ 1 2 3 ]; arr8 = [ 1 2 ]; bare-key = "value"; bare_key = "value"; bin1 = 214; bool1 = true; bool2 = false; "character encoding" = "value"; d = { e = { f = { }; }; }; dog = { "tater.man" = { type = { name = "pug"; }; }; }; flt1 = 1; flt2 = 3.1415; flt3 = -0.01; flt4 = 5e+22; flt5 = 1e+06; flt6 = -0.02; flt7 = 6.626e-34; flt8 = 9.22462e+06; fruit = [ { name = "apple"; physical = { color = "red"; shape = "round"; }; variety = [ { name = "red delicious"; } { name = "granny smith"; } ]; } { name = "banana"; variety = [ { name = "plantain"; } ]; } ]; g = { h = { i = { }; }; }; hex1 = 3735928559; hex2 = 3735928559; hex3 = 3735928559; int1 = 99; int2 = 42; int3 = 0; int4 = -17; int5 = 1000; int6 = 5349221; int7 = 12345; j = { "ʞ" = { l = { }; }; }; key = "value"; key2 = "value"; ld1 = { _type = "timestamp"; value = "1979-05-27"; }; ldt1 = { _type = "timestamp"; value = "1979-05-27T07:32:00"; }; ldt2 = { _type = "timestamp"; value = "1979-05-27T00:32:00.999999"; }; lt1 = { _type = "timestamp"; value = "07:32:00"; }; lt2 = { _type = "timestamp"; value = "00:32:00.999999"; }; name = "Orange"; oct1 = 342391; oct2 = 493; odt1 = { _type = "timestamp"; value = "1979-05-27T07:32:00Z"; }; odt2 = { _type = "timestamp"; value = "1979-05-27T00:32:00-07:00"; }; odt3 = { _type = "timestamp"; value = "1979-05-27T00:32:00.999999-07:00"; }; odt4 = { _type = "timestamp"; value = "1979-05-27T07:32:00Z"; }; physical = { color = "orange"; shape = "round"; }; products = [ { name = "Hammer"; sku = 738594937; } { } { color = "gray"; name = "Nail"; sku = 284758393; } ]; "quoted \"value\"" = "value"; site = { "google.com" = true; }; str = "I'm a string. \"You can quote me\". Name\tJosé\nLocation\tSF."; table-1 = { key1 = "some string"; key2 = 123; }; table-2 = { key1 = "another string"; key2 = 456; }; x = { y = { z = { w = { animal = { type = { name = "pug"; }; }; name = { first = "Tom"; last = "Preston-Werner"; }; point = { x = 1; y = 2; }; }; }; }; }; "ʎǝʞ" = "value"; }

```

### eval-okay-fromTOML.nix

```nix
[

  (builtins.fromTOML ''
    # This is a TOML document.

    title = "TOML Example"

    [owner]
    name = "Tom Preston-Werner"
    #dob = 1979-05-27T07:32:00-08:00 # First class dates

    [database]
    server = "192.168.1.1"
    ports = [ 8001, 8001, 8002 ]
    connection_max = 5000
    enabled = true

    [servers]

      # Indentation (tabs and/or spaces) is allowed but not required
      [servers.alpha]
      ip = "10.0.0.1"
      dc = "eqdc10"

      [servers.beta]
      ip = "10.0.0.2"
      dc = "eqdc10"

    [clients]
    data = [ ["gamma", "delta"], [1, 2] ]

    # Line breaks are OK when inside arrays
    hosts = [
      "alpha",
      "omega"
    ]
  '')

  (builtins.fromTOML ''
    key = "value"
    bare_key = "value"
    bare-key = "value"
    1234 = "value"

    "127.0.0.1" = "value"
    "character encoding" = "value"
    "ʎǝʞ" = "value"
    'key2' = "value"
    'quoted "value"' = "value"

    name = "Orange"

    physical.color = "orange"
    physical.shape = "round"
    site."google.com" = true

    # This is legal according to the spec, but cpptoml doesn't handle it.
    #a.b.c = 1
    #a.d = 2

    str = "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."

    int1 = +99
    int2 = 42
    int3 = 0
    int4 = -17
    int5 = 1_000
    int6 = 5_349_221
    int7 = 1_2_3_4_5

    hex1 = 0xDEADBEEF
    hex2 = 0xdeadbeef
    hex3 = 0xdead_beef

    oct1 = 0o01234567
    oct2 = 0o755

    bin1 = 0b11010110

    flt1 = +1.0
    flt2 = 3.1415
    flt3 = -0.01
    flt4 = 5e+22
    flt5 = 1e6
    flt6 = -2E-2
    flt7 = 6.626e-34
    flt8 = 9_224_617.445_991_228_313

    bool1 = true
    bool2 = false

    # FIXME: not supported because Nix doesn't have a date/time type.
    #odt1 = 1979-05-27T07:32:00Z
    #odt2 = 1979-05-27T00:32:00-07:00
    #odt3 = 1979-05-27T00:32:00.999999-07:00
    #odt4 = 1979-05-27 07:32:00Z
    #ldt1 = 1979-05-27T07:32:00
    #ldt2 = 1979-05-27T00:32:00.999999
    #ld1 = 1979-05-27
    #lt1 = 07:32:00
    #lt2 = 00:32:00.999999

    arr1 = [ 1, 2, 3 ]
    arr2 = [ "red", "yellow", "green" ]
    arr3 = [ [ 1, 2 ], [3, 4, 5] ]
    arr4 = [ "all", 'strings', """are the same""", ''''type'''']
    arr5 = [ [ 1, 2 ], ["a", "b", "c"] ]

    arr7 = [
      1, 2, 3
    ]

    arr8 = [
      1,
      2, # this is ok
    ]

    [table-1]
    key1 = "some string"
    key2 = 123


    [table-2]
    key1 = "another string"
    key2 = 456

    [dog."tater.man"]
    type.name = "pug"

    [a.b.c]
    [ d.e.f ]
    [ g .  h  . i ]
    [ j . "ʞ" . 'l' ]
    [x.y.z.w]

    name = { first = "Tom", last = "Preston-Werner" }
    point = { x = 1, y = 2 }
    animal = { type.name = "pug" }

    [[products]]
    name = "Hammer"
    sku = 738594937

    [[products]]

    [[products]]
    name = "Nail"
    sku = 284758393
    color = "gray"

    [[fruit]]
      name = "apple"

      [fruit.physical]
        color = "red"
        shape = "round"

      [[fruit.variety]]
        name = "red delicious"

      [[fruit.variety]]
        name = "granny smith"

    [[fruit]]
      name = "banana"

      [[fruit.variety]]
        name = "plantain"
  '')

  (builtins.fromTOML ''
    [[package]]
    name = "aho-corasick"
    version = "0.6.4"
    source = "registry+https://github.com/rust-lang/crates.io-index"
    dependencies = [
     "memchr 2.0.1 (registry+https://github.com/rust-lang/crates.io-index)",
    ]

    [[package]]
    name = "ansi_term"
    version = "0.9.0"
    source = "registry+https://github.com/rust-lang/crates.io-index"

    [[package]]
    name = "atty"
    version = "0.2.10"
    source = "registry+https://github.com/rust-lang/crates.io-index"
    dependencies = [
     "libc 0.2.42 (registry+https://github.com/rust-lang/crates.io-index)",
     "termion 1.5.1 (registry+https://github.com/rust-lang/crates.io-index)",
     "winapi 0.3.5 (registry+https://github.com/rust-lang/crates.io-index)",
    ]

    [metadata]
    "checksum aho-corasick 0.6.4 (registry+https://github.com/rust-lang/crates.io-index)" = "d6531d44de723825aa81398a6415283229725a00fa30713812ab9323faa82fc4"
    "checksum ansi_term 0.11.0 (registry+https://github.com/rust-lang/crates.io-index)" = "ee49baf6cb617b853aa8d93bf420db2383fab46d314482ca2803b40d5fde979b"
    "checksum ansi_term 0.9.0 (registry+https://github.com/rust-lang/crates.io-index)" = "23ac7c30002a5accbf7e8987d0632fa6de155b7c3d39d0067317a391e00a2ef6"
    "checksum arrayvec 0.4.7 (registry+https://github.com/rust-lang/crates.io-index)" = "a1e964f9e24d588183fcb43503abda40d288c8657dfc27311516ce2f05675aef"
  '')

  (builtins.fromTOML ''
    a = [[{ b = true }]]
    c = [ [ { d = true } ] ]
    e = [[123]]
  '')

]
```

#### Expected 

```text
{ "1234" = "value"; "127.0.0.1" = "value"; a = { b = { c = { }; }; }; arr1 = [ 1 2 3 ]; arr2 = [ "red" "yellow" "green" ]; arr3 = [ [ 1 2 ] [ 3 4 5 ] ]; arr4 = [ "all" "strings" "are the same" "type" ]; arr5 = [ [ 1 2 ] [ "a" "b" "c" ] ]; arr7 = [ 1 2 3 ]; arr8 = [ 1 2 ]; bare-key = "value"; bare_key = "value"; bin1 = 214; bool1 = true; bool2 = false; "character encoding" = "value"; d = { e = { f = { }; }; }; dog = { "tater.man" = { type = { name = "pug"; }; }; }; flt1 = 1; flt2 = 3.1415; flt3 = -0.01; flt4 = 5e+22; flt5 = 1e+06; flt6 = -0.02; flt7 = 6.626e-34; flt8 = 9.22462e+06; fruit = [ { name = "apple"; physical = { color = "red"; shape = "round"; }; variety = [ { name = "red delicious"; } { name = "granny smith"; } ]; } { name = "banana"; variety = [ { name = "plantain"; } ]; } ]; g = { h = { i = { }; }; }; hex1 = 3735928559; hex2 = 3735928559; hex3 = 3735928559; int1 = 99; int2 = 42; int3 = 0; int4 = -17; int5 = 1000; int6 = 5349221; int7 = 12345; j = { "ʞ" = { l = { }; }; }; key = "value"; key2 = "value"; ld1 = { _type = "timestamp"; value = "1979-05-27"; }; ldt1 = { _type = "timestamp"; value = "1979-05-27T07:32:00"; }; ldt2 = { _type = "timestamp"; value = "1979-05-27T00:32:00.999999"; }; lt1 = { _type = "timestamp"; value = "07:32:00"; }; lt2 = { _type = "timestamp"; value = "00:32:00.999999"; }; name = "Orange"; oct1 = 342391; oct2 = 493; odt1 = { _type = "timestamp"; value = "1979-05-27T07:32:00Z"; }; odt2 = { _type = "timestamp"; value = "1979-05-27T00:32:00-07:00"; }; odt3 = { _type = "timestamp"; value = "1979-05-27T00:32:00.999999-07:00"; }; odt4 = { _type = "timestamp"; value = "1979-05-27T07:32:00Z"; }; physical = { color = "orange"; shape = "round"; }; products = [ { name = "Hammer"; sku = 738594937; } { } { color = "gray"; name = "Nail"; sku = 284758393; } ]; "quoted \"value\"" = "value"; site = { "google.com" = true; }; str = "I'm a string. \"You can quote me\". Name\tJosé\nLocation\tSF."; table-1 = { key1 = "some string"; key2 = 123; }; table-2 = { key1 = "another string"; key2 = 456; }; x = { y = { z = { w = { animal = { type = { name = "pug"; }; }; name = { first = "Tom"; last = "Preston-Werner"; }; point = { x = 1; y = 2; }; }; }; }; }; "ʎǝʞ" = "value"; }

```

### eval-okay-fromjson-escapes.nix

```nix
# This string contains all supported escapes in a JSON string, per json.org
# \b and \f are not supported by Nix
builtins.fromJSON ''"quote \" reverse solidus \\ solidus \/ backspace \b formfeed \f newline \n carriage return \r horizontal tab \t 1 char unicode encoded backspace \u0008 1 char unicode encoded e with accent \u00e9 2 char unicode encoded s with caron \u0161 3 char unicode encoded rightwards arrow \u2192"''
```

#### Expected 

```text
"quote \" reverse solidus \\ solidus / backspace  formfeed  newline \n carriage return \r horizontal tab \t 1 char unicode encoded backspace  1 char unicode encoded e with accent é 2 char unicode encoded s with caron š 3 char unicode encoded rightwards arrow →"

```

### eval-okay-fromjson.nix

```nix
builtins.fromJSON
  ''
    {
      "Video": {
          "Title":  "The Penguin Chronicles",
          "Width":  1920,
          "Height": 1080,
          "EmbeddedData": [3.14159, 23493,null, true  ,false, -10],
          "Thumb": {
              "Url":    "http://www.example.com/video/5678931",
              "Width":  200,
              "Height": 250
          },
          "Animated" : false,
          "IDs": [116, 943, 234, 38793, true  ,false,null, -100],
          "Escapes": "\"\\\/\t\n\r\t",
          "Subtitle" : false,
          "Latitude":  37.7668,
          "Longitude": -122.3959
        }
    }
  ''
==
  { Video =
    { Title = "The Penguin Chronicles";
      Width = 1920;
      Height = 1080;
      EmbeddedData = [ 3.14159 23493 null true false (0-10) ];
      Thumb =
        { Url = "http://www.example.com/video/5678931";
          Width = 200;
          Height = 250;
        };
      Animated = false;
      IDs = [ 116 943 234 38793 true false null (0-100) ];
      Escapes = "\"\\\/\t\n\r\t";  # supported in JSON but not Nix: \b\f
      Subtitle = false;
      Latitude = 37.7668;
      Longitude = -122.3959;
    };
  }
```

#### Expected 

```text
"quote \" reverse solidus \\ solidus / backspace  formfeed  newline \n carriage return \r horizontal tab \t 1 char unicode encoded backspace  1 char unicode encoded e with accent é 2 char unicode encoded s with caron š 3 char unicode encoded rightwards arrow →"

```

### eval-okay-functionargs.nix

```nix
let

  stdenvFun = { }: { name = "stdenv"; };
  stdenv2Fun = { }: { name = "stdenv2"; };
  fetchurlFun = { stdenv }: assert stdenv.name == "stdenv"; { name = "fetchurl"; };
  atermFun = { stdenv, fetchurl }: { name = "aterm-${stdenv.name}"; };
  aterm2Fun = { stdenv, fetchurl }: { name = "aterm2-${stdenv.name}"; };
  nixFun = { stdenv, fetchurl, aterm }: { name = "nix-${stdenv.name}-${aterm.name}"; };
  
  mplayerFun =
    { stdenv, fetchurl, enableX11 ? false, xorg ? null, enableFoo ? true, foo ? null  }:
    assert stdenv.name == "stdenv2";
    assert enableX11 -> xorg.libXv.name == "libXv";
    assert enableFoo -> foo != null;
    { name = "mplayer-${stdenv.name}.${xorg.libXv.name}-${xorg.libX11.name}"; };

  makeOverridable = f: origArgs: f origArgs //
    { override = newArgs:
        makeOverridable f (origArgs // (if builtins.isFunction newArgs then newArgs origArgs else newArgs));
    };
    
  callPackage_ = pkgs: f: args:
    makeOverridable f ((builtins.intersectAttrs (builtins.functionArgs f) pkgs) // args);

  allPackages =
    { overrides ? (pkgs: pkgsPrev: { }) }:
    let
      callPackage = callPackage_ pkgs;
      pkgs = pkgsStd // (overrides pkgs pkgsStd);
      pkgsStd = {
        inherit pkgs;
        stdenv = callPackage stdenvFun { };
        stdenv2 = callPackage stdenv2Fun { };
        fetchurl = callPackage fetchurlFun { };
        aterm = callPackage atermFun { };
        xorg = callPackage xorgFun { };
        mplayer = callPackage mplayerFun { stdenv = pkgs.stdenv2; enableFoo = false; };
        nix = callPackage nixFun { };
      };
    in pkgs;

  libX11Fun = { stdenv, fetchurl }: { name = "libX11"; };
  libX11_2Fun = { stdenv, fetchurl }: { name = "libX11_2"; };
  libXvFun = { stdenv, fetchurl, libX11 }: { name = "libXv"; };
  
  xorgFun =
    { pkgs }:
    let callPackage = callPackage_ (pkgs // pkgs.xorg); in
    {
      libX11 = callPackage libX11Fun { };
      libXv = callPackage libXvFun { };
    };

in

let

  pkgs = allPackages { };
  
  pkgs2 = allPackages {
    overrides = pkgs: pkgsPrev: {
      stdenv = pkgs.stdenv2;
      nix = pkgsPrev.nix.override { aterm = aterm2Fun { inherit (pkgs) stdenv fetchurl; }; };
      xorg = pkgsPrev.xorg // { libX11 = libX11_2Fun { inherit (pkgs) stdenv fetchurl; }; };
    };
  };
  
in

  [ pkgs.stdenv.name
    pkgs.fetchurl.name
    pkgs.aterm.name
    pkgs2.aterm.name
    pkgs.xorg.libX11.name
    pkgs.xorg.libXv.name
    pkgs.mplayer.name
    pkgs2.mplayer.name
    pkgs.nix.name
    pkgs2.nix.name
  ]
```

#### Expected 

```text
[ "stdenv" "fetchurl" "aterm-stdenv" "aterm-stdenv2" "libX11" "libXv" "mplayer-stdenv2.libXv-libX11" "mplayer-stdenv2.libXv-libX11_2" "nix-stdenv-aterm-stdenv" "nix-stdenv2-aterm2-stdenv2" ]

```

### eval-okay-getattrpos-functionargs.nix

```nix
let
  fun = { foo }: {};
  pos = builtins.unsafeGetAttrPos "foo" (builtins.functionArgs fun);
in { inherit (pos) column line; file = baseNameOf pos.file; }
```

#### Expected 

```text
{ column = 11; file = "eval-okay-getattrpos-functionargs.nix"; line = 2; }

```

### eval-okay-getattrpos-undefined.nix

```nix
builtins.unsafeGetAttrPos "abort" builtins
```

#### Expected 

```text
null

```

### eval-okay-getattrpos.nix

```nix
let
  as = {
    foo = "bar";
  };
  pos = builtins.unsafeGetAttrPos "foo" as;
in { inherit (pos) column line; file = baseNameOf pos.file; }
```

#### Expected 

```text
{ column = 11; file = "eval-okay-getattrpos-functionargs.nix"; line = 2; }

```

### eval-okay-getenv.nix

```nix
builtins.getEnv "TEST_VAR" + (if builtins.getEnv "NO_SUCH_VAR" == "" then "bar" else "bla")
```

#### Expected 

```text
"foobar"

```

### eval-okay-groupBy.nix

```nix
with import ./lib.nix;

builtins.groupBy (n:
  builtins.substring 0 1 (builtins.hashString "sha256" (toString n))
) (range 0 31)
```

#### Expected 

```text
{ "1" = [ 9 ]; "2" = [ 8 ]; "3" = [ 13 29 ]; "4" = [ 3 4 10 11 17 18 ]; "5" = [ 0 23 26 28 ]; "6" = [ 1 12 21 27 30 ]; "7" = [ 7 22 ]; "8" = [ 14 ]; "9" = [ 19 ]; b = [ 16 25 ]; c = [ 24 ]; d = [ 2 ]; e = [ 5 6 15 31 ]; f = [ 20 ]; }

```

### eval-okay-hashfile.nix

```nix
let
  paths = [ ./data ./binary-data ];
in
  builtins.concatLists (map (hash: map (builtins.hashFile hash) paths) ["md5" "sha1" "sha256" "sha512"])
```

#### Expected 

```text
[ "d3b07384d113edec49eaa6238ad5ff00" "0f343b0931126a20f133d67c2b018a3b" "f1d2d2f924e986ac86fdf7b36c94bcdf32beec15" "60cacbf3d72e1e7834203da608037b1bf83b40e8" "b5bb9d8014a0f9b1d61e21e796d78dccdf1352f23cd32812f4850b878ae4944c" "5f70bf18a086007016e948b04aed3b82103a36bea41755b6cddfaf10ace3c6ef" "0cf9180a764aba863a67b6d72f0918bc131c6772642cb2dce5a34f0a702f9470ddc2bf125c12198b1995c233c34b4afd346c54a2334c350a948a51b6e8b4e6b6" "8efb4f73c5655351c444eb109230c556d39e2c7624e9c11abc9e3fb4b9b9254218cc5085b454a9698d085cfa92198491f07a723be4574adc70617b73eb0b6461" ]

```

### eval-okay-hashstring.nix

```nix
let
  strings = [ "" "text 1" "text 2" ];
in
  builtins.concatLists (map (hash: map (builtins.hashString hash) strings) ["md5" "sha1" "sha256" "sha512"])
```

#### Expected 

```text
[ "d41d8cd98f00b204e9800998ecf8427e" "6c69ee7f211c640419d5366cc076ae46" "bb3438fbabd460ea6dbd27d153e2233b" "da39a3ee5e6b4b0d3255bfef95601890afd80709" "cd54e8568c1b37cf1e5badb0779bcbf382212189" "6d12e10b1d331dad210e47fd25d4f260802b7e77" "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855" "900a4469df00ccbfd0c145c6d1e4b7953dd0afafadd7534e3a4019e8d38fc663" "ad0387b3bd8652f730ca46d25f9c170af0fd589f42e7f23f5a9e6412d97d7e56" "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e" "9d0886f8c6b389398a16257bc79780fab9831c7fc11c8ab07fa732cb7b348feade382f92617c9c5305fefba0af02ab5fd39a587d330997ff5bd0db19f7666653" "21644b72aa259e5a588cd3afbafb1d4310f4889680f6c83b9d531596a5a284f34dbebff409d23bcc86aee6bad10c891606f075c6f4755cb536da27db5693f3a7" ]

```

### eval-okay-if.nix

```nix
if "foo" != "f" + "oo" then 1 else if false then 2 else 3
```

#### Expected 

```text
3

```

### eval-okay-import.nix

```nix
let

  overrides = {
    import = fn: scopedImport overrides fn;

    scopedImport = attrs: fn: scopedImport (overrides // attrs) fn;

    builtins = builtins // overrides;
  } // import ./lib.nix;

in scopedImport overrides ./imported.nix
```

#### Expected 

```text
[ 1 2 3 4 5 6 7 8 9 10 ]

```

### eval-okay-ind-string.nix

```nix
let

  s1 = ''
    This is an indented multi-line string
    literal.  An amount of whitespace at
    the start of each line matching the minimum
    indentation of all lines in the string
    literal together will be removed.  Thus,
    in this case four spaces will be
    stripped from each line, even though
      THIS LINE is indented six spaces.

    Also, empty lines don't count in the
    determination of the indentation level (the
    previous empty line has indentation 0, but
    it doesn't matter).
  '';

  s2 = ''  If the string starts with whitespace
    followed by a newline, it's stripped, but
    that's not the case here. Two spaces are
    stripped because of the "  " at the start. 
  '';

  s3 = ''
      This line is indented
      a bit further.
        ''; # indentation of last line doesn't count if it's empty

  s4 = ''
    Anti-quotations, like ${if true then "so" else "not so"}, are
    also allowed.
  '';

  s5 = ''
      The \ is not special here.
    ' can be followed by any character except another ', e.g. 'x'.
    Likewise for $, e.g. $$ or $varName.
    But ' followed by ' is special, as is $ followed by {.
    If you want them, use anti-quotations: ${"''"}, ${"\${"}.
  '';

  s6 = ''  
    Tabs are not interpreted as whitespace (since we can't guess
    what tab settings are intended), so don't use them.
 	This line starts with a space and a tab, so only one
    space will be stripped from each line.
  '';

  s7 = ''
    Also note that if the last line (just before the closing ' ')
    consists only of whitespace, it's ignored.  But here there is
    some non-whitespace stuff, so the line isn't removed. '';

  s8 = ''    ${""}
    This shows a hacky way to preserve an empty line after the start.
    But there's no reason to do so: you could just repeat the empty
    line.
  '';

  s9 = ''
  ${""}  Similarly you can force an indentation level,
    in this case to 2 spaces.  This works because the anti-quote
    is significant (not whitespace).
  '';

  s10 = ''
  '';

  s11 = '''';

  s12 = ''   '';

  s13 = ''
    start on network-interfaces

    start script
    
      rm -f /var/run/opengl-driver
      ${if true
        then "ln -sf 123 /var/run/opengl-driver"
        else if true
        then "ln -sf 456 /var/run/opengl-driver"
        else ""
      }

      rm -f /var/log/slim.log
       
    end script

    env SLIM_CFGFILE=${"abc"}
    env SLIM_THEMESDIR=${"def"}
    env FONTCONFIG_FILE=/etc/fonts/fonts.conf  				# !!! cleanup
    env XKB_BINDIR=${"foo"}/bin         				# Needed for the Xkb extension.
    env LD_LIBRARY_PATH=${"libX11"}/lib:${"libXext"}/lib:/usr/lib/          # related to xorg-sys-opengl - needed to load libglx for (AI)GLX support (for compiz)

    ${if true
      then "env XORG_DRI_DRIVER_PATH=${"nvidiaDrivers"}/X11R6/lib/modules/drivers/"
    else if true
      then "env XORG_DRI_DRIVER_PATH=${"mesa"}/lib/modules/dri"
      else ""
    } 

    exec ${"slim"}/bin/slim
  '';

  s14 = ''
    Escaping of ' followed by ': '''
    Escaping of $ followed by {: ''${
    And finally to interpret \n etc. as in a string: ''\n, ''\r, ''\t.
  '';

  # Regression test: string interpolation in '${x}' should work, but didn't.
  s15 = let x = "bla"; in ''
    foo
    '${x}'
    bar
  '';

  # Regression test: accept $'.
  s16 = ''
    cut -d $'\t' -f 1
  '';

  # Accept dollars at end of strings 
  s17 = ''ending dollar $'' + ''$'' + "\n";

in s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11 + s12 + s13 + s14 + s15 + s16 + s17
```

#### Expected 

```text
"This is an indented multi-line string\nliteral.  An amount of whitespace at\nthe start of each line matching the minimum\nindentation of all lines in the string\nliteral together will be removed.  Thus,\nin this case four spaces will be\nstripped from each line, even though\n  THIS LINE is indented six spaces.\n\nAlso, empty lines don't count in the\ndetermination of the indentation level (the\nprevious empty line has indentation 0, but\nit doesn't matter).\nIf the string starts with whitespace\n  followed by a newline, it's stripped, but\n  that's not the case here. Two spaces are\n  stripped because of the \"  \" at the start. \nThis line is indented\na bit further.\nAnti-quotations, like so, are\nalso allowed.\n  The \\ is not special here.\n' can be followed by any character except another ', e.g. 'x'.\nLikewise for $, e.g. $$ or $varName.\nBut ' followed by ' is special, as is $ followed by {.\nIf you want them, use anti-quotations: '', \${.\n   Tabs are not interpreted as whitespace (since we can't guess\n   what tab settings are intended), so don't use them.\n\tThis line starts with a space and a tab, so only one\n   space will be stripped from each line.\nAlso note that if the last line (just before the closing ' ')\nconsists only of whitespace, it's ignored.  But here there is\nsome non-whitespace stuff, so the line isn't removed. \nThis shows a hacky way to preserve an empty line after the start.\nBut there's no reason to do so: you could just repeat the empty\nline.\n  Similarly you can force an indentation level,\n  in this case to 2 spaces.  This works because the anti-quote\n  is significant (not whitespace).\nstart on network-interfaces\n\nstart script\n\n  rm -f /var/run/opengl-driver\n  ln -sf 123 /var/run/opengl-driver\n\n  rm -f /var/log/slim.log\n   \nend script\n\nenv SLIM_CFGFILE=abc\nenv SLIM_THEMESDIR=def\nenv FONTCONFIG_FILE=/etc/fonts/fonts.conf  \t\t\t\t# !!! cleanup\nenv XKB_BINDIR=foo/bin         \t\t\t\t# Needed for the Xkb extension.\nenv LD_LIBRARY_PATH=libX11/lib:libXext/lib:/usr/lib/          # related to xorg-sys-opengl - needed to load libglx for (AI)GLX support (for compiz)\n\nenv XORG_DRI_DRIVER_PATH=nvidiaDrivers/X11R6/lib/modules/drivers/ \n\nexec slim/bin/slim\nEscaping of ' followed by ': ''\nEscaping of $ followed by {: \${\nAnd finally to interpret \\n etc. as in a string: \n, \r, \t.\nfoo\n'bla'\nbar\ncut -d $'\\t' -f 1\nending dollar $$\n"

```

### eval-okay-inherit-attr-pos.nix

```nix
let
  d = 0;
  x = 1;
  y = { inherit d x; };
  z = { inherit (y) d x; };
in
  [
    (builtins.unsafeGetAttrPos "d" y)
    (builtins.unsafeGetAttrPos "x" y)
    (builtins.unsafeGetAttrPos "d" z)
    (builtins.unsafeGetAttrPos "x" z)
  ]
```

#### Expected 

```text
[ { column = 17; file = "/pwd/lang/eval-okay-inherit-attr-pos.nix"; line = 4; } { column = 19; file = "/pwd/lang/eval-okay-inherit-attr-pos.nix"; line = 4; } { column = 21; file = "/pwd/lang/eval-okay-inherit-attr-pos.nix"; line = 5; } { column = 23; file = "/pwd/lang/eval-okay-inherit-attr-pos.nix"; line = 5; } ]

```

### eval-okay-inherit-from.nix

```nix
let
  inherit (builtins.trace "used" { a = 1; b = 2; }) a b;
  x.c = 3;
  y.d = 4;

  merged = {
    inner = {
      inherit (y) d;
    };

    inner = {
      inherit (x) c;
    };
  };
in
  [ a b rec { x.c = []; inherit (x) c; inherit (y) d; __overrides.y.d = []; } merged ]
```

#### Expected 

```text
trace: used

```

### eval-okay-intersectAttrs.nix

```nix
let
  alphabet =
  { a = "a";
    b = "b";
    c = "c";
    d = "d";
    e = "e";
    f = "f";
    g = "g";
    h = "h";
    i = "i";
    j = "j";
    k = "k";
    l = "l";
    m = "m";
    n = "n";
    o = "o";
    p = "p";
    q = "q";
    r = "r";
    s = "s";
    t = "t";
    u = "u";
    v = "v";
    w = "w";
    x = "x";
    y = "y";
    z = "z";
  };
  foo = {
    inherit (alphabet) f o b a r z q u x;
    aa = throw "aa";
  };
  alphabetFail = builtins.mapAttrs throw alphabet;
in
[ (builtins.intersectAttrs { a = abort "l1"; } { b = abort "r1"; })
  (builtins.intersectAttrs { a = abort "l2"; } { a = 1; })
  (builtins.intersectAttrs alphabetFail { a = 1; })
  (builtins.intersectAttrs  { a = abort "laa"; } alphabet)
  (builtins.intersectAttrs alphabetFail { m = 1; })
  (builtins.intersectAttrs  { m = abort "lam"; } alphabet)
  (builtins.intersectAttrs alphabetFail { n = 1; })
  (builtins.intersectAttrs  { n = abort "lan"; } alphabet)
  (builtins.intersectAttrs alphabetFail { n = 1; p = 2; })
  (builtins.intersectAttrs  { n = abort "lan2"; p = abort "lap"; } alphabet)
  (builtins.intersectAttrs alphabetFail { n = 1; p = 2; })
  (builtins.intersectAttrs  { n = abort "lan2"; p = abort "lap"; } alphabet)
  (builtins.intersectAttrs alphabetFail alphabet)
  (builtins.intersectAttrs alphabet foo == builtins.intersectAttrs foo alphabet)
]
```

#### Expected 

```text
[ { } { a = 1; } { a = 1; } { a = "a"; } { m = 1; } { m = "m"; } { n = 1; } { n = "n"; } { n = 1; p = 2; } { n = "n"; p = "p"; } { n = 1; p = 2; } { n = "n"; p = "p"; } { a = "a"; b = "b"; c = "c"; d = "d"; e = "e"; f = "f"; g = "g"; h = "h"; i = "i"; j = "j"; k = "k"; l = "l"; m = "m"; n = "n"; o = "o"; p = "p"; q = "q"; r = "r"; s = "s"; t = "t"; u = "u"; v = "v"; w = "w"; x = "x"; y = "y"; z = "z"; } true ]

```

### eval-okay-let.nix

```nix
let {
  x = "foo";
  y = "bar";
  body = x + y;
}
```

#### Expected 

```text
"foobar"

```

### eval-okay-list.nix

```nix
with import ./lib.nix;

let {

  body = concat ["foo" "bar" "bla" "test"];
    
}
```

#### Expected 

```text
"foobarblatest"

```

### eval-okay-listtoattrs.nix

```nix
# this test shows how to use listToAttrs and that evaluation is still lazy (throw isn't called)
with import ./lib.nix;

let 
  asi = name: value : { inherit name value; };
  list = [ ( asi "a" "A" ) ( asi "b" "B" ) ];
  a = builtins.listToAttrs list;
  b = builtins.listToAttrs ( list ++ list );
  r = builtins.listToAttrs [ (asi "result" [ a b ]) ( asi "throw" (throw "this should not be thrown")) ];
  x = builtins.listToAttrs [ (asi "foo" "bar") (asi "foo" "bla") ];
in concat (map (x: x.a) r.result) + x.foo
```

#### Expected 

```text
"AAbar"

```

### eval-okay-logic.nix

```nix
assert !false && (true || false) -> true; 1
```

#### Expected 

```text
1

```

### eval-okay-map.nix

```nix
with import ./lib.nix;

concat (map (x: x + "bar") [ "foo" "bla" "xyzzy" ])
```

#### Expected 

```text
"foobarblabarxyzzybar"

```

### eval-okay-mapattrs.nix

```nix
with import ./lib.nix;

builtins.mapAttrs (name: value: name + "-" + value) { x = "foo"; y = "bar"; }
```

#### Expected 

```text
{ x = "x-foo"; y = "y-bar"; }

```

### eval-okay-merge-dynamic-attrs.nix

```nix
{
  set1 = { a = 1; };
  set1 = { "${"b" + ""}" = 2; };

  set2 = { "${"b" + ""}" = 2; };
  set2 = { a = 1; };

  set3.a = 1;
  set3."${"b" + ""}" = 2;

  set4."${"b" + ""}" = 2;
  set4.a = 1;
}
```

#### Expected 

```text
{ set1 = { a = 1; b = 2; }; set2 = { a = 1; b = 2; }; set3 = { a = 1; b = 2; }; set4 = { a = 1; b = 2; }; }

```

### eval-okay-nested-with.nix

```nix
with { x = 1; };
with { x = 2; };
x
```

#### Expected 

```text
2

```

### eval-okay-new-let.nix

```nix
let

  f = z: 

    let
      x = "foo";
      y = "bar";
      body = 1; # compat test
    in
      z + x + y;

  arg = "xyzzy";

in f arg
```

#### Expected 

```text
"xyzzyfoobar"

```

### eval-okay-null-dynamic-attrs.nix

```nix
{ ${null} = true; } == {}
```

#### Expected 

```text
true

```

### eval-okay-overrides.nix

```nix
let

  overrides = { a = 2; b = 3; };

in (rec {
  __overrides = overrides;
  x = a;
  a = 1;
}).x
```

#### Expected 

```text
2

```

### eval-okay-parse-flake-ref.nix

```nix
  builtins.parseFlakeRef "github:NixOS/nixpkgs/23.05?dir=lib"
```

#### Expected 

```text
{ dir = "lib"; owner = "NixOS"; ref = "23.05"; repo = "nixpkgs"; type = "github"; }

```

### eval-okay-partition.nix

```nix
with import ./lib.nix;

builtins.partition
  (x: x / 2 * 2 == x)
  (builtins.concatLists [ (range 0 10) (range 100 110) ])
```

#### Expected 

```text
{ right = [ 0 2 4 6 8 10 100 102 104 106 108 110 ]; wrong = [ 1 3 5 7 9 101 103 105 107 109 ]; }

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

#### Expected 

```text
{ absolute = /foo; expr = /pwd/lang/foo/bar; home = /fake-home/foo; notfirst = /pwd/lang/bar/foo; simple = /pwd/lang/foo; slashes = /foo/bar; surrounded = /pwd/lang/a-foo-b; }

```

### eval-okay-path.nix

```nix
[
  (builtins.path
    { path = ./.;
      filter = path: _: baseNameOf path == "data";
      recursive = true;
      sha256 = "1yhm3gwvg5a41yylymgblsclk95fs6jy72w0wv925mmidlhcq4sw";
      name = "output";
    })
  (builtins.path
    { path = ./data;
      recursive = false;
      sha256 = "0k4lwj58f2w5yh92ilrwy9917pycipbrdrr13vbb3yd02j09vfxm";
      name = "output";
    })
]
```

#### Expected 

```text
[ "/nix/store/ya937r4ydw0l6kayq8jkyqaips9c75jm-output" "/nix/store/m7y372g6jb0g4hh1dzmj847rd356fhnz-output" ]

```

### eval-okay-pathexists.nix

```nix
builtins.pathExists (./lib.nix)
&& builtins.pathExists (builtins.toPath ./lib.nix)
&& builtins.pathExists (builtins.toString ./lib.nix)
&& !builtins.pathExists (builtins.toString ./lib.nix + "/")
&& !builtins.pathExists (builtins.toString ./lib.nix + "/.")
# FIXME
# && !builtins.pathExists (builtins.toString ./lib.nix + "/..")
# && !builtins.pathExists (builtins.toString ./lib.nix + "/a/..")
# && !builtins.pathExists (builtins.toString ./lib.nix + "/../lib.nix")
&& !builtins.pathExists (builtins.toString ./lib.nix + "/./")
&& !builtins.pathExists (builtins.toString ./lib.nix + "/./.")
&& builtins.pathExists (builtins.toString ./.. + "/lang/lib.nix")
&& !builtins.pathExists (builtins.toString ./.. + "lang/lib.nix")
&& builtins.pathExists (builtins.toString ./. + "/../lang/lib.nix")
&& builtins.pathExists (builtins.toString ./. + "/../lang/./lib.nix")
&& builtins.pathExists (builtins.toString ./.)
&& builtins.pathExists (builtins.toString ./. + "/")
&& builtins.pathExists (builtins.toString ./. + "/../lang")
&& builtins.pathExists (builtins.toString ./. + "/../lang/")
&& builtins.pathExists (builtins.toString ./. + "/../lang/.")
&& builtins.pathExists (builtins.toString ./. + "/../lang/./")
&& builtins.pathExists (builtins.toString ./. + "/../lang//./")
&& builtins.pathExists (builtins.toString ./. + "/../lang/..")
&& builtins.pathExists (builtins.toString ./. + "/../lang/../")
&& builtins.pathExists (builtins.toString ./. + "/../lang/..//")
&& builtins.pathExists (builtins.toPath (builtins.toString ./lib.nix))
&& !builtins.pathExists (builtins.toPath (builtins.toString ./bla.nix))
&& builtins.pathExists (builtins.toPath { __toString = x: builtins.toString ./lib.nix; })
&& builtins.pathExists (builtins.toPath { outPath = builtins.toString ./lib.nix; })
&& builtins.pathExists ./lib.nix
&& !builtins.pathExists ./bla.nix
&& builtins.pathExists ./symlink-resolution/foo/overlays/overlay.nix
&& builtins.pathExists ./symlink-resolution/broken
&& builtins.pathExists (builtins.toString ./symlink-resolution/foo/overlays + "/.")
```

#### Expected 

```text
true

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

### eval-okay-print.nix

```nix
with builtins; trace [(1+1)] [ null toString (deepSeq "x") (a: a) (let x=[x]; in x) ]
```

#### Expected 

```text
trace: [ «thunk» ]

```

### eval-okay-readDir.nix

```nix
builtins.readDir ./readDir
```

#### Expected 

```text
{ bar = "regular"; foo = "directory"; ldir = "symlink"; linked = "symlink"; }

```

### eval-okay-readFileType.nix

```nix
{
  bar    = builtins.readFileType ./readDir/bar;
  foo    = builtins.readFileType ./readDir/foo;
  linked = builtins.readFileType ./readDir/linked;
  ldir   = builtins.readFileType ./readDir/ldir;
}
```

#### Expected 

```text
{ bar = "regular"; foo = "directory"; ldir = "symlink"; linked = "symlink"; }

```

### eval-okay-readfile.nix

```nix
builtins.readFile ./eval-okay-readfile.nix
```

#### Expected 

```text
"builtins.readFile ./eval-okay-readfile.nix\n"

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

### eval-okay-regex-match.nix

```nix
with builtins;

let

  matches = pat: s: match pat s != null;

  splitFN = match "((.*)/)?([^/]*)\\.(nix|cc)";

in

assert  matches "foobar" "foobar";
assert  matches "fo*" "f";
assert !matches "fo+" "f";
assert  matches "fo*" "fo";
assert  matches "fo*" "foo";
assert  matches "fo+" "foo";
assert  matches "fo{1,2}" "foo";
assert !matches "fo{1,2}" "fooo";
assert !matches "fo*" "foobar";
assert  matches "[[:space:]]+([^[:space:]]+)[[:space:]]+" "  foo   ";
assert !matches "[[:space:]]+([[:upper:]]+)[[:space:]]+" "  foo   ";

assert match "(.*)\\.nix" "foobar.nix" == [ "foobar" ];
assert match "[[:space:]]+([[:upper:]]+)[[:space:]]+" "  FOO   " == [ "FOO" ];

assert splitFN "/path/to/foobar.nix" == [ "/path/to/" "/path/to" "foobar" "nix" ];
assert splitFN "foobar.cc" == [ null null "foobar" "cc" ];

true
```

#### Expected 

```text
true

```

### eval-okay-regex-split.nix

```nix
with builtins;

# Non capturing regex returns empty lists
assert  split "foobar" "foobar"  == ["" [] ""];
assert  split "fo*" "f"          == ["" [] ""];
assert  split "fo+" "f"          == ["f"];
assert  split "fo*" "fo"         == ["" [] ""];
assert  split "fo*" "foo"        == ["" [] ""];
assert  split "fo+" "foo"        == ["" [] ""];
assert  split "fo{1,2}" "foo"    == ["" [] ""];
assert  split "fo{1,2}" "fooo"   == ["" [] "o"];
assert  split "fo*" "foobar"     == ["" [] "bar"];

# Capturing regex returns a list of sub-matches
assert  split "(fo*)" "f"        == ["" ["f"] ""];
assert  split "(fo+)" "f"        == ["f"];
assert  split "(fo*)" "fo"       == ["" ["fo"] ""];
assert  split "(f)(o*)" "f"      == ["" ["f" ""] ""];
assert  split "(f)(o*)" "foo"    == ["" ["f" "oo"] ""];
assert  split "(fo+)" "foo"      == ["" ["foo"] ""];
assert  split "(fo{1,2})" "foo"  == ["" ["foo"] ""];
assert  split "(fo{1,2})" "fooo" == ["" ["foo"] "o"];
assert  split "(fo*)" "foobar"   == ["" ["foo"] "bar"];

# Matches are greedy.
assert  split "(o+)" "oooofoooo" == ["" ["oooo"] "f" ["oooo"] ""];

# Matches multiple times.
assert  split "(b)" "foobarbaz"  == ["foo" ["b"] "ar" ["b"] "az"];

# Split large strings containing newlines. null are inserted when a
# pattern within the current did not match anything.
assert  split "[[:space:]]+|([',.!?])" ''
  Nix Rocks!
  That's why I use it.
''  == [
  "Nix" [ null ] "Rocks" ["!"] "" [ null ]
  "That" ["'"] "s" [ null ] "why" [ null ] "I" [ null ] "use" [ null ] "it" ["."] "" [ null ]
  ""
];

# Documentation examples
assert  split  "(a)b" "abc"      == [ "" [ "a" ] "c" ];
assert  split  "([ac])" "abc"    == [ "" [ "a" ] "b" [ "c" ] "" ];
assert  split  "(a)|(c)" "abc"   == [ "" [ "a" null ] "b" [ null "c" ] "" ];
assert  split  "([[:upper:]]+)" "  FOO   " == [ "  " [ "FOO" ] "   " ];

true
```

#### Expected 

```text
true

```

### eval-okay-regression-20220122.nix

```nix
((_: _) 1) + ((__: __) 2)
```

#### Expected 

```text
3

```

### eval-okay-regression-20220125.nix

```nix
((__curPosFoo: __curPosFoo) 1) + ((__curPosBar: __curPosBar) 2)

```

#### Expected 

```text
3

```

### eval-okay-remove.nix

```nix
let {
  attrs = {x = 123; y = 456;};

  body = (removeAttrs attrs ["x"]).y;
}
```

#### Expected 

```text
456

```

### eval-okay-repeated-empty-attrs.nix

```nix
# Tests that empty attribute sets are not printed as `«repeated»`.
[ {} {} ]
```

#### Expected 

```text
[ { } { } ]

```

### eval-okay-repeated-empty-list.nix

```nix
[ [] [] ]
```

#### Expected 

```text
[ [ ] [ ] ]

```

### eval-okay-replacestrings.nix

```nix
with builtins;

[ (replaceStrings ["o"] ["a"] "foobar")
  (replaceStrings ["o"] [""] "foobar")
  (replaceStrings ["oo"] ["u"] "foobar")
  (replaceStrings ["oo" "a"] ["a" "oo"] "foobar")
  (replaceStrings ["oo" "oo"] ["u" "i"] "foobar")
  (replaceStrings [""] ["X"] "abc")
  (replaceStrings [""] ["X"] "")
  (replaceStrings ["-"] ["_"] "a-b")
  (replaceStrings ["oo" "XX"] ["u" (throw "unreachable")] "foobar")
]
```

#### Expected 

```text
[ "faabar" "fbar" "fubar" "faboor" "fubar" "XaXbXcX" "X" "a_b" "fubar" ]

```

### eval-okay-scope-1.nix

```nix
(({x}: x:

  { x = 1;
    y = x;
  }
) {x = 2;} 3).y
```

#### Expected 

```text
3

```

### eval-okay-scope-2.nix

```nix
((x: {x}:
  rec {
    x = 1;
    y = x;
  }
) 2 {x = 3;}).y
```

#### Expected 

```text
1

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

### eval-okay-scope-4.nix

```nix
let {

  x = "a";
  y = "b";

  f = {x ? y, y ? x}: x + y;

  body = f {x = "c";} + f {y = "d";};

}
```

#### Expected 

```text
"ccdd"

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

### eval-okay-search-path.nix

```nix
with import ./lib.nix;
with builtins;

assert isFunction (import <nix/fetchurl.nix>);

assert length __nixPath == 5;
assert length (filter (x: baseNameOf x.path == "dir4") __nixPath) == 1;

import <a.nix> + import <b.nix> + import <c.nix> + import <dir5/c.nix>
  + (let __nixPath = [ { path = ./dir2; } { path = ./dir1; } ]; in import <a.nix>)
```

#### Expected 

```text
"abccX"

```

### eval-okay-seq.nix

```nix
builtins.seq 1 2
```

#### Expected 

```text
2

```

### eval-okay-sort.nix

```nix
with builtins;

[ (sort lessThan [ 483 249 526 147 42 77 ])
  (sort (x: y: y < x) [ 483 249 526 147 42 77 ])
  (sort lessThan [ "foo" "bar" "xyzzy" "fnord" ])
  (sort (x: y: x.key < y.key)
    [ { key = 1; value = "foo"; } { key = 2; value = "bar"; } { key = 1; value = "fnord"; } ])
  (sort lessThan [
    [ 1 6 ]
    [ ]
    [ 2 3 ]
    [ 3 ]
    [ 1 5 ]
    [ 2 ]
    [ 1 ]
    [ ]
    [ 1 4 ]
    [ 3 ]
  ])
]
```

#### Expected 

```text
[ [ 42 77 147 249 483 526 ] [ 526 483 249 147 77 42 ] [ "bar" "fnord" "foo" "xyzzy" ] [ { key = 1; value = "foo"; } { key = 1; value = "fnord"; } { key = 2; value = "bar"; } ] [ [ ] [ ] [ 1 ] [ 1 4 ] [ 1 5 ] [ 1 6 ] [ 2 ] [ 2 3 ] [ 3 ] [ 3 ] ] ]

```

### eval-okay-splitversion.nix

```nix
builtins.splitVersion "1.2.3"
```

#### Expected 

```text
[ "1" "2" "3" ]

```

### eval-okay-string.nix

```nix
"foo" + "bar"
  + toString (/a/b + /c/d)
  + toString (/foo/bar + "/../xyzzy/." + "/foo.txt")
  + ("/../foo" + toString /x/y)
  + "escape: \"quote\" \n \\"
  + "end
of
line"
  + "foo${if true then "b${"a" + "r"}" else "xyzzy"}blaat"
  + "foo$bar"
  + "$\"$\""
  + "$"
```

#### Expected 

```text
true

```

### eval-okay-strings-as-attrs-names.nix

```nix
let

  attr = {
    "key 1" = "test";
    "key 2" = "caseok";
  };

  t1 = builtins.getAttr "key 1" attr;
  t2 = attr."key 2";
  t3 = attr ? "key 1";
  t4 = builtins.attrNames { inherit (attr) "key 1"; };

  # This is permitted, but there is currently no way to reference this
  # variable.
  "foo bar" = 1;

in t1 == "test"
   && t2 == "caseok"
   && t3 == true
   && t4 == ["key 1"]
```

#### Expected 

```text
true

```

### eval-okay-substring-context.nix

```nix
with builtins;

let

  s = "${builtins.derivation { name = "test"; builder = "/bin/sh"; system = "x86_64-linux"; }}";

in

if getContext s == getContext "${substring 0 0 s + unsafeDiscardStringContext s}"
then "okay"
else throw "empty substring should preserve context"
```

#### Expected 

```text
"okay"

```

### eval-okay-substring.nix

```nix
with builtins;

let

  s = "foobar";

in

substring 1 2 s
+ "x"
+ substring 0 (stringLength s) s
+ "y"
+ substring 3 100 s
+ "z"
+ substring 2 (sub (stringLength s) 3) s
+ "a"
+ substring 3 0 s
+ "b"
+ substring 3 1 s
+ "c"
+ substring 5 10 "perl"
+ "_"
+ substring 3 (-1) "tebbad"
```

#### Expected 

```text
"ooxfoobarybarzobaabbc_bad"

```

### eval-okay-symlink-resolution.nix

```nix
import symlink-resolution/foo/overlays/overlay.nix
```

#### Expected 

```text
"test"

```

### eval-okay-tail-call-1.nix

```nix
let
  f = n: if n == 100000 then n else f (n + 1);
in f 0
```

### eval-okay-tojson.nix

```nix
builtins.toJSON
  { a = 123;
    b = -456;
    c = "foo";
    d = "foo\n\"bar\"";
    e = true;
    f = false;
    g = [ 1 2 3 ];
    h = [ "a" [ "b" { "foo\nbar" = {}; } ] ];
    i = 1 + 2;
    j = 1.44;
    k = { __toString = self: self.a; a = "foo"; };
  }
```

#### Expected 

```text
"{\"a\":123,\"b\":-456,\"c\":\"foo\",\"d\":\"foo\\n\\\"bar\\\"\",\"e\":true,\"f\":false,\"g\":[1,2,3],\"h\":[\"a\",[\"b\",{\"foo\\nbar\":{}}]],\"i\":3,\"j\":1.44,\"k\":\"foo\"}"

```

### eval-okay-toxml.nix

```nix
# Make sure the expected XML output is produced; in particular, make sure it
# doesn't contain source location information.
builtins.toXML { a = "s"; }
```

#### Expected 

```text
"<?xml version='1.0' encoding='utf-8'?>\n<expr>\n  <list>\n    <string value=\"ab\" />\n    <int value=\"10\" />\n    <attrs>\n      <attr name=\"x\">\n        <string value=\"x\" />\n      </attr>\n      <attr name=\"y\">\n        <string value=\"x\" />\n      </attr>\n    </attrs>\n  </list>\n</expr>\n"

```

### eval-okay-toxml2.nix

```nix
builtins.toXML [("a" + "b") 10 (rec {x = "x"; y = x;})]
```

#### Expected 

```text
"<?xml version='1.0' encoding='utf-8'?>\n<expr>\n  <list>\n    <string value=\"ab\" />\n    <int value=\"10\" />\n    <attrs>\n      <attr name=\"x\">\n        <string value=\"x\" />\n      </attr>\n      <attr name=\"y\">\n        <string value=\"x\" />\n      </attr>\n    </attrs>\n  </list>\n</expr>\n"

```

### eval-okay-tryeval.nix

```nix
{
  x = builtins.tryEval "x";
  y = builtins.tryEval (assert false; "y");
  z = builtins.tryEval (throw "bla");
}
```

#### Expected 

```text
{ x = { success = true; value = "x"; }; y = { success = false; value = false; }; z = { success = false; value = false; }; }

```

### eval-okay-types.nix

```nix
with builtins;

[ (isNull null)
  (isNull (x: x))
  (isFunction (x: x))
  (isFunction "fnord")
  (isString ("foo" + "bar"))
  (isString [ "x" ])
  (isInt (1 + 2))
  (isInt { x = 123; })
  (isInt (1 / 2))
  (isInt (1 + 1))
  (isInt (1 / 2))
  (isInt (1 * 2))
  (isInt (1 - 2))
  (isFloat (1.2))
  (isFloat (1 + 1.0))
  (isFloat (1 / 2.0))
  (isFloat (1 * 2.0))
  (isFloat (1 - 2.0))
  (isBool (true && false))
  (isBool null)
  (isPath /nix/store)
  (isPath ./.)
  (isAttrs { x = 123; })
  (isAttrs null)
  (typeOf (3 * 4))
  (typeOf true)
  (typeOf "xyzzy")
  (typeOf null)
  (typeOf { x = 456; })
  (typeOf [ 1 2 3 ])
  (typeOf (x: x))
  (typeOf ((x: y: x) 1))
  (typeOf map)
  (typeOf (map (x: x)))
]
```

#### Expected 

```text
[ true false true false true false true false true true true true true true true true true true true false true true true false "int" "bool" "string" "null" "set" "list" "lambda" "lambda" "lambda" "lambda" ]

```

### eval-okay-versions.nix

```nix
let

  name1 = "hello-1.0.2";
  name2 = "hello";
  name3 = "915resolution-0.5.2";
  name4 = "xf86-video-i810-1.7.4";
  name5 = "name-that-ends-with-dash--1.0";

  eq = 0;
  lt = builtins.sub 0 1;
  gt = 1;

  versionTest = v1: v2: expected:
    let d1 = builtins.compareVersions v1 v2;
        d2 = builtins.compareVersions v2 v1;
    in d1 == builtins.sub 0 d2 && d1 == expected;

  tests = [
    ((builtins.parseDrvName name1).name == "hello")
    ((builtins.parseDrvName name1).version == "1.0.2")
    ((builtins.parseDrvName name2).name == "hello")
    ((builtins.parseDrvName name2).version == "")
    ((builtins.parseDrvName name3).name == "915resolution")
    ((builtins.parseDrvName name3).version == "0.5.2")
    ((builtins.parseDrvName name4).name == "xf86-video-i810")
    ((builtins.parseDrvName name4).version == "1.7.4")
    ((builtins.parseDrvName name5).name == "name-that-ends-with-dash")
    ((builtins.parseDrvName name5).version == "-1.0")
    (versionTest "1.0" "2.3" lt)
    (versionTest "2.1" "2.3" lt)
    (versionTest "2.3" "2.3" eq)
    (versionTest "2.5" "2.3" gt)
    (versionTest "3.1" "2.3" gt)
    (versionTest "2.3.1" "2.3" gt)
    (versionTest "2.3.1" "2.3a" gt)
    (versionTest "2.3pre1" "2.3" lt)
    (versionTest "2.3pre3" "2.3pre12" lt)
    (versionTest "2.3a" "2.3c" lt)
    (versionTest "2.3pre1" "2.3c" lt)
    (versionTest "2.3pre1" "2.3q" lt)
  ];

in (import ./lib.nix).and tests
```

#### Expected 

```text
true

```

### eval-okay-with.nix

```nix
let {

  a = "xyzzy";

  as = {
    a = "foo";
    b = "bar";
  };

  bs = {
    a = "bar";
  };

  x = with as; a + b;

  y = with as; with bs; a + b;

  body = x + y;
}
```

#### Expected 

```text
"xyzzybarxyzzybar"

```

### eval-okay-xml.nix

```nix
rec {

  x = 123;

  y = 567.890;

  a = "foo";

  b = "bar";

  c = "foo" + "bar";

  f = {z, x, y}: if y then x else z;

  id = x: x;

  at = args@{x, y, z}: x;

  ellipsis = {x, y, z, ...}: x;

}
```

### eval-okay-zipAttrsWith.nix

```nix
with import ./lib.nix;

let
  str = builtins.hashString "sha256" "test";
in
builtins.zipAttrsWith
  (n: v: { inherit n v; })
  (map (n: { ${builtins.substring n 1 str} = n; })
    (range 0 31))
```

#### Expected 

```text
{ "0" = { n = "0"; v = [ 5 23 29 ]; }; "1" = { n = "1"; v = [ 7 30 ]; }; "2" = { n = "2"; v = [ 18 ]; }; "4" = { n = "4"; v = [ 10 ]; }; "5" = { n = "5"; v = [ 15 25 26 31 ]; }; "6" = { n = "6"; v = [ 3 14 ]; }; "7" = { n = "7"; v = [ 12 ]; }; "8" = { n = "8"; v = [ 2 6 8 9 ]; }; "9" = { n = "9"; v = [ 0 16 ]; }; a = { n = "a"; v = [ 17 21 22 27 ]; }; c = { n = "c"; v = [ 11 24 ]; }; d = { n = "d"; v = [ 4 13 28 ]; }; e = { n = "e"; v = [ 20 ]; }; f = { n = "f"; v = [ 1 19 ]; }; }

```

### imported.nix

```nix
# The function ‘range’ comes from lib.nix and was added to the lexical
# scope by scopedImport.
range 1 5 ++ import ./imported2.nix
```

### imported2.nix

```nix
range 6 10
```

### lib.nix

```nix
with builtins;

rec {

  fold = op: nul: list:
    if list == []
    then nul
    else op (head list) (fold op nul (tail list));

  concat =
    fold (x: y: x + y) "";

  and = fold (x: y: x && y) true;

  flatten = x:
    if isList x
    then fold (x: y: (flatten x) ++ y) [] x
    else [x];

  sum = foldl' (x: y: add x y) 0;

  hasSuffix = ext: fileName:
    let lenFileName = stringLength fileName;
        lenExt = stringLength ext;
    in !(lessThan lenFileName lenExt) &&
       substring (sub lenFileName lenExt) lenFileName fileName == ext;

  # Split a list at the given position.
  splitAt = pos: list:
    if pos == 0 then {first = []; second = list;} else
    if list == [] then {first = []; second = [];} else
    let res = splitAt (sub pos 1) (tail list);
    in {first = [(head list)] ++ res.first; second = res.second;};

  # Stable merge sort.
  sortBy = comp: list:
    if lessThan 1 (length list)
    then
      let
        split = splitAt (div (length list) 2) list;
        first = sortBy comp split.first;
        second = sortBy comp split.second;
      in mergeLists comp first second
    else list;

  mergeLists = comp: list1: list2:
    if list1 == [] then list2 else
    if list2 == [] then list1 else
    if comp (head list2) (head list1) then [(head list2)] ++ mergeLists comp list1 (tail list2) else
    [(head list1)] ++ mergeLists comp (tail list1) list2;

  id = x: x;

  const = x: y: x;

  range = first: last:
    if first > last
      then []
      else genList (n: first + n) (last - first + 1);

}
```

### non-eval-fail-bad-drvPath.nix

```nix
let
  package = {
    type = "derivation";
    name = "cachix-1.7.3";
    system = builtins.currentSystem;
    outputs = [ "out" ];
    # Illegal, because does not end in `.drv`
    drvPath = "${builtins.storeDir}/8qlfcic10lw5304gqm8q45nr7g7jl62b-cachix-1.7.3-bin";
    outputName = "out";
    outPath = "${builtins.storeDir}/8qlfcic10lw5304gqm8q45nr7g7jl62b-cachix-1.7.3-bin";
    out = package;
  };
in
package
```

### parse-fail-dup-attrs-1.nix

```nix
{ x = 123;
  y = 456;
  x = 789;
}
```

#### Expected 

```text
error: attribute 'x' already defined at «stdin»:1:3
       at «stdin»:3:3:
            2|   y = 456;
            3|   x = 789;
             |   ^
            4| }

```

### parse-fail-dup-attrs-2.nix

```nix
let {

  as = {
    x = 123;
    y = 456;
  };

  bs = {
    x = 789;
    inherit (as) x;
  };
  
}
```

#### Expected 

```text
error: attribute 'x' already defined at «stdin»:9:5
       at «stdin»:10:18:
            9|     x = 789;
           10|     inherit (as) x;
             |                  ^
           11|   };

```

### parse-fail-dup-attrs-3.nix

```nix
let {

  as = {
    x = 123;
    y = 456;
  };

  bs = rec {
    x = 789;
    inherit (as) x;
  };
  
}
```

#### Expected 

```text
error: attribute 'x' already defined at «stdin»:9:5
       at «stdin»:10:18:
            9|     x = 789;
           10|     inherit (as) x;
             |                  ^
           11|   };

```

### parse-fail-dup-attrs-4.nix

```nix
{
  services.ssh.port = 22;
  services.ssh.port = 23;
}
```

#### Expected 

```text
error: attribute 'services.ssh.port' already defined at «stdin»:2:3
       at «stdin»:3:3:
            2|   services.ssh.port = 22;
            3|   services.ssh.port = 23;
             |   ^
            4| }

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

### parse-fail-dup-formals.nix

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

### parse-fail-eof-in-string.nix

```nix
# https://github.com/NixOS/nix/issues/6562
# Note that this file must not end with a newline.
a 1"$
```

#### Expected 

```text
error: syntax error, unexpected end of file, expecting '"'
       at «stdin»:3:6:
            2| # Note that this file must not end with a newline.
            3| a 1"$
             |      ^

```

### parse-fail-eof-pos.nix

```nix
(
# no content
```

#### Expected 

```text
error: syntax error, unexpected end of file
       at «stdin»:3:1:
            2| # no content
            3|
             | ^

```

### parse-fail-mixed-nested-attrs1.nix

```nix
{ 
  x.z = 3; 
  x = { y = 3; z = 3; }; 
}
```

#### Expected 

```text
error: attribute 'z' already defined at «stdin»:3:16
       at «stdin»:2:3:
            1| {
            2|   x.z = 3;
             |   ^
            3|   x = { y = 3; z = 3; };

```

### parse-fail-mixed-nested-attrs2.nix

```nix
{ 
  x.y.y = 3; 
  x = { y.y= 3; z = 3; }; 
}
```

#### Expected 

```text
error: attribute 'y' already defined at «stdin»:3:9
       at «stdin»:2:3:
            1| {
            2|   x.y.y = 3;
             |   ^
            3|   x = { y.y= 3; z = 3; };

```

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

### parse-fail-regression-20060610.nix

```nix
let {
  x =
    {gcc}:
    {
      inherit gcc;
    };

  body = ({
    inherit gcc;
  }).gcc;
}
```

#### Expected 

```text
error: undefined variable 'gcc'
       at «stdin»:9:13:
            8|   body = ({
            9|     inherit gcc;
             |             ^
           10|   }).gcc;

```

### parse-fail-undef-var-2.nix

```nix
let {

  f = {x, y : ["baz" "bar" z "bat"]}: x + y;

  body = f {x = "foo"; y = "bar";};

}
```

#### Expected 

```text
error: syntax error, unexpected ':', expecting '}' or ','
       at «stdin»:3:13:
            2|
            3|   f = {x, y : ["baz" "bar" z "bat"]}: x + y;
             |             ^
            4|

```

### parse-fail-undef-var.nix

```nix
x: y
```

#### Expected 

```text
error: syntax error, unexpected ':', expecting '}' or ','
       at «stdin»:3:13:
            2|
            3|   f = {x, y : ["baz" "bar" z "bat"]}: x + y;
             |             ^
            4|

```

### parse-fail-utf8.nix

```nix
123 é 4
```

#### Expected 

```text
error: syntax error, unexpected invalid token, expecting end of file
       at «stdin»:1:5:
            1| 123 é 4
             |     ^
            2|

```

### parse-okay-1.nix

```nix
{x, y, z}: x + y + z
```

#### Expected 

```text
({ x, y, z }: ((x + y) + z))

```

### parse-okay-crlf.nix

```nix
rec {

  /* Dit is
  een test. */

  x = 
  # Dit is een test.y;
  
  y = 123;

  # CR or CR/LF (but not explicit \r's) in strings should be
  # translated to LF.
  foo = "multiline
  string
  test\r";

  z = 456;}
```

#### Expected 

```text
rec { foo = "multi\nline\n  string\n  test\r"; x = y; y = 123; z = 456; }

```

### parse-okay-dup-attrs-5.nix

```nix
{
  services.ssh = { enable = true; };
  services.ssh.port = 23;
}
```

#### Expected 

```text
{ services = { ssh = { enable = true; port = 23; }; }; }

```

### parse-okay-dup-attrs-6.nix

```nix
{
  services.ssh.port = 23;
  services.ssh = { enable = true; };
}
```

#### Expected 

```text
{ services = { ssh = { enable = true; port = 23; }; }; }

```

### parse-okay-ind-string.nix

```nix
let
  string = "str";
in [
  /some/path

  ''${/some/path}''

  ''
    ${/some/path}''

  ''${/some/path}
    end''

  string

  ''${string}''

  ''
    ${string}''

  ''${string}
    end''

  ''''

  ''
  ''

  ''
    end''
]
```

#### Expected 

```text
(let string = "str"; in [ (/some/path) ((/some/path)) ((/some/path)) ((/some/path + "\n    end")) (string) ((string)) ((string)) ((string + "\n    end")) ("") ("") ("end") ])

```

### parse-okay-inherits.nix

```nix
let
  c = {};
  b = 2;
in {
  a = 1;
  inherit b;
  inherit (c) d e;
  f = 3;
}
```

#### Expected 

```text
(let b = 2; c = { }; in { inherit b; inherit (c) d e; a = 1; f = 3; })

```

### parse-okay-mixed-nested-attrs-1.nix

```nix
{ 
  x = { y = 3; z = 3; }; 
  x.q = 3; 
}
```

#### Expected 

```text
{ x = { q = 3; y = 3; z = 3; }; }

```

### parse-okay-mixed-nested-attrs-2.nix

```nix
{ 
  x.q = 3; 
  x = { y = 3; z = 3; }; 
}
```

#### Expected 

```text
{ x = { q = 3; y = 3; z = 3; }; }

```

### parse-okay-mixed-nested-attrs-3.nix

```nix
{
    services.ssh.enable = true;
    services.ssh = { port = 123; };
    services = {
        httpd.enable = true;
    };
}
```

#### Expected 

```text
{ services = { httpd = { enable = true; }; ssh = { enable = true; port = 123; }; }; }

```

### parse-okay-regression-20041027.nix

```nix
{stdenv, fetchurl /* pkgconfig, libX11 */ }:

stdenv.mkDerivation {
  name = "libXi-6.0.1";
  src = fetchurl {
    url = http://freedesktop.org/~xlibs/release/libXi-6.0.1.tar.bz2;
    md5 = "7e935a42428d63a387b3c048be0f2756";
  };
/*  buildInputs = [pkgconfig];
  propagatedBuildInputs = [libX11]; */
}
```

#### Expected 

```text
({ fetchurl, stdenv }: ((stdenv).mkDerivation { name = "libXi-6.0.1"; src = (fetchurl { md5 = "7e935a42428d63a387b3c048be0f2756"; url = "http://freedesktop.org/~xlibs/release/libXi-6.0.1.tar.bz2"; }); }))

```

### parse-okay-regression-751.nix

```nix
let const = a: "const"; in
''${ const { x = "q"; }}''
```

#### Expected 

```text
(let const = (a: "const"); in ((const { x = "q"; })))

```

### parse-okay-subversion.nix

```nix
{ localServer ? false
, httpServer ? false
, sslSupport ? false
, pythonBindings ? false
, javaSwigBindings ? false
, javahlBindings ? false
, stdenv, fetchurl
, openssl ? null, httpd ? null, db4 ? null, expat, swig ? null, j2sdk ? null
}:

assert expat != null;
assert localServer -> db4 != null;
assert httpServer -> httpd != null && httpd.expat == expat;
assert sslSupport -> openssl != null && (httpServer -> httpd.openssl == openssl);
assert pythonBindings -> swig != null && swig.pythonSupport;
assert javaSwigBindings -> swig != null && swig.javaSupport;
assert javahlBindings -> j2sdk != null;

stdenv.mkDerivation {
  name = "subversion-1.1.1";

  builder = /foo/bar;
  src = fetchurl {
    url = http://subversion.tigris.org/tarballs/subversion-1.1.1.tar.bz2;
    md5 = "a180c3fe91680389c210c99def54d9e0";
  };

  # This is a hopefully temporary fix for the problem that
  # libsvnjavahl.so isn't linked against libstdc++, which causes
  # loading the library into the JVM to fail.
  patches = if javahlBindings then [/javahl.patch] else [];

  openssl = if sslSupport then openssl else null;
  httpd = if httpServer then httpd else null;
  db4 = if localServer then db4 else null;
  swig = if pythonBindings || javaSwigBindings then swig else null;
  python = if pythonBindings then swig.python else null;
  j2sdk = if javaSwigBindings then swig.j2sdk else
          if javahlBindings then j2sdk else null;

  inherit expat localServer httpServer sslSupport
          pythonBindings javaSwigBindings javahlBindings;
}
```

#### Expected 

```text
({ db4 ? null, expat, fetchurl, httpServer ? false, httpd ? null, j2sdk ? null, javaSwigBindings ? false, javahlBindings ? false, localServer ? false, openssl ? null, pythonBindings ? false, sslSupport ? false, stdenv, swig ? null }: assert (expat != null); assert (localServer -> (db4 != null)); assert (httpServer -> ((httpd != null) && ((httpd).expat == expat))); assert (sslSupport -> ((openssl != null) && (httpServer -> ((httpd).openssl == openssl)))); assert (pythonBindings -> ((swig != null) && (swig).pythonSupport)); assert (javaSwigBindings -> ((swig != null) && (swig).javaSupport)); assert (javahlBindings -> (j2sdk != null)); ((stdenv).mkDerivation { inherit expat httpServer javaSwigBindings javahlBindings localServer pythonBindings sslSupport; builder = /foo/bar; db4 = (if localServer then db4 else null); httpd = (if httpServer then httpd else null); j2sdk = (if javaSwigBindings then (swig).j2sdk else (if javahlBindings then j2sdk else null)); name = "subversion-1.1.1"; openssl = (if sslSupport then openssl else null); patches = (if javahlBindings then [ (/javahl.patch) ] else [ ]); python = (if pythonBindings then (swig).python else null); src = (fetchurl { md5 = "a180c3fe91680389c210c99def54d9e0"; url = "http://subversion.tigris.org/tarballs/subversion-1.1.1.tar.bz2"; }); swig = (if (pythonBindings || javaSwigBindings) then swig else null); }))

```

### parse-okay-url.nix

```nix
[ x:x
  https://svn.cs.uu.nl:12443/repos/trace/trunk
  http://www2.mplayerhq.hu/MPlayer/releases/fonts/font-arial-iso-8859-1.tar.bz2
  http://losser.st-lab.cs.uu.nl/~armijn/.nix/gcc-3.3.4-static-nix.tar.gz
  http://fpdownload.macromedia.com/get/shockwave/flash/english/linux/7.0r25/install_flash_player_7_linux.tar.gz
  https://ftp5.gwdg.de/pub/linux/archlinux/extra/os/x86_64/unzip-6.0-14-x86_64.pkg.tar.zst
  ftp://ftp.gtk.org/pub/gtk/v1.2/gtk+-1.2.10.tar.gz
]
```

#### Expected 

```text
[ ("x:x") ("https://svn.cs.uu.nl:12443/repos/trace/trunk") ("http://www2.mplayerhq.hu/MPlayer/releases/fonts/font-arial-iso-8859-1.tar.bz2") ("http://losser.st-lab.cs.uu.nl/~armijn/.nix/gcc-3.3.4-static-nix.tar.gz") ("http://fpdownload.macromedia.com/get/shockwave/flash/english/linux/7.0r25/install_flash_player_7_linux.tar.gz") ("https://ftp5.gwdg.de/pub/linux/archlinux/extra/os/x86_64/unzip-6.0-14-x86_64.pkg.tar.zst") ("ftp://ftp.gtk.org/pub/gtk/v1.2/gtk+-1.2.10.tar.gz") ]

```

