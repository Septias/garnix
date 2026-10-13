# Why instrumentation changes the meaning of a NixLang program.
#
# A gradual type system inserts a cast where a value of type ★ meets a static
# type. Checking the cast inspects the value, and inspecting a lazy value forces
# it. The uninstrumented program never forced that value. The cast can
# therefore introduce an error or non-termination that the original program
# does not have.
#
#   nix-instantiate --eval --strict examples/forcing.nix -A plain
#   nix-instantiate --eval --strict examples/forcing.nix -A cast.<case>
#
# `plain` evaluates to { arg = 1; field = "hello"; self = "hello"; }.
# cast.arg and cast.field fail with an error. cast.self does not terminate;
# Nix cuts it off with "stack overflow; max-call-depth exceeded".
# lazy.arg evaluates to 1: a cast that is itself delayed forces nothing, but it
# also checks nothing at the boundary, so it gives up the point of the cast.
let
  # Casts as a checker would insert them, written as plain Nix.
  # ⟨{}⟩ v: v must be an attribute set (forces v to WHNF).
  toRecord = v: assert builtins.isAttrs v; v;
  # ⟨τ⟩ v: the field l of v must have type τ (forces v.l).
  toField = l: check: v: assert check v.${l}; v;
  # ⟨T⟩ v for a fully static record type T: checks every field, transitively.
  toDeep = v: builtins.deepSeq v v;

  # 1. Unused argument. `f` ignores its argument, so it is never evaluated.
  #    If the argument is typed ★ and f's parameter {}, the cast sits at the
  #    call and forces the argument before f runs (`seq`).
  f = x: 1;
  broken = throw "evaluation of a package that is not used";

  # 2. Unused field. A package set whose `bar` field fails to evaluate, as
  #    nixpkgs does for packages that are unsupported on the host platform.
  #    Only `foo` is selected. A cast that checks the record against
  #    {foo: string, bar: string} forces `bar`.
  pkgs1 = {
    foo = "hello";
    bar = throw "bar is not available on this platform";
  };

  # 3. Unbounded structure. nixpkgs contains further package sets built on
  #    demand (`pkgs.pkgsCross.<platform>.pkgsCross…`). Each is a fresh value,
  #    so the structure is infinite rather than cyclic. The selection
  #    terminates, a cast that checks the whole structure does not.
  mkPkgs = n: {
    foo = "hello";
    pkgsCross = mkPkgs (n + 1);
  };
  pkgs2 = mkPkgs 0;
in
{
  plain = {
    arg = f broken;
    field = pkgs1.foo;
    self = pkgs2.pkgsCross.pkgsCross.foo;
  };

  cast = {
    arg = let a = toRecord broken; in builtins.seq a (f a);
    field = (toField "bar" builtins.isString pkgs1).foo;
    self = (toDeep pkgs2).foo;
  };

  lazy = {
    arg = f (toRecord broken);
  };
}
