#import "../functions.typ": *

#let export = [
  The nix module system is a framework and part of the nix standart library.
  It is a DSL inside the module system, that does type-checking during evaluation. This helps during module system configuration, because on build, you can see what might go wrong instead of in practice due to experiments or no at all. The typesystem provides the basic types and a combination thereof. It does not bring type connectives to the table I can imagine.


  ```nix
  let
    systemModule = { lib, config, ... }: {
      options.toplevel = lib.mkOption {
        type = lib.types.str;
      };

      options.enableFoo = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };

      config.toplevel = ''
        Is foo enabled? ${lib.boolToString config.enableFoo}
      '';
    };

    userModule = {
      enableFoo = true;
    };

  in (import <nixpkgs/lib>).evalModules {
    modules = [ systemModule userModule ];
  }
  ```

  https://github.com/NixOS/nixpkgs/blob/master/lib/types.nix

  == Simple Types
  anything: Useful when it is used under a meta-type.
  bool: A Boolean useful for enable flags. The merge function is a logical OR between all definitions.
  int: An Integer.
  str: A string where all definitions are concatenated.
  envVar: A string where all definitions are concatenated with a colon between all definitions.
  attrs: An attribute set. (you should prefer attrsOf inferred)
  package: A derivation.


  == Meta Types
  === Data meta-types:

  listOf t: A list of elements with the type t.
  attrsOf t: An attribute set of elements with the type t. The merge function zip all attribute sets into one. Attribute values of the resulting attribute set are merged with the merge function of the type t.

  === Definition meta-types:
  uniq t: This type define raise an error if more than one definitions exists. All other properties are inherited from the type t. This is useful to avoid ambiguous definitions.
  none t: This type define raise an error if at least one definitions exists. All other properties are inherited from the type t. This is useful to provide a computation result to other modules. See also the apply function of option declarations.
  nullOr t: This type allows an option to be null or type t.

]
