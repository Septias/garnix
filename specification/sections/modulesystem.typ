#import "../functions.typ": *

#let export = [
  The nix module system is a framework and part of the nix standart library.
  It is a DSL inside the module system, that does type-checking during evaluation. This helps writing module system configuration, because it verifies on build (of packages) that configured values have the proper shape at least. The typesystem is not standardized and can be extended by every user. All type definitions consist of a description, a name, a merge function and a check function. The check function is used to check whether a provided value is of the type, mostly using the builtin reflection capabilities of nix. The merge function is used to merge option values that were defined in different places of the configiguration. Due to nix' hirarchical structure and the interdependency of packages, configuration changes can happen anywhere in the configuration hirarchy. For example, the networkmanager configuration will be edited be enabled and configured by a wireguard system. This very wireguard system will also probably.


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
]
https://github.com/NixOS/nixpkgs/blob/master/lib/types.nix
== Question
- How can you connect the two?



== Ana

- bool: normal type
  - boolByOr: Merged using or
- path: we also have that
  - in_store (more precise)
  - external (more precise)
- enum: array of possible elements, not mergable
- optionType: some options
- numbers.inbetween: ints in ranges (qualified types?)
- seperatedString: Merges using commas
- luaInLine: very specific
- connectives
  - either
  - oneOf
  - nullOr
- constuctors
  - listOf(t): (can also represent union lists)
  - attrsaOf(t): all values are of type t
- unique: type can not be merged
- coercedTo: can coerce a type
- json & toml
- submodule: recursive types/options?



== Draft
- How are they checked? What exactly is checked?
- Write a bit about the merging
  - multiple definitions occur all over the place
  - types are used more as "merging decisision"
- Everyone can add types
