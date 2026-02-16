#import "../functions.typ": *

#let export = [
  == Nix' Module System
  The nix module system is a framework and part of the nix standard library.
  It is a DSL inside the module system, that does type-checking during evaluation. This helps writing module system configuration, because it verifies on build (of packages) that configured values have the proper shape at least. The typesystem is not standardized and can be extended by every user by simply adding a new constructor for a type.
  All these type definitions consist of a description, a name, a merge function and a check function. The check function is used to check whether a provided value is of the type, mostly using the builtin reflection capabilities of nix and the merge function is used to merge option values that were defined in different places of the configuration. Due to nix' hierarchical structure and the interdependency of packages, configuration changes can happen anywhere in the configuration hierarchy. For example, the networkmanager configuration will be enabled and configured by a wireguard system. This very wireguard system will also probably be configurable directly, leading to more changes in the networkmanager configuration. A merge-function is thus essential to uphold this programming paradigm.
  As to be expected, there is no formal treatment for types, leading to a mixture of types that are sometimes used more for their merging behaviour than for typechecking itself. A full list of the provided types can be found in @module-types but we will give a short overview now to give the reader a better understanding of the capabilities of the module system.

  There exist types for the usual primitives types bool, path, string and more. But also a specialized bool-type `boolByOr` that is used to merge multiple bool types with the or-operation. This is useful for the enable-option on every package. This option can be written everywhere and many packages might enable it as a subdependency. The logical or operation is thus a good fit to enable the package if it is needed by any other service. The types `inLineLua` and `separatedString` and `numbers.inBetween` go in a similar direction and show the mixture of typing and property verifictation. The path primitive also has two variations, one that checks if the path is in the _nix store_ and one that checks whether it is outside of it. In a merely theoretical world there is no distinction between the two but their applicabilty during evaluation shows their second usage to propose properties outside of merely type theorectial ones.

  The DLS also provides some higher-order types like enumes, options, sum types, lists and types for attrSets (records) but these types are very limited in their form as they can only be checked using the reflection tools of the language. There is also no inference to it as type annotations are only used to enforce some properties on the explicit fields.

  In conclusion, it is not really possible to create a sound and sensible typesystem out of this wild mix of types that that are sometimes only used for their merging behaviour. Maybe a system with qualified types could deduce interesting properties next to purely type theoretic ones, but that goes more into the direction of verified programming. The only real-world use I see is inference for the check-operation. By giving this function a valid type, it could in theory be possible to apply it to the actually supplied values and check for their correctness this way. This will not lead to a sound type system but could improve the security of nix due to static detection of faulty arguments. Whether a typestem for the nix source language is able to deduce the application of this function to arguments precisely enough to be valuable is still an open question.


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
