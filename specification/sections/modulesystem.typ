#import "../functions.typ": *

#let export = [
  == Nix' Module System
  The Nix module system is a framework in the Nixpkgs standard library that offers a domain‑specific language for declarative configuration. Types in this DSL are enforced at evaluation time: option values are checked as configurations are instantiated, providing early feedback about shape and well‑formedness. The catalogue of types is intentionally open and user‑extensible.

  Each type definition comprises a human‑readable description and name, together with two operational components: a check function and a merge function. The check function validates candidate values—primarily via Nix’s reflective predicates—while the merge function reconciles option values contributed by multiple modules. Because configurations are hierarchical and cross‑cutting, values for a given option may be introduced or refined at many points in the module graph (e.g., a WireGuard module may enable and specialize NetworkManager). Robust merging is therefore a first‑class design requirement.

  There is currently no formal semantics for this type language. In practice, types serve a dual role as both validators and merge policies, reflecting pragmatic concerns more than type‑theoretic uniformity. An overview of the available constructors is provided in @module-types.

  The library includes primitive types for booleans, paths, and strings, as well as specialized variants tailored to configuration workflows. For instance, `boolByOr` merges multiple boolean contributions via logical disjunction, which fits ubiquitous “enable” flags; `inLineLua`, `separatedString`, and `numbers.inBetween` exemplify the blend of typing with property validation. Path types are refined into “in‑store” and “out‑of‑store” variants to capture operational provenance—distinctions that are immaterial in a purely static setting yet salient during evaluation.

  Higher‑order constructors exist for enumerations, options, sum types, lists, and attribute sets (records). Their expressiveness is constrained by reliance on runtime reflection; there is no global type inference, and annotations primarily enforce local properties on declared fields.

  Consequently, it is difficult to distill from this ad‑hoc collection a sound, conventional static type system. One could imagine augmenting it with qualified or refinement types to state richer invariants, but that trajectory approaches verification rather than lightweight validation. A more immediately useful direction is to ascribe precise types to the check functions themselves and, where possible, stage their application statically so configuration errors surface earlier. Whether a type system for the Nix source language can predict such applications with sufficient precision to be broadly valuable remains an open question.


  #figure(caption: "Nix module system example", ```nix
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
  ```)
]

https://github.com/NixOS/nixpkgs/blob/master/lib/types.nix


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
