#let module_types = figure(caption: [Nix module system builtin types taken from #link("https://github.com/NixOS/nixpkgs/blob/42c103b34b6ae7f7db1e1901891cc0b74690fb09/nixos/doc/manual/development/option-types.section.md")], table(
  columns: 2,
  table.header([*Type*], [*Description*]),
  table.cell(colspan: 2)[*Basic Types*],
  [`types.bool`],
  [A boolean, its values can be true or false. All definitions must have the same value, after priorities. An error is thrown in case of a conflict.],

  [`types.boolByOr`],
  [A boolean, its values can be true or false. The result is true if any of multiple definitions is true. In other words, definitions are merged with the logical OR operator.],

  [`types.path`],
  [A filesystem path that starts with a slash. Even if derivations can be considered as paths, the more specific types.package should be preferred.],

  [`types.pathInStore`],
  [A path contained in the Nix store, either a top-level store path or a descendant path.],

  [`types.externalPath`],
  [A path not contained in the Nix store, typically used for secrets or external files. Validation only checks current location and does not prevent later copying into the store.],

  [`types.pathWith`],
  [A filesystem path with configurable constraints controlling whether it must be in the store and/or absolute.],

  [`types.package`],
  [A top-level store path, typically a derivation or flake input pointing to a package.],

  [`types.enum`],
  [A value restricted to one element of a given list. Multiple definitions cannot be merged.],

  [`types.anything`],
  [Accepts any value and recursively merges attribute sets, useful when the option type is unknown.],

  [`types.raw`],
  [Accepts a single arbitrary value without checking, merging, or recursive evaluation, useful for values coming from outside the module system.],

  [`types.optionType`],
  [The type representing option types themselves, ensuring correct merging and source tracking for nested options.],

  [`types.attrs`],
  [A free-form attribute set type that does not recurse and may silently drop earlier definitions; deprecated in favor of attrsOf types.anything.],

  [`types.pkgs`], [The type representing the top-level Nixpkgs package set.],
  [`types.int`], [A signed integer.],
  [`types.ints.s8`], [An 8-bit signed integer ranging from −128 to 127.],
  [`types.ints.s16`], [A 16-bit signed integer ranging from −32768 to 32767.],
  [`types.ints.s32`], [A 32-bit signed integer.],
  [`types.ints.unsigned`], [An unsigned integer greater than or equal to 0.],
  [`types.ints.u8`], [An 8-bit unsigned integer ranging from 0 to 255.],
  [`types.ints.u16`], [A 16-bit unsigned integer ranging from 0 to 65535.],
  [`types.ints.u32`], [A 32-bit unsigned integer.],
  [`types.ints.between`],
  [An integer restricted to a specified inclusive range.],

  [`types.ints.positive`], [A strictly positive integer greater than 0.],
  [`types.port`], [A port number, equivalent to types.ints.u16.],
  [`types.float`],
  [A floating point number; string conversion may cause precision loss.],

  [`types.number`],
  [Either an integer or floating point number, without implicit conversion between them.],

  [`types.numbers.between`],
  [An integer or floating point number within a specified inclusive range.],

  [`types.numbers.nonnegative`],
  [A nonnegative integer or floating point number greater than or equal to 0.],

  [`types.numbers.positive`],
  [A strictly positive integer or floating point number greater than 0.],

  [`types.str`], [A string type where multiple definitions cannot be merged.],
  [`types.separatedString`],
  [A string type where multiple definitions are concatenated using a specified separator.],

  [`types.lines`],
  [A string type where multiple definitions are concatenated using newline characters.],

  [`types.commas`],
  [A string type where multiple definitions are concatenated using commas.],

  [`types.envVar`],
  [A string type where multiple definitions are concatenated using colons.],

  [`types.strMatching`],
  [A string constrained to match a given regular expression.],

  [`types.luaInline`],
  [A string wrapped for inline Lua embedding using lib.mkLuaInline.],

  [`types.submodule`],
  [A set of sub-options handled as a separate module, defined via attribute set, function, or module reference.],

  [`types.submoduleWith`],
  [A configurable submodule type allowing specification of default modules, special arguments, and configuration shorthand behavior.],

  [`types.deferredModule`],
  [Represents a module value that can be set multiple times and imported into other module fixpoints.],

  [`types.either`],
  [A union type allowing values matching either of two types; merging multiple definitions is not supported.],

  [`types.oneOf`],
  [A union type allowing values matching any type from a list; merging multiple definitions is not supported.],

  [`types.nullOr`],
  [Allows either null or a value of a specified type, merging according to the non-null type rules.],

  [`types.attrTag`],
  [An attribute-tagged union requiring exactly one attribute key from a predefined set, each associated with its own option type.],

  [`types.listOf`],
  [A list where all elements must match a specified type, with definitions merged via list concatenation.],

  [`types.attrsOf`],
  [An attribute set whose values must match a specified type; definitions are merged by joining attributes strictly.],

  [`types.lazyAttrsOf`],
  [A lazily evaluated version of attrsOf allowing attributes to depend on each other, with limitations around conditional definitions.],

  [`types.attrsWith`],
  [A configurable attribute set type specifying element type, optional lazy evaluation, and documentation placeholders for attribute names.],

  [`types.uniq`],
  [Ensures that a value of a given type cannot be merged and must be defined only once.],

  [`types.unique`],
  [Ensures that a value of a given type cannot be merged, emitting a custom message when multiple definitions occur.],

  [`types.coercedTo`],
  [Allows values of one type to be coerced into another using a conversion function, typically for backward compatibility.],

  [`types.json`],
  [Represents JSON-compatible values including null, booleans, numbers, strings, paths, lists, and attribute sets.],

  [`types.toml`],
  [Represents TOML-compatible values including booleans, numbers, strings, paths, lists, and attribute sets.],
))
