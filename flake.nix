{
  description = "Type-inference for Nix";
  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "rust-overlay/nixpkgs";
    naersk.url = "github:nix-community/naersk";
  };
  outputs = inputs:
    with inputs;
      flake-utils.lib.eachDefaultSystem (
        system: let
          name = "garnix";
          pkgs = import nixpkgs {
            overlays = [(import rust-overlay)];
            inherit system;
          };
          naerskLib = pkgs.callPackage naersk {
            cargo = rust-toolchain;
            rustc = rust-toolchain;
          };
          buildInputs = with pkgs; [
            openssl
          ];
          nativeBuildInputs = with pkgs; [pkg-config];
          rust-toolchain = pkgs.rust-bin.nightly.latest.default.override {
            extensions = ["rust-src" "rustfmt" "rust-docs" "clippy" "rust-analyzer"];
          };
        in {
          formatter = pkgs.alejandra;
          packages = {
            default = naerskLib.buildPackage {
              inherit name buildInputs nativeBuildInputs;
              src = ./.;
            };
          };
          devShells.default = pkgs.mkShell {
            inherit buildInputs;
            nativeBuildInputs = nativeBuildInputs ++ [rust-toolchain];
            RUST_BACKTRACE = 1;
          };
        }
      );
}
