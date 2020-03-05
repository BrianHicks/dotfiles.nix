{ ... }:
let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;
rustPlatform.buildRustPackage {
  name = "kak-lsp";
  src = sources.kak-lsp;

  # note: when updating, this needs to be set to all 1's so that Nix will
  # re-fetch the dependencies from crates.io
  cargoSha256 = "0bbp6chk5946h2li971b6w5dhc29j0n6j4rp8h2fqdfzsxc4z4g4";
}
