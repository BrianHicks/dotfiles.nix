{ ... }:
let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs; rec {
  src = pkgs.fetchgit {
    url = "https://github.com/ul/kak-tree.git";
    rev = "2fa4b122a06c6b8b802329a66e2a59ddf00e8372";
    sha256 = "17n0g9dljz700f1qd5qa4ps78mbzl24ai2zv549knv57ig09g5k5";
    fetchSubmodules = true;
  };

  kak-tree = rustPlatform.buildRustPackage {
    name = "kak-tree";
    src = src;

    # note: when updating, this needs to be set to all 1's so that Nix will
    # re-fetch the dependencies from crates.io
    cargoSha256 = "0n3j2d15m039cl3fn5ibk4kyk1hnawhwbkinph50ry4pzhix3ikb";
    cargoBuildFlags = [
      ''--features "bash css elm haskell html javascript json python ruby"''
    ];
  };
}
