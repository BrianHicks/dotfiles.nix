{ ... }:
let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs; rec {
  src = pkgs.fetchgit {
    url = "https://github.com/ul/kak-tree.git";
    rev = "b5ea0f4fad961c8b3f6279a3af608f96b8eb3e35";
    sha256 = "02pgaynkrss679j15ckvka9kn74k0ldwixzdgygsjhzzlcxi0gji";
    fetchSubmodules = true;
  };

  kak-tree = rustPlatform.buildRustPackage {
    name = "kak-tree";
    src = src;

    cargoSha256 = "1hqnxjn898kpi850m3qz361hkkhjkhr5j4gk828isqlwl8q75dpr";
    cargoBuildFlags =
      [ ''--features "bash css haskell html javascript json python ruby"'' ];
  };
}
