{ ... }:
let
  sources = import ../../nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
in with nixpkgs;
buildGoPackage rec {
  name = "gh-cli-${version}";
  version = sources.gh-cli.rev;

  goPackagePath = "github.com/${sources.gh-cli.owner}/${sources.gh-cli.repo}";

  src = sources.gh-cli;
  goDeps = ./deps.nix;
}
