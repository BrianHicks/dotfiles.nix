{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
pkgs.callPackage (pkgs.fetchgit {
  url = sources.tree-grepper.repo;
  rev = sources.tree-grepper.rev;
  sha256 = "0lwr8spkj0czlrnslcx31bsk24l7a208ild5cz64illwapvvc0ap";

  # This whole thing is just a workaround for niv not being able to check
  # out a repo with submodules. No big deal to add a little wrapper, though!
  fetchSubmodules = true;
}) { }
