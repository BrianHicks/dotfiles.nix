{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
pkgs.callPackage (pkgs.fetchgit {
  url = sources.tree-grepper.repo;
  rev = sources.tree-grepper.rev;
  sha256 = "11if9aw5mr10jmb25hn6003c1xxzjxgs8cs1z2g2r79rn0zx4nyw";

  # This whole thing is just a workaround for niv not being able to check
  # out a repo with submodules. No big deal to add a little wrapper, though!
  fetchSubmodules = true;
}) { }
