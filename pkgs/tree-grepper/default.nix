{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
pkgs.callPackage (pkgs.fetchgit {
  url = sources.tree-grepper.repo;
  rev = sources.tree-grepper.rev;
  sha256 = "0w6ichhdxz0zlxpvdn1hl8a66gk265myxnz5zvxw81b2wj2k3yqw";

  # This whole thing is just a workaround for niv not being able to check
  # out a repo with submodules. No big deal to add a little wrapper, though!
  fetchSubmodules = true;
}) { }
