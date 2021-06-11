{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
pkgs.callPackage (pkgs.fetchgit {
  url = sources.tree-grepper.repo;
  rev = sources.tree-grepper.rev;
  sha256 = "0dkavi2lyi8l58pgfvwi3d7vl6slxlfg0yb58rm34fqcg6j4gxjz";

  # This whole thing is just a workaround for niv not being able to check
  # out a repo with submodules. No big deal to add a little wrapper, though!
  fetchSubmodules = true;
}) { }
