{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
pkgs.callPackage (pkgs.fetchgit {
  url = sources.tree-grepper.repo;
  rev = sources.tree-grepper.rev;
  sha256 = "11pfqw6k2clia6nxqf8haiwja4irza8rxxm2mg996ghwr9v0cm2v";

  # This whole thing is just a workaround for niv not being able to check
  # out a repo with submodules. No big deal to add a little wrapper, though!
  fetchSubmodules = true;
}) { }
