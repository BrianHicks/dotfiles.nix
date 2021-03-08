{ pkgs, ... }:
let
  sources = import ../../nix/sources.nix;

  src = pkgs.fetchgit {
    url = sources.tree-grepper.repo;
    rev = sources.tree-grepper.rev;
    sha256 = "0fw8dlp63pgangczjb68dpzi097jncdai4bhvw6mprzwkh6nspf5";
    # This whole thing is just a workaround for niv not being able to check
    # out a repo with submodules. No big deal to add a little wrapper, though!
    fetchSubmodules = true;
  };
in pkgs.callPackage src { }
