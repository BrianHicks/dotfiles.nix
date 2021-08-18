{ sources ? import ../../nix/sources.nix { }, pkgs ? import sources.nixpkgs { }
, ... }:
let naersk = pkgs.callPackage sources.naersk { };
in naersk.buildPackage sources.kak-subvert
