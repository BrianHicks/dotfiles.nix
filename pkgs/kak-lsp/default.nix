{ sources ? import ../../nix/sources.nix { }, ... }:
let
  nixpkgs-mozilla = import sources.nixpkgs-mozilla;

  pinnedNightly = {
    date = "2021-08-06";
    channel = "nightly";
  };

  pkgs = import sources.nixpkgs {
    overlays = [
      nixpkgs-mozilla
      (self: super: {
        rustc = (self.rustChannelOf pinnedNightly).rust;
        cargo = (self.rustChannelOf pinnedNightly).rust;
      })
    ];
  };
  naersk = pkgs.callPackage sources.naersk { };
in naersk.buildPackage sources.kak-lsp
