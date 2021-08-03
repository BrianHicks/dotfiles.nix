{ sources ? import ../../nix/sources.nix { }, ... }:
let
  nixpkgs-mozilla = import sources.nixpkgs-mozilla;
  pkgs = import sources.nixpkgs {
    overlays = [
      nixpkgs-mozilla
      (self: super: {
        rustc = self.latest.rustChannels.nightly.rust;
        cargo = self.latest.rustChannels.nightly.rust;
      })
    ];
  };
  naersk = pkgs.callPackage sources.naersk { };
in naersk.buildPackage sources.kak-lsp
