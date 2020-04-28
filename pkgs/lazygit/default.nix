{ sources ? import ../../nix/sources.nix, nixpkgs ? import sources.nixpkgs { }
}:
nixpkgs.lazygit.overrideAttrs (oldAttrs: { src = sources.lazygit; })
