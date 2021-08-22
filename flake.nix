{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";

    tmux = {
      url = "github:tmux/tmux";
      flake = false;
    };
  };

  outputs = inputs: {
    overlay = final: prev: {
      tmux = prev.tmux.overrideAttrs (attrs:
        attrs // {
          src = inputs.tmux;

          # macOS does some weird stuff with locales and character widths.
          # Practically, that means that without extra support tmux will
          # behave weirdly around multi-byte characters like emoji. Enabling
          # utf8proc support should backfill the right tables so that tmux
          # can get the correct character widths.
          buildInputs = attrs.buildInputs ++ [ prev.utf8proc ];
          configureFlags = attrs.configureFlags ++ [ "--enable-utf8proc" ];
        }
      );

      git-gclone = final.callPackage ./pkgs/git-gclone {};
    };

    nixosConfigurations.torch = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        (import ./machines/torch inputs)
        inputs.home-manager.nixosModules.home-manager
      ];
    };

    nixosConfigurations.vbox-dev = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        (import ./machines/vbox-dev inputs)
        inputs.home-manager.nixosModules.home-manager
      ];
    };
  };
}
