{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";

    similar-sort.url =
      "git+https://git.bytes.zone/brian/similar-sort.git?ref=main";
    similar-sort.inputs.nixpkgs.follows = "nixpkgs";

    tree-grepper.url = "github:BrianHicks/tree-grepper";

    tmux = {
      url = "github:tmux/tmux";
      flake = false;
    };

    fzf-tab = {
      url = "github:Aloxaf/fzf-tab";
      flake = false;
    };

    comma = {
      url = "github:shopify/comma";
      flake = false;
    };
  };

  outputs = inputs:
    let
      mkOverlays = system: [
        inputs.similar-sort.overlay."${system}"
        inputs.tree-grepper.overlay."${system}"
        (final: prev: {
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
            });

          git-gclone = final.callPackage ./pkgs/git-gclone { };

          # is this going to cause problems by not actually being a package?
          fzf-tab = inputs.fzf-tab;

          lazygit-window = final.callPackage ./pkgs/lazygit-window { };

          tmux-session = final.callPackage ./pkgs/tmux-session { };

          comma = final.callPackage inputs.comma { };

          kak-session = final.callPackage ./pkgs/kak-session { };
        })
      ];
    in {
      nixosConfigurations.torch = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ({ ... }: { nixpkgs.overlays = mkOverlays system; })
          (import ./machines/torch inputs)
          inputs.home-manager.nixosModules.home-manager
        ];
      };

      nixosConfigurations.vbox-dev = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ({ ... }: { nixpkgs.overlays = mkOverlays system; })
          (import ./machines/vbox-dev inputs)
          inputs.home-manager.nixosModules.home-manager
        ];
      };
    };
}
