{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";

    similar-sort.url =
      "git+https://git.bytes.zone/brian/similar-sort.git?ref=main";
    similar-sort.inputs.nixpkgs.follows = "nixpkgs";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    tree-grepper.url = "github:BrianHicks/tree-grepper";
    tree-grepper.inputs.nixpkgs.follows = "nixpkgs-unstable";

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

    # kakoune plugins
    active-window = {
      url = "github:greenfork/active-window.kak";
      flake = false;
    };
    kakoune-find = {
      url = "github:occivink/kakoune-find";
      flake = false;
    };
    kakoune-surround = {
      url = "github:h-youhei/kakoune-surround";
      flake = false;
    };
    shellcheck-kak = {
      url = "github:whereswaldon/shellcheck.kak";
      flake = false;
    };
    smarttab-kak = {
      url = "github:andreyorst/smarttab.kak";
      flake = false;
    };
    tug = {
      url = "github:matthias-margush/tug";
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

          kakounePlugins = let
            buildKakounePlugin = name: input:
              final.kakouneUtils.buildKakounePlugin {
                inherit name;
                version = input.rev;
                src = input;
              };
          in prev.kakounePlugins // {
            active-window =
              buildKakounePlugin "active-window" inputs.active-window;

            kakoune-find =
              buildKakounePlugin "kakoune-find" inputs.kakoune-find;

            kakoune-surround =
              buildKakounePlugin "kakoune-surround" inputs.kakoune-surround;

            shellcheck-kak =
              buildKakounePlugin "shellcheck.kak" inputs.shellcheck-kak;

            smarttab-kak =
              buildKakounePlugin "smarttab.kak" inputs.smarttab-kak;

            tug = buildKakounePlugin "tug" inputs.tug;
          };
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
