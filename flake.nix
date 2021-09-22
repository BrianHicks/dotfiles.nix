{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-darwin.url = "github:NixOS/nixpkgs/nixpkgs-21.05-darwin";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";

    home-manager.url = "github:nix-community/home-manager/release-21.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    naersk.url = "github:nix-community/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs-unstable";

    similar-sort.url =
      "git+https://git.bytes.zone/brian/similar-sort.git?ref=main";
    similar-sort.inputs.nixpkgs.follows = "nixpkgs";
    similar-sort.inputs.naersk.follows = "naersk";

    tree-grepper.url = "github:BrianHicks/tree-grepper/2.0.6";
    tree-grepper.inputs.nixpkgs.follows = "nixpkgs-unstable";
    tree-grepper.inputs.naersk.follows = "naersk";

    comma = {
      url = "github:shopify/comma";
      flake = false;
    };

    fzf-tab = {
      url = "github:Aloxaf/fzf-tab";
      flake = false;
    };

    sysz = {
      url = "github:joehillen/sysz";
      flake = false;
    };

    tmux = {
      url = "github:tmux/tmux/3.3-rc";
      flake = false;
    };

    # kakoune plugins
    auto-pairs = {
      url = "github:alexherbo2/auto-pairs.kak";
      flake = false;
    };
    active-window = {
      url = "github:greenfork/active-window.kak";
      flake = false;
    };
    kak-subvert = {
      url = "github:dmerejkowsky/kak-subvert";
      flake = false;
    };
    kakoune-auto-percent = {
      url = "github:Delapouite/kakoune-auto-percent";
      flake = false;
    };
    kakoune-find = {
      url = "github:occivink/kakoune-find";
      flake = false;
    };
    kakoune-idris = {
      url = "github:stoand/kakoune-idris";
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
        (final: prev:
          let
            naersk = inputs.naersk.lib."${system}";

            # kak-tree uses submodules. I don't know how to get Nix to fetch
            # those in a flake, so we source with fetchgit instead, all wrapped
            # up in this package. I'd love to move it to a real flake, though,
            # so things would be all in one place!
            kak-tree = final.callPackage ./pkgs/kak-tree { inherit naersk; };

            unstable = inputs.nixpkgs-unstable.legacyPackages."${system}";
          in {
            comma = final.callPackage inputs.comma { };

            # is this going to cause problems by not actually being a package?
            fzf-tab = inputs.fzf-tab;

            git-gclone = final.callPackage ./pkgs/git-gclone { };

            gh = unstable.gh;

            kak-lsp = unstable.kak-lsp;

            kak-session = final.callPackage ./pkgs/kak-session { };

            kak-subvert = naersk.buildPackage inputs.kak-subvert;

            kak-tree = kak-tree.kak-tree;

            kakounePlugins = let
              buildKakounePlugin = name: input:
                final.kakouneUtils.buildKakounePlugin {
                  pname = name;
                  version = input.rev;
                  src = input;
                };
            in prev.kakounePlugins // {
              active-window =
                buildKakounePlugin "active-window" inputs.active-window;

              auto-pairs =
                buildKakounePlugin "auto-pairs.kak" inputs.auto-pairs;

              kak-ayu = final.callPackage ./pkgs/kak-ayu { };

              kak-subvert = buildKakounePlugin "kak-subvert" inputs.kak-subvert;

              kak-tree = kak-tree.kakounePlugins.kak-tree;

              kakoune-auto-percent = buildKakounePlugin "kakoune-auto-percent"
                inputs.kakoune-auto-percent;

              kakoune-find =
                buildKakounePlugin "kakoune-find" inputs.kakoune-find;

              kakoune-idris = final.kakouneUtils.buildKakounePlugin {
                name = "kakoune-idris";
                version = inputs.kakoune-idris.rev;
                src = inputs.kakoune-idris;
                patches = [
                  (builtins.fetchurl {
                    url =
                      "https://patch-diff.githubusercontent.com/raw/stoand/kakoune-idris/pull/9.patch";
                    sha256 =
                      "sha256:1hwrn4hqji4qi4bxrzvccxcaq9gkd0cmyra3jl8zzaqplh6m7jyn";
                  })
                ];
              };

              kakoune-surround =
                buildKakounePlugin "kakoune-surround" inputs.kakoune-surround;

              shellcheck-kak =
                buildKakounePlugin "shellcheck.kak" inputs.shellcheck-kak;

              smarttab-kak =
                buildKakounePlugin "smarttab.kak" inputs.smarttab-kak;

              tug = buildKakounePlugin "tug" inputs.tug;
            };

            lazygit-window = final.callPackage ./pkgs/lazygit-window { };

            mand = final.writeShellScriptBin "mand" ''
              ${final.pandoc}/bin/pandoc -s -f markdown -t man $1 | ${final.groff}/bin/groff -T utf8 -man | ${final.less}/bin/less
            '';

            sysz = final.stdenv.mkDerivation {
              name = "sysz";
              src = inputs.sysz;

              buildPhase = "true";
              buildInputs = [ final.makeWrapper ];
              installPhase = ''
                mkdir -p $out/bin
                install -m755 sysz $out/bin

                wrapProgram $out/bin/sysz --prefix PATH : ${
                  final.lib.makeBinPath [ final.fzf ]
                }
              '';
            };

            tmux = prev.tmux.overrideAttrs (attrs:
              attrs // {
                src = inputs.tmux;
                version = inputs.tmux.rev;

                # macOS does some weird stuff with locales and character widths.
                # Practically, that means that without extra support tmux will
                # behave weirdly around multi-byte characters like emoji. Enabling
                # utf8proc support should backfill the right tables so that tmux
                # can get the correct character widths.
                buildInputs = attrs.buildInputs ++ [ prev.utf8proc ];
                configureFlags = attrs.configureFlags
                  ++ [ "--enable-utf8proc" ];
              });

            tmux-session = final.callPackage ./pkgs/tmux-session { };

            # tree-grepper tests pass most of the time, but sometimes the nixpkgs
            # between that repo and this one cause clippy incompatibilities. The
            # tests are run in CI anyway, so there's no need to re-run them here.
            tree-grepper =
              prev.tree-grepper.overrideAttrs (attrs: { doCheck = false; });
          })
      ];
    in {
      nixosConfigurations.torch = inputs.nixpkgs.lib.nixosSystem rec {
        system = "x86_64-linux";
        modules = [
          ({ ... }: {
            nixpkgs.overlays = mkOverlays system;
            nix.nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
          })
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

      darwinConfigurations.flame = inputs.darwin.lib.darwinSystem rec {
        inherit inputs;

        system = "x86_64-darwin";

        modules = [
          ({ pkgs, ... }: {
            nix.package = pkgs.nixUnstable;
            nixpkgs.overlays = mkOverlays system;
          })
          ./machines/flame
          inputs.home-manager.darwinModules.home-manager
        ];
      };
    };
}
