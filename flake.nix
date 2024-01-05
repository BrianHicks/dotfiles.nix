{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    darwin.url = "github:LnL7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    naersk.url = "github:nix-community/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs";

    nix-index.url = "github:bennofs/nix-index";
    nix-index.inputs.nixpkgs.follows = "nixpkgs";

    tree-grepper.url = "github:BrianHicks/tree-grepper";

    xbar-pr-status.url = "github:BrianHicks/xbar-pr-status";
    xbar-pr-status.inputs.nixpkgs.follows = "nixpkgs";
    xbar-pr-status.inputs.naersk.follows = "naersk";

    xbar-review-request-status.url =
      "github:BrianHicks/xbar-review-request-status";
    xbar-review-request-status.inputs.nixpkgs.follows = "nixpkgs";
    xbar-review-request-status.inputs.naersk.follows = "naersk";

    comma = {
      url =
        "github:nix-community/comma/54149dc417819af14ddc0d59216d4add5280ad14";
      flake = false;
    };

    fzf-tab = {
      url = "github:Aloxaf/fzf-tab";
      flake = false;
    };

    spoons = {
      url = "github:Hammerspoon/Spoons";
      flake = false;
    };

    sysz = {
      url = "github:joehillen/sysz";
      flake = false;
    };
  };

  outputs = inputs:
    let
      mkOverlays = system: [
        inputs.tree-grepper.overlay."${system}"
        inputs.xbar-pr-status.overlay."${system}"
        inputs.xbar-review-request-status.overlay."${system}"
        (final: prev:
          let
            naersk = inputs.naersk.lib."${system}";
          in
          {
            comma = final.callPackage inputs.comma { };

            # is this going to cause problems by not actually being a package?
            fzf-tab = inputs.fzf-tab;

            git-gclone = final.callPackage ./pkgs/git-gclone { };

            hammerspoon.spoons = final.stdenv.mkDerivation {
              name = "spoons";
              rev = inputs.spoons.rev;
              src = inputs.spoons;

              buildPhase = "true";
              installPhase = ''
                mkdir -p $out
                cp -r Source/* $out
              '';
            };

            lazygit-window = final.callPackage ./pkgs/lazygit-window { };

            mand = final.writeShellScriptBin "mand" ''
              ${final.pandoc}/bin/pandoc -s -f markdown -t man $1 | ${final.groff}/bin/groff -T utf8 -man | ${final.less}/bin/less
            '';

            meet = final.callPackage ./pkgs/meet { };

            # nix-index = inputs.nix-index.packages.${system}.nix-index;

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

            tmux-session = final.callPackage ./pkgs/tmux-session { };

            # tree-grepper tests pass most of the time, but sometimes the nixpkgs
            # between that repo and this one cause clippy incompatibilities. The
            # tests are run in CI anyway, so there's no need to re-run them here.
            tree-grepper =
              prev.tree-grepper.overrideAttrs (attrs: { doCheck = false; });
          })
      ];
    in
    {
      formatter.aarch64-darwin = inputs.nixpkgs.legacyPackages.aarch64-darwin.nixpkgs-fmt;

      darwinConfigurations.VNDR-A535 = inputs.darwin.lib.darwinSystem rec {
        inherit inputs;

        system = "aarch64-darwin";

        modules = [
          ({ pkgs, ... }: { nixpkgs.overlays = mkOverlays system; })
          ./machines/VNDR-A535
          inputs.home-manager.darwinModules.home-manager
        ];
      };

      darwinConfigurations.birch = inputs.darwin.lib.darwinSystem rec {
        inherit inputs;

        system = "aarch64-darwin";

        modules = [
          ({ pkgs, ... }: { nixpkgs.overlays = mkOverlays system; })
          ./machines/birch
          inputs.home-manager.darwinModules.home-manager
        ];
      };
    };
}
