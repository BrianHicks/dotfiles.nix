{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    naersk.url = "github:nix-community/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs";

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
          {
            comma = final.callPackage inputs.comma { };

            # is this going to cause problems by not actually being a package?
            fzf-tab = inputs.fzf-tab;

            git-gclone = final.callPackage ./pkgs/git-gclone { };

            home-manager = inputs.home-manager.packages.${system}.home-manager;

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
      homeConfigurations.brianhicks = inputs.home-manager.lib.homeManagerConfiguration {
        pkgs = import inputs.nixpkgs rec {
          system = "aarch64-darwin";
          overlays = mkOverlays system;
        };

        modules = [ ./home.nix ];
      };
    } // inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;

          # we need overlays even in the dev-shell home-manager because we want
          # to use the exact home-manager version from the flake, not whatever
          # one happens to be upstream in nixpkgs.
          overlays = mkOverlays system;
        };
      in
      {
        formatter = pkgs.nixpkgs-fmt;

        devShell = pkgs.mkShell {
          packages = [
            pkgs.home-manager
          ];

          FLAKE_CONFIG_URI = "/Users/brianhicks/code/BrianHicks/dotfiles.nix#homeConfigurations.brianhicks";
        };
      }
    );
}
