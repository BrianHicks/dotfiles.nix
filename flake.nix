{
  description = "Dotfiles galore!";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";

    crit = {
      url = "github:tomasz-tomczyk/crit";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    learning-opportunities = {
      url = "github:DrCatHicks/learning-opportunities";
      flake = false;
    };
  };

  outputs =
    inputs:
    let
      mkOverlay =
        {
          system ? "x86_64-linux",
          nixpkgs ? inputs.nixpkgs,
        }:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        (final: prev: {
          git-gclone = pkgs.callPackage ./pkgs/git-gclone { };

          git-hclone = pkgs.callPackage ./pkgs/git-hclone { };

          homebrew-sync = pkgs.callPackage ./pkgs/homebrew-sync { };

          list-python-tests = pkgs.callPackage ./pkgs/list-python-tests { };

          mypy-error-count-score = pkgs.callPackage ./pkgs/mypy-error-count-score { };

          crit = inputs.crit.packages.${system}.crit;

          # source only
          learning-opportunities = inputs.learning-opportunities;
        });

      homeModuleSetup =
        {
          system ? "aarch64-darwin",
          extraSpecialArgs ? { },
          nixpkgs ? inputs.nixpkgs,
        }:
        {
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (mkOverlay { inherit system; }) ];
          };

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./modules/homebrew
          ];

          # Optionally use extraSpecialArgs
          # to pass through arguments to home.nix
          inherit extraSpecialArgs;
        };

      mkHomeConfiguration =
        {
          system ? "aarch64-darwin",
          profile,
        }:
        let
          base = homeModuleSetup {
            system = system;
            extraSpecialArgs.profile = profile;
          };
        in
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit (base) pkgs extraSpecialArgs;
          modules = [ ./homes/brian.nix ] ++ base.modules;
        };
    in
    {
      inherit mkOverlay homeModuleSetup;

      homeModules = {
        anne = ./homes/anne.nix;
        brian = ./homes/brian.nix;
        nate = ./homes/nate.nix;
      };

      homeConfigurations = {
        home = mkHomeConfiguration { profile = "home"; };
        work = mkHomeConfiguration { profile = "work"; };
      };
    }
    // inputs.flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;

          # we need overlays even in the dev-shell home-manager because we want
          # to use the exact home-manager version from the flake, not whatever
          # one happens to be upstream in nixpkgs.
          overlays = mkOverlay {
            inherit system;
          };
        };
      in
      {
        formatter = pkgs.nixfmt-tree;
      }
    );
}
