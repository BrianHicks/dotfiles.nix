{
  description = "Home Manager configuration of brianhicks";

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
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      flake-utils,
      crit,
      ...
    }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      overlays = [
        (final: prev: {
          git-gclone = pkgs.callPackage ./pkgs/git-gclone { };

          git-hclone = pkgs.callPackage ./pkgs/git-hclone { };

          homebrew-sync = pkgs.callPackage ./pkgs/homebrew-sync { };

          list-python-tests = pkgs.callPackage ./pkgs/list-python-tests { };

          mypy-error-count-score = pkgs.callPackage ./pkgs/mypy-error-count-score { };

          crit = crit.packages.${system}.crit;
        })
      ];

      mkProfile =
        profile:
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "aarch64-darwin";
            inherit overlays;
          };

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home.nix
            ./modules/homebrew
          ];

          # Optionally use extraSpecialArgs
          # to pass through arguments to home.nix
          extraSpecialArgs = {
            inherit profile;
          };
        };
    in
    {
      homeConfigurations = {
        home = mkProfile "home";
        work = mkProfile "work";
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;

          # we need overlays even in the dev-shell home-manager because we want
          # to use the exact home-manager version from the flake, not whatever
          # one happens to be upstream in nixpkgs.
          inherit overlays;
        };
      in
      {
        formatter = pkgs.nixfmt-tree;
      }
    );
}
