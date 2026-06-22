{
  description = "Dotfiles galore!";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nixos.url = "github:nixos/nixpkgs/nixos-unstable";

    disko.url = "github:nix-community/disko";

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

    ik-llama-cpp = {
      url = "github:ikawrakow/ik_llama.cpp?rev=b47b90d0be80981f6f476c997afbdfab99bba6c7";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      flake-utils,
      crit,
      learning-opportunities,
      disko,
      ik-llama-cpp,
      ...
    }@inputs:
    let
      mkOverlays =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        [
          (final: prev: {
            git-gclone = pkgs.callPackage ./pkgs/git-gclone { };

            git-hclone = pkgs.callPackage ./pkgs/git-hclone { };

            homebrew-sync = pkgs.callPackage ./pkgs/homebrew-sync { };

            list-python-tests = pkgs.callPackage ./pkgs/list-python-tests { };

            mypy-error-count-score = pkgs.callPackage ./pkgs/mypy-error-count-score { };

            crit = crit.packages.${system}.crit;

            ik-llama-cpp = (
              ik-llama-cpp.packages.${system}.default.overrideAttrs (old: {
                NIX_CFLAGS_COMPILE = (old.NIX_CFLAGS_COMPILE or "") + " -mavx2 -mavxvnni -mfma -mf16c";

                cmakeFlags = old.cmakeFlags ++ [
                  "-DGGML_AVX2=ON"
                  "-DGGML_AVX_VNNI=ON"
                  "-DGGML_FMA=ON"
                  "-DGGML_F16C=ON"
                  "-DGGML_OPENMP=ON"
                ];
              })
            );

            # source only
            learning-opportunities = learning-opportunities;
          })
        ];

      mkHomeConfiguration =
        system: profile:
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            inherit system;
            overlays = mkOverlays system;
          };

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./homes/brian.nix
            ./modules/homebrew
          ];

          # Optionally use extraSpecialArgs
          # to pass through arguments to home.nix
          extraSpecialArgs = {
            inherit profile;
          };
        };

      mkHost =
        hostPath:
        nixpkgs.lib.nixosSystem rec {
          system = "x86_64-linux";
          pkgs = import nixpkgs {
            inherit system;
            overlays = mkOverlays system;
            config.allowUnfree = true;
          };
          modules = [
            disko.nixosModules.disko
            hostPath
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.sharedModules = [
                ./modules/homebrew
              ];
              home-manager.extraSpecialArgs = {
                inherit inputs;
                profile = "home";
                hostname = baseNameOf hostPath;
              };

              home-manager.users.anne = ./homes/anne.nix;
              home-manager.users.brian = ./homes/brian.nix;
              home-manager.users.nate = ./homes/nate.nix;
            }
          ];
        };
    in
    {
      homeConfigurations = {
        home = mkHomeConfiguration "aarch64-darwin" "home";
        work = mkHomeConfiguration "aarch64-darwin" "work";
      };

      nixosConfigurations.avior = mkHost ./machines/avior;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;

          # we need overlays even in the dev-shell home-manager because we want
          # to use the exact home-manager version from the flake, not whatever
          # one happens to be upstream in nixpkgs.
          overlays = mkOverlays system;
        };
      in
      {
        formatter = pkgs.nixfmt-tree;
      }
    );
}
