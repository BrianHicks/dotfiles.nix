{
  description = "Home Manager configuration of brianhicks";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, home-manager, ... }:
    let
      system = "aarch64-darwin";
      pkgs = nixpkgs.legacyPackages.${system};
      overlays = [
        (final: prev: {
          homebrew-sync = pkgs.callPackage ./pkgs/homebrew-sync/default.nix {};
        })
      ];

      mkProfile = profile: home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs rec {
          system = "aarch64-darwin";
          inherit overlays;
        };

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ./modules/homebrew ];

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
    };
}
