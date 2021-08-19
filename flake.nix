{
  description = "Brian's Dotfiles";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-21.05";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    home-manager.url = "github:nix-community/home-manager/release-21.05";
  };

  outputs = inputs: {
    nixosConfigurations.torch = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ (import ./torch inputs) ];
    };
  };
}
