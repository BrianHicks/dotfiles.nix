{
  description = "Brian's Dotfiles";

  inputs = { nixpkgs.url = "github:NixOS/nixpkgs"; };

  outputs = inputs: {
    nixosConfigurations.torch = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ (import ./torch/configuration.nix inputs) ];
    };
  };
}
