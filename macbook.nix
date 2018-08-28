{ pkgs, ... }:

{
  imports = [
    ./config/git.nix
  ];

  programs.home-manager = {
    enable = true;
    path = "./home-manager";
  };
}
