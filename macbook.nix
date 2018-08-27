{ pkgs, ... };

{
  programs.home-manager = {
    enable = true;
    path = ./home-manager;
  };
}
