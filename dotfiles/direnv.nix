{ pkgs, ... }:

{
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  home.file.".direnvrc".source = ./direnv/direnvrc;
}
