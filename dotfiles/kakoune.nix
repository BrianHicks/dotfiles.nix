{ pkgs, lib, ... }:
let
  sources = import ../nix/sources.nix;
in
{
  programs.kakoune = {
    enable = true;
    config = {
      colorScheme = "gotham";
      scrollOff = {
        columns = 0;
        lines = 5;
      };
      numberLines = {
       enable = true;
       separator = " ";
      };
      showMatching = true;
      ui.enableMouse = true;
      wrapLines = {
        enable = true;
        indent = true;
        marker = "⎁";
      };
    };
  };

  # plugins
  home.file.".config/kak/colors".source = sources.kakoune-colors;
}
