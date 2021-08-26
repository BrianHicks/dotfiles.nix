{ pkgs, ... }:
let
  copyCommand = if pkgs.stdenv.isDarwin then "pbcopy" else "UNKNOWN";
  pasteCommand = if pkgs.stdenv.isDarwin then "pbpaste" else "UNKNOWN";
in {
  programs.kakoune = {
    enable = true;

    config = {
      scrollOff.lines = 5;
      numberLines = {
        enable = true;
        highlightCursor = true;
        separator = ''" "'';
      };
      showMatching = true;
      ui.enableMouse = true;
      ui.assistant = "clippy";
      wrapLines = {
        enable = true;
        indent = true;
      };
    };
  };
}
