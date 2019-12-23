{ pkgs, lib, ... }: {
  programs.kakoune = {
    enable = true;
    config = {
      # colorScheme = "";
      scrollOff = {
        columns = 0;
        lines = 5;
      };
      numberLines.enable = true;
      showMatching = true;
      ui.enableMouse = true;
      wrapLines = {
        enable = true;
        indent = true;
        marker = "⎁";
      };
    };
  };
}
