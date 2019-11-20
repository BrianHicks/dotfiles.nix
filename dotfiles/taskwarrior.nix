{ pkgs, ... }:

{
  programs.taskwarrior = {
    enable = true;
    dataLocation = "~/.local/share/task";
  };

  home.packages = [ pkgs.vit ];
}
