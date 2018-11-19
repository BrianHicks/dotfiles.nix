{ pkgs, ... }:

{
  home.packages = [
    pkgs.tmux
  ];

  home.file.".tmux.conf" = {
    text = "set-option -g prefix C-a";
  };
}
