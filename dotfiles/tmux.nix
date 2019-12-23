{ pkgs, lib, ... }: {
  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
    extraConfig = "set -ga terminal-overrides \",*col*:Tc\"";
  };
}
