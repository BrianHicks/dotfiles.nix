{ pkgs, lib, ... }: {
  programs.tmux = {
    enable = true;
    terminal = "xterm-256color";
    extraConfig = "set -ga terminal-overrides \",*col*:Tc\"";
  };
}
