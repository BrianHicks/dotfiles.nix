{ pkgs, lib, ... }: {
  programs.tmux = {
    enable = true;
    terminal = "screen-256color";
  };
}
