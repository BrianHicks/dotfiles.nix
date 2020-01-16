{ pkgs, lib, ... }: {
  programs.tmux = {
    enable = true;
    terminal = "tmux-256color";
    shortcut = "a";
    keyMode = "vi";
    secureSocket = false;
    baseIndex = 1;
    extraConfig = ''
      set -ga terminal-overrides ",*col*:Tc"

      # navigate around split panes with C-{h/j/k/l}
      bind-key -n C-h select-pane -L
      bind-key -n C-j select-pane -U
      bind-key -n C-k select-pane -D
      bind-key -n C-l select-pane -R

      # get rid of the half-second escape time for kakoune's escape key
      set -sg escape-time 25

      # status line
      # set status-bg colour0
      # set status-fg colour16
    '';
  };
}
