{ pkgs, lib, ... }:
let tmux-session = pkgs.callPackage ../pkgs/tmux-session { };
in {
  home.packages = [ tmux-session ];

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
      bind-key -n C-j select-pane -D
      bind-key -n C-k select-pane -U
      bind-key -n C-l select-pane -R

      # quickly open CLI tools
      bind h run-shell "tmux switch-client -t :lazygit || tmux new-window -k -n 'lazygit' -c '#{pane_current_path}' ${pkgs.lazygit}/bin/lazygit"
      bind j split-window -h ${tmux-session}/bin/tmux-jump

      # get rid of the half-second escape time for kakoune's escape key
      set -sg escape-time 25

      # mouse
      set -g mouse on

      # open new terminals in the same working directory
      bind '"' split-window -c "#{pane_current_path}"
      bind '-' split-window -c "#{pane_current_path}"

      bind % split-window -h -c "#{pane_current_path}"
      bind '|' split-window -h -c "#{pane_current_path}"

      bind c new-window -c "#{pane_current_path}"

      bind l select-layout main-vertical

      # status line (thanks, Ju!)
      set-option -g status-justify left
      set-option -g status-left '#[bg=colour2] #[bg=colour8] #[bg=colour0] #S '
      set-option -g status-left-length 16
      set-option -g status-fg colour7
      set-option -g status-bg colour0
      set-option -g status-right '%a %R #[bg=colour8] #[bg=colour2] #[]'
      set-option -g status-interval 60
      set-option -g pane-active-border-style fg=colour8
      set-option -g pane-border-style fg=colour238
      set-window-option -g window-status-format '#[bg=colour8]#[fg=colour3] #I #[fg=colour15]#W#[fg=colour5]#F# '
      set-window-option -g window-status-current-format '#[bg=colour8]#[fg=colour3] #I #[bg=colour7]#[fg=colour8] #W#[fg=colour0]#F #[bg=colour8]'
    '';
  };
}
