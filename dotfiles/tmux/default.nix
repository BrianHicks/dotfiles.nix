{ pkgs, ... }:
let
  shortStat = pkgs.writeShellScriptBin "shortStat" ''
    if ${pkgs.git}/bin/git show HEAD > /dev/null; then
      OUTPUT="$(${pkgs.git}/bin/git diff --shortstat | ${pkgs.gnused}/bin/sed -E 's/ ([0-9]+) files? changed(, ([0-9]+) insertions?\(\+\))?(, ([0-9]+) deletions?\(\-\))?/\1f \3+ \5-/' | ${pkgs.gnused}/bin/sed 's/ -//' | ${pkgs.gnused}/bin/sed 's/ +//')"
      if test -z "$OUTPUT"; then
        echo clean
      else
        echo "$OUTPUT"
      fi
    fi
  '';

  showBranch = pkgs.writeShellScriptBin "showBranch" ''
    if ${pkgs.git}/bin/git show HEAD > /dev/null; then
      ${pkgs.git}/bin/git rev-parse --abbrev-ref HEAD
    fi
  '';
in
{
  home.packages = [ pkgs.tmux-session ];

  programs.tmux = {
    enable = true;
    terminal = "xterm-256color";
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
      bind h run-shell "${pkgs.lazygit-window}/bin/lazygit-window"
      bind j display-popup -E ${pkgs.tmux-session}/bin/tmux-jump

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

      bind k switch-client -l

      set -g history-limit 20000 # default is 2000

      # let active-window.kak know when it's focused
      set -g focus-events on

      # status line (thanks, Ju!)
      set-option -g status-justify left
      set-option -g status-left '#[bg=colour2] #[bg=colour8] #[bg=colour0] #S '
      set-option -g status-left-length 16
      set-option -g status-fg colour7
      set-option -g status-bg colour0
      set-option -g status-right '#(${showBranch}/bin/showBranch) #(${shortStat}/bin/shortStat) %a %R #[bg=colour8] #[bg=colour2] #[]'
      set-option -g status-interval 15
      set-option -g pane-active-border-style fg=colour2
      set-option -g pane-border-style fg=colour238
      set-option -g status-right-length 70
      set-window-option -g window-status-format '#[bg=colour8]#[fg=colour3] #I #[fg=colour15]#W#[fg=colour5]#F# '
      set-window-option -g window-status-current-format '#[bg=colour8]#[fg=colour3] #I #[bg=colour7]#[fg=colour8] #W#[fg=colour0]#F #[bg=colour8]'

      # Smart pane switching with awareness of Vim splits.
      # See: https://github.com/christoomey/vim-tmux-navigator
      is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
          | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|l?n?vim?x?)(diff)?$'"
      bind-key -n 'C-h' if-shell "$is_vim" 'send-keys C-h'  'select-pane -L'
      bind-key -n 'C-j' if-shell "$is_vim" 'send-keys C-j'  'select-pane -D'
      bind-key -n 'C-k' if-shell "$is_vim" 'send-keys C-k'  'select-pane -U'
      bind-key -n 'C-l' if-shell "$is_vim" 'send-keys C-l'  'select-pane -R'
      tmux_version='$(tmux -V | sed -En "s/^tmux ([0-9]+(.[0-9]+)?).*/\1/p")'
      if-shell -b '[ "$(echo "$tmux_version < 3.0" | bc)" = 1 ]' \
          "bind-key -n 'C-\\' if-shell \"$is_vim\" 'send-keys C-\\'  'select-pane -l'"
      if-shell -b '[ "$(echo "$tmux_version >= 3.0" | bc)" = 1 ]' \
          "bind-key -n 'C-\\' if-shell \"$is_vim\" 'send-keys C-\\\\'  'select-pane -l'"

      bind-key -T copy-mode-vi 'C-h' select-pane -L
      bind-key -T copy-mode-vi 'C-j' select-pane -D
      bind-key -T copy-mode-vi 'C-k' select-pane -U
      bind-key -T copy-mode-vi 'C-l' select-pane -R
      bind-key -T copy-mode-vi 'C-\' select-pane -l

      bind-key C-l send-keys 'C-l'
      bind-key C-\\ send-keys 'C-\\'
    '';
  };
}
