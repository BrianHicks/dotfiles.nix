{ pkgs, ... }:

{
  home.packages = [
    pkgs.tmux
  ];

  home.file.".tmux.conf" = {
    text = ''
      # 256 colors
      set -g default-terminal "screen-256color"

      # I don't much like c-b; maybe this should be an esc-sequence though?
      unbind C-b
      set -g prefix C-a
      bind C-a send-prefix

      # set escape time to 0 so that hitting escape isn't interpeted as an escape code
      # might not be a good idea for Kakoune? We'll see.
      set -g escape-time 0 # might need -s?

      # mouse on
      set -g mouse on

      # window management
      set -g base-index 1
      setw -g pane-base-index 1
      set -g renumber-windows on

      # status line key bindings
      set-option -g status-keys emacs

      # use vim keybindings in copy mode
      setw -g mode-keys vi

      # TODO: look in Ju's config for copy https://github.com/Arkham/dotfiles/blob/master/tmux.conf
      # TODO: look in Ju's config for handy keybindings for vim -> kak
      # TODO: look in Juan's config for more fun stuff https://github.com/juanedi/dotfiles/blob/master/.tmux.conf

      # preserve path on new windows and panes
      bind-key c new-window -c '#{pane_current_path}'
      bind-key \ split-window -h -c '#{pane_current_path}'
      bind-key - split-window -v -c '#{pane_current_path}'
    '';
  };
}
