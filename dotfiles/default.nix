{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./bat
    ./chromium
    ./fzf
    ./git
    ./keyboard
    ./rofi
    ./tmux
    ./xmonad
    ./xsession
    ./zoom
  ];
}
