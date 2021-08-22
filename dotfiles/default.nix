{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./bat
    ./chromium
    ./direnv
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
