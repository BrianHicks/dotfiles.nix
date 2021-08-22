{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./chromium
    ./git
    ./keyboard
    ./rofi
    ./tmux
    ./xmonad
    ./xsession
    ./zoom
  ];
}
