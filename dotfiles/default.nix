{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./chromium
    ./git
    ./tmux
    ./rofi
    ./xsession
    ./xmonad
    ./zoom
  ];
}
