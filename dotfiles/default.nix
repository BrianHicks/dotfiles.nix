{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./chromium
    ./tmux
    ./rofi
    ./xsession
    ./xmonad
    ./zoom
  ];
}
