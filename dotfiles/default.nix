{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./chromium
    ./tmux
    ./rofi
    ./xsession
    ./xmonad
    ./zoom
  ];
}
