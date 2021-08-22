{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./bat
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
