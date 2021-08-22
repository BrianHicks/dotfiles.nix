{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./chromium
    ./tmux
    ./xsession
    ./xmonad
    ./zoom
  ];
}
