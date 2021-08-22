{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./tmux
    ./xsession
    ./xmonad
    ./zoom
  ];
}
