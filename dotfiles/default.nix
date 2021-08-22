{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./tmux
    ./xsession
    ./zoom
  ];
}
