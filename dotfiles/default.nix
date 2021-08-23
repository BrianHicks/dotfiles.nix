{ ... }: {
  programs.home-manager.enable = true;

  imports = [
    ./alacritty
    ./bat
    ./chromium
    ./direnv
    ./fzf
    ./git
    ./home-manager
    ./keyboard
    ./rofi
    ./ssh
    ./tmux
    ./xmonad
    ./xsession
    ./zoom
    ./zsh
  ];
}
