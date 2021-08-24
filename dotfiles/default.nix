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
    ./hyperfine
    ./keyboard
    ./rofi
    ./ripgrep
    ./ssh
    ./tmux
    ./xmonad
    ./xsession
    ./zoom
    ./zsh
  ];
}
