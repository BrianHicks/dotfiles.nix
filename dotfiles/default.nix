{ pkgs, ... }: {
  home.packages = [
    pkgs.jless
    pkgs.meet
    pkgs.nix-tree
    pkgs.pstree
    pkgs.pv
  ];

  imports = [
    ./bat
    ./cachix
    ./comma
    ./direnv
    ./dog
    ./fd
    ./fzf
    ./git
    ./hammerspoon
    ./home-manager
    ./htop
    ./hyperfine
    ./jq
    ./lf
    ./ncdu
    ./neovim
    ./ripgrep
    ./sd
    ./ssh
    ./tmux
    ./tree
    ./tree-grepper
    ./watch
    ./wezterm
    ./zsh
  ];
}
