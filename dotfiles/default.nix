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
    ./bun
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
    ./jujutsu
    ./lf
    ./ncdu
    ./neovim
    ./nixconfig
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
