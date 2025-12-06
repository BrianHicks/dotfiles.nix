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
    ./fd
    ./fzf
    ./git
    ./home-manager
    ./htop
    ./hyperfine
    ./jq
    ./jujutsu
    ./lf
    # ./ncdu
    ./neovim
    ./nixconfig
    ./ripgrep
    ./sd
    ./ssh
    ./tmux
    ./tree
    ./watch
    ./wezterm
    ./zsh
  ];
}
