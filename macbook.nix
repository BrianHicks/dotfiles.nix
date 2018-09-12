{ pkgs, ... }:

{
  imports = [
    ./config/direnv.nix
    ./config/emacs.nix
    ./config/fzf.nix
    ./config/git.nix
    ./config/ssh.nix
    ./config/zsh.nix
  ];

  home.packages = [
    pkgs.ag
    pkgs.jq
    pkgs.tree
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
