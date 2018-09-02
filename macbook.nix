{ pkgs, ... }:

{
  imports = [
    #./config/emacs.nix
    ./config/direnv.nix
    ./config/fzf.nix
    ./config/git.nix
    ./config/ssh.nix
    ./config/zsh.nix
  ];

  programs.man.enable = true;

  programs.home-manager = {
    enable = true;
    path = "$HOME/dotfiles.nix/home-manager";
  };
}
