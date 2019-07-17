{ pkgs, ... }:

let
  brianhicks-nur = import (builtins.fetchTarball "https://github.com/BrianHicks/nur-packages/archive/4d10cde0e0cc519c624b7fd0d43ee98d261056ff.tar.gz") { };
in
  {
    imports = [
      ./config/direnv.nix
      ./config/emacs.nix
      ./config/fzf.nix
      ./config/git.nix
      ./config/kakoune.nix
      ./config/neovim.nix
      ./config/ssh.nix
      ./config/tmux.nix
      ./config/zsh.nix
    ];

    home.packages = [
      pkgs.ag
      pkgs.awscli
      pkgs.jq
      pkgs.pv
      pkgs.tree
      pkgs.watch

      # language servers
      brianhicks-nur.elm-language-server

      # local packages. I know I could use overlays for these (cf
      # https://github.com/jwoudenberg/dotfiles/commit/12bd31b269b82f0dc661140b8df275ef24f41b81)
      # but I don't want to have to symlink into the overlays directory manually.
      (pkgs.callPackage ./pkgs/lorri.nix { })
    ];

    programs.man.enable = true;

    programs.home-manager = {
      enable = true;
      path = "$HOME/dotfiles.nix/home-manager";
    };
  }
